(*
  JDOSQL implementation include
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

constructor TJDOCustomSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrdered := True;
end;

constructor TJDOCustomSQL.Create(AOwner: TComponent;
  AQuery: TSQLQuery; const ATableName: string);
begin
  inherited Create(AOwner);
  CheckQuery;
  FQuery := AQuery;
  FTableName := ATableName;
end;

destructor TJDOCustomSQL.Destroy;
begin
  Query := nil;
  inherited Destroy;
end;

procedure TJDOCustomSQL.CheckTableName;
begin
  if FTableName = ES then
    raise EJDOSQL.Create(Self, SEmptyTableNameError);
end;

procedure TJDOCustomSQL.CheckQuery;
begin
  if not Assigned(FQuery) then
    raise EJDOSQL.Create(Self, SNilQueryError);
end;

procedure TJDOCustomSQL.InternalCheckFieldDefs(const ACount: Integer);
begin
  if ACount = 0 then
    raise EJDOSQL.Create(Self, SEmptyFieldDefsCountError);
end;

procedure TJDOCustomSQL.InternalCheckServerIndexDefs(const ACount: Integer);
begin
  if ACount = 0 then
    raise EJDOSQL.Create(Self, SEmptyServerIndexDefsCountError);
end;

procedure TJDOCustomSQL.PrepareCols(const AStatementType: TJDOStatementType;
  out ACols, AValues: string);
var
  I, C: Integer;
  VValue: string;
  VValues: TStrings;
  VIndexDef: TIndexDef;
  VFieldName: ShortString;
begin
  CheckQuery;
  ACols := ES;
  AValues := ES;
  C := FQuery.FieldDefs.Count;
  case AStatementType of
    stSelect:
      if C = 0 then
        if FTableAlias <> ES then
          ACols := FTableAlias + DT + AK
        else
          ACols := AK
      else
        for I := 0 to Pred(C) do
        begin
          if I > 0 then
            ACols += CS;
          ACols += FQuery.FieldDefs[I].Name;
        end;
    stInsert:
      for I := 0 to Pred(C) do
      begin
        if I > 0 then
        begin
          ACols += CS;
          AValues += CS;
        end;
        VFieldName := FQuery.FieldDefs[I].Name;
        ACols += VFieldName;
        AValues += CO + VFieldName;
      end;
    stUpdate, stDelete:
      begin
        InternalCheckFieldDefs(C);
        C := FQuery.ServerIndexDefs.Count;
        InternalCheckServerIndexDefs(C);
        VValues := TStringList.Create;
        try
          VValues.Delimiter := SC;
          for I := 0 to Pred(C) do
          begin
            VIndexDef := FQuery.ServerIndexDefs[I];
            if ixPrimary in VIndexDef.Options then
            begin
              VValues.DelimitedText := VIndexDef.Fields;
              Break;
            end;
            raise EJDOSQL.Create(Self, SPrimaryKeyNotFound);
          end;
          for I := 0 to Pred(FQuery.FieldDefs.Count) do
          begin
            VFieldName := FQuery.FieldDefs[I].Name;
            if VValues.IndexOf(VFieldName) = -1 then
              ACols += VFieldName + SQL_EQ_PARAM_TOKEN + VFieldName + CS;
          end;
          SetLength(ACols, Length(ACols) - 1);
          for I := 0 to Pred(VValues.Count) do
          begin
            VValue := VValues.ValueFromIndex[I];
            AValues += VValue + SQL_EQ_PARAM_TOKEN + VValue + SQL_AND_TOKEN;
          end;
          SetLength(AValues, Length(AValues) - Length(SQL_AND_TOKEN));
        finally
          VValues.Free;
        end;
      end;
  end;
end;

procedure TJDOCustomSQL.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FQuery) then
    FQuery := nil;
end;

function TJDOCustomSQL.Compose(const AStatementType: TJDOStatementType): Boolean;
var
  VIsWhere: Boolean;
  VCols, VValues: string;
begin
  CheckQuery;
  CheckTableName;
  Result := AStatementType <> FStatementType;
  if Result then
  begin
    case AStatementType of
      stSelect:
        begin
          FQuery.SQL.Clear;
          if FPutBegin <> ES then
            VCols := FPutBegin
          else
            PrepareCols(stSelect, VCols, VValues);
          FQuery.SQL.Add(SQL_SELECT_TOKEN + VCols + SP + SQL_FROM_TOKEN +
            FTableName + SP + FTableAlias);
          if FPutMiddle <> ES then
            FQuery.SQL.Add(FPutMiddle)
          else
          begin
            VIsWhere := FWhere <> ES;
            if VIsWhere then
              FQuery.SQL.Add(SQL_WHERE_TOKEN + FWhere);
            if FLike <> ES then
              if VIsWhere then
                FQuery.SQL.Add(SQL_AND_TOKEN + FLike)
              else
                FQuery.SQL.Add(SQL_WHERE_TOKEN + FLike);
          end;
          if FPutEnd <> ES then
            FQuery.SQL.Add(FPutEnd)
          else
            if FOrdered and (FOrderBy <> ES) then
              if FTableAlias <> ES then
                FQuery.SQL.Add(SQL_ORDER_BY_TOKEN + FTableAlias + DT + FOrderBy)
              else
                FQuery.SQL.Add(SQL_ORDER_BY_TOKEN + FOrderBy);
        end;
      stInsert:
        begin
          PrepareCols(stInsert, VCols, VValues);
          FQuery.InsertSQL.Text := SQL_INSERT_TOKEN + FTableName + SP + PS +
            VCols + PE + SQL_VALUES_TOKEN + PS + VValues + PE;
        end;
      stUpdate:
        begin
          PrepareCols(stUpdate, VCols, VValues);
          FQuery.UpdateSQL.Text := SQL_UPDATE_TOKEN + FTableName +
            SQL_SET_TOKEN + VCols + SQL_WHERE_TOKEN + VValues;
        end;
      stDelete:
        begin
          PrepareCols(stDelete, VCols, VValues);
          FQuery.DeleteSQL.Text := SQL_DELETE_TOKEN + SQL_FROM_TOKEN +
            FTableName + SQL_WHERE_TOKEN + VValues;
        end;
    end;
    FStatementType := AStatementType;
  end;
  if Assigned(FOnCompose) then
    FOnCompose(AStatementType);
end;

procedure TJDOCustomSQL.Reset;
begin
  FPutBegin := ES;
  FPutMiddle := ES;
  FPutEnd := ES;
  if FStatementType <> stUnknown then
    FStatementType := stUnknown;
end;

procedure TJDOCustomSQL.Clear;
begin
  CheckQuery;
  Reset;
  FQuery.SQL.Clear;
  FQuery.InsertSQL.Clear;
  FQuery.UpdateSQL.Clear;
  FQuery.DeleteSQL.Clear;
end;

procedure TJDOCustomSQL.Put(const ASQL: string; const AType: TJDOPutTypes);
begin
  case AType of
    ptBegin: FPutBegin += ASQL + SP;
    ptMiddle: FPutMiddle += ASQL + SP;
    ptEnd: FPutEnd += ASQL + SP;
  end;
end;

procedure TJDOCustomSQL.Like(const AKey: string; const ACaseInsensitive: Boolean);
begin
  if ACaseInsensitive then
    FLike := SQL_LOWER_TOKEN + PS + AKey + PE + SQL_LIKE_TOKEN + AKey + PE
  else
    FLike := AKey + SQL_LIKE_TOKEN + AKey + PE;
end;
