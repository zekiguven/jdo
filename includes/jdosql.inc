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

constructor TJDOCustomSQL.Create(AOwner: TComponent; ASQL: TStrings;
  AFieldDefs: TFieldDefs; AIndexDefs: TServerIndexDefs;
  const ATableName: string);
begin
  inherited Create(AOwner);
  if not Assigned(ASQL) then
    raise EJDOSQL.Create(Self, '"ASQL" must be not nil.');
  if not Assigned(AIndexDefs) then
    raise EJDOSQL.Create(Self, '"AIndexDefs" must be not nil.');
  if not Assigned(AFieldDefs) then
    raise EJDOSQL.Create(Self, '"AFieldDefs" must be not nil.');
  FFieldDefs := AFieldDefs;
  FIndexDefs := AIndexDefs;
  FScript := ASQL;
  FOrdered := True;
  FTableName := ATableName;
end;

constructor TJDOCustomSQL.Create(ASQL: TStrings; AFieldDefs: TFieldDefs;
  AIndexDefs: TServerIndexDefs; const ATableName: string);
begin
  Create(nil, ASQL, AFieldDefs, AIndexDefs, ATableName);
end;

function TJDOCustomSQL.IsComposed: Boolean;
begin
  Result := (FStatementType <> stUnknown) or (FScript.Count > 0);
end;

procedure TJDOCustomSQL.CheckTableName;
begin
  if FTableName = ES then
    raise EJDOSQL.Create(Self, SEmptyTableNameError);
end;

procedure TJDOCustomSQL.InternalCheckFieldDefs(const ACount: Integer);
begin
  if ACount = 0 then
    raise EJDOSQL.Create(Self, SEmptyFieldDefsCountError);
end;

procedure TJDOCustomSQL.InternalCheckIndexDefs(const ACount: Integer);
begin
  if ACount = 0 then
    raise EJDOSQL.Create(Self, SEmptyIndexDefsCountError);
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
  ACols := ES;
  AValues := ES;
  C := FFieldDefs.Count;
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
          ACols += FFieldDefs[I].Name;
        end;
    stInsert:
      for I := 0 to Pred(C) do
      begin
        if I > 0 then
        begin
          ACols += CS;
          AValues += CS;
        end;
        VFieldName := FFieldDefs[I].Name;
        ACols += VFieldName;
        AValues += CO + VFieldName;
      end;
    stUpdate, stDelete:
      begin
        InternalCheckFieldDefs(C);
        C := FIndexDefs.Count;
        InternalCheckIndexDefs(C);
        VValues := TStringList.Create;
        try
          VValues.Delimiter := SC;
          for I := 0 to Pred(C) do
          begin
            VIndexDef := FIndexDefs[I];
            if ixPrimary in VIndexDef.Options then
            begin
              VValues.DelimitedText := VIndexDef.Fields;
              Break;
            end;
            raise EJDOSQL.Create(Self, SPrimaryKeyNotFound);
          end;
          for I := 0 to Pred(FFieldDefs.Count) do
          begin
            VFieldName := FFieldDefs[I].Name;
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

procedure TJDOCustomSQL.Compose(const AStatementType: TJDOStatementType);
var
  VIsWhere: Boolean;
  VCols, VValues: string;
begin
  if AStatementType <> FStatementType then
  begin
    CheckTableName;
    FScript.Clear;
    case AStatementType of
      stSelect:
        begin
          if FPutBegin <> ES then
            VCols := FPutBegin
          else
            PrepareCols(stSelect, VCols, VValues);
          FScript.Add(SQL_SELECT_TOKEN + VCols + SP + SQL_FROM_TOKEN +
            FTableName + SP + FTableAlias);
          if FPutMiddle <> ES then
            FScript.Add(FPutMiddle)
          else
          begin
            VIsWhere := FWhere <> ES;
            if VIsWhere then
              FScript.Add(SQL_WHERE_TOKEN + FWhere);
            if FLike <> ES then
              if VIsWhere then
                FScript.Add(SQL_AND_TOKEN + FLike)
              else
                FScript.Add(SQL_WHERE_TOKEN + FLike);
          end;
          if FPutEnd <> ES then
            FScript.Add(FPutEnd)
          else
            if FOrdered and (FOrderBy <> ES) then
              if FTableAlias <> ES then
                FScript.Add(SQL_ORDER_BY_TOKEN + FTableAlias + DT + FOrderBy)
              else
                FScript.Add(SQL_ORDER_BY_TOKEN + FOrderBy);
        end;
      stInsert:
        begin
          PrepareCols(stInsert, VCols, VValues);
          FScript.Add(SQL_INSERT_TOKEN + FTableName + SP + PS + VCols + PE +
            SQL_VALUES_TOKEN + PS + VValues + PE);
        end;
      stUpdate:
        begin
          PrepareCols(stUpdate, VCols, VValues);
          FScript.Add(SQL_UPDATE_TOKEN + FTableName + SQL_SET_TOKEN + VCols +
            SQL_WHERE_TOKEN + VValues);
        end;
      stDelete:
        begin
          PrepareCols(stDelete, VCols, VValues);
          FScript.Add(SQL_DELETE_TOKEN + SQL_FROM_TOKEN + FTableName +
            SQL_WHERE_TOKEN + VValues);
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
  Reset;
  FScript.Clear;
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
