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
  if Trim(FTableName) = ES then
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

  procedure GetValues(out S: TStrings; out IDef: TIndexDef;
    const C: Integer); inline;
  var
    I: Integer;
  begin
    S := TStringList.Create;
    S.Delimiter := SC;
    for I := 0 to Pred(C) do
    begin
      IDef := FQuery.ServerIndexDefs[I];
      if ixPrimary in IDef.Options then
      begin
        S.DelimitedText := IDef.Fields;
        Break;
      end;
      raise EJDOSQL.Create(Self, SPrimaryKeyNotFound);
    end;
  end;

var
  VValue: string;
  I, C, L: Integer;
  VValues: TStrings;
  VIndexDef: TIndexDef;
  VFieldName: ShortString;
begin
  CheckQuery;
  ACols := ES;
  AValues := ES;
  C := FQuery.FieldDefs.Count;
  if AStatementType = jstSelect then
  begin
    if C = 0 then
      if FTableAlias <> ES then
        ACols := FTableAlias + DT + AK
      else
        ACols := AK
    else
      for I := 0 to Pred(C) do
      begin
        if I > 0 then
          ACols += CS + SP;
        VFieldName := FQuery.FieldDefs[I].Name;
        if FTableAlias <> ES then
          ACols += FTableAlias + DT + VFieldName
        else
          ACols += VFieldName;
      end;
  end
  else
  begin
    InternalCheckFieldDefs(C);
    C := FQuery.ServerIndexDefs.Count;
    L := Length(CS + SP);
    case AStatementType of
      jstInsert:
        if C > 0 then
        begin
          GetValues(VValues, VIndexDef, C);
          try
            for I := 0 to Pred(FQuery.FieldDefs.Count) do
            begin
              VFieldName := FQuery.FieldDefs[I].Name;
              if VValues.IndexOf(VFieldName) = -1 then
              begin
                ACols += VFieldName + CS + SP;
                AValues +=  CO + VFieldName + CS + SP;
              end;
            end;
            SetLength(AValues, Length(AValues) - L);
          finally
            VValues.Free;
          end;
        end
        else
          for I := 0 to Pred(C) do
          begin
            if I > 0 then
            begin
              ACols += CS + SP;
              AValues += CS + SP;
            end;
            VFieldName := FQuery.FieldDefs[I].Name;
            ACols += VFieldName;
            AValues += CO + VFieldName;
          end;
      jstUpdate, jstDelete:
      begin
        InternalCheckServerIndexDefs(C);
        GetValues(VValues, VIndexDef, C);
        try
          for I := 0 to Pred(FQuery.FieldDefs.Count) do
          begin
            VFieldName := FQuery.FieldDefs[I].Name;
            if VValues.IndexOf(VFieldName) = -1 then
              ACols += VFieldName + SQL_EQ_PARAM_TOKEN + VFieldName + CS + SP;
          end;
          for I := 0 to Pred(VValues.Count) do
          begin
            VValue := VValues.ValueFromIndex[I];
            AValues += VValue + SQL_EQ_PARAM_TOKEN + VValue + SP +
              SQL_AND_TOKEN + SP;
          end;
          SetLength(AValues, Length(AValues) - Length(SP + SQL_AND_TOKEN + SP));
        finally
          VValues.Free;
        end;
      end;
    end;
    SetLength(ACols, Length(ACols) - L);
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
      jstSelect:
        begin
          FQuery.SQL.Clear;
          if FPutBegin <> ES then
            VCols := FPutBegin
          else
            PrepareCols(jstSelect, VCols, VValues);
          FQuery.SQL.Add(SQL_SELECT_TOKEN + SP + VCols + SP + SQL_FROM_TOKEN +
            SP + FTableName + SP + FTableAlias);
          if FPutMiddle <> ES then
            FQuery.SQL.Add(FPutMiddle)
          else
          begin
            VIsWhere := FWhere <> ES;
            if VIsWhere then
              FQuery.SQL.Add(SQL_WHERE_TOKEN + SP + FWhere);
            if FLike <> ES then
              if VIsWhere then
                FQuery.SQL.Add(SQL_AND_TOKEN + SP + FLike)
              else
                FQuery.SQL.Add(SQL_WHERE_TOKEN + SP + FLike);
          end;
          if FPutEnd <> ES then
            FQuery.SQL.Add(FPutEnd)
          else
            if FOrdered and (FOrderBy <> ES) then
              if FTableAlias <> ES then
                FQuery.SQL.Add(SQL_ORDER_BY_TOKEN + SP + FTableAlias +
                  DT + FOrderBy)
              else
                FQuery.SQL.Add(SQL_ORDER_BY_TOKEN + SP + FOrderBy);
        end;
      jstInsert:
        begin
          PrepareCols(jstInsert, VCols, VValues);
          FQuery.InsertSQL.Text := SQL_INSERT_TOKEN + SP + FTableName + SP +
            PS + VCols + PE + SP + SQL_VALUES_TOKEN + SP + PS + VValues + PE;
        end;
      jstUpdate:
        begin
          PrepareCols(jstUpdate, VCols, VValues);
          FQuery.UpdateSQL.Text := SQL_UPDATE_TOKEN + SP + FTableName + SP +
            SQL_SET_TOKEN + SP + VCols + SP + SQL_WHERE_TOKEN + SP + VValues;
        end;
      jstDelete:
        begin
          PrepareCols(jstDelete, VCols, VValues);
          FQuery.DeleteSQL.Text := SQL_DELETE_TOKEN + SP + SQL_FROM_TOKEN + SP +
            FTableName + SP + SQL_WHERE_TOKEN + SP + VValues;
        end;
    end;
    FStatementType := AStatementType;
  end;
  if Assigned(FOnCompose) then
    FOnCompose(AStatementType);
end;

procedure TJDOCustomSQL.ComposeAll;
begin
  FStatementType := jstUnknown;
  Compose(jstSelect);
  Compose(jstInsert);
  Compose(jstUpdate);
  Compose(jstDelete);
end;

procedure TJDOCustomSQL.Reset;
begin
  FPutBegin := ES;
  FPutMiddle := ES;
  FPutEnd := ES;
  if FStatementType <> jstUnknown then
    FStatementType := jstUnknown;
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

procedure TJDOCustomSQL.Put(const ASQL: string; const AType: TJDOPutTypes;
  const ALineBreak: ShortString);
begin
  case AType of
    ptBegin:
      begin
        if FPutBegin <> ES then
          FPutBegin += ALineBreak;
        FPutBegin += ASQL;
      end;
    ptMiddle:
      begin
        if FPutMiddle <> ES then
          FPutMiddle += ALineBreak;
        FPutMiddle += ASQL;
      end;
    ptEnd:
      begin
        if FPutEnd <> ES then
          FPutEnd += ALineBreak;
        FPutEnd += ASQL;
      end;
  end;
end;

procedure TJDOCustomSQL.Like(const AKey: string; const ACaseInsensitive: Boolean);
begin
  if ACaseInsensitive then
    FLike := SQL_LOWER_TOKEN + PS + AKey + PE
  else
    FLike := AKey;
  FLike += SP + SQL_LIKE_TOKEN + SP + PS + CO + AKey + PE;
end;
