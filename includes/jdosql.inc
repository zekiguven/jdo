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

constructor TJDOSQL.Create(AOwner: TComponent; ASQL: TStrings;
  AFieldDefs: TFieldDefs; const ATableName: string);
begin
  inherited Create(AOwner);
  if not Assigned(ASQL) then
    raise EJDOSQL.Create(Self, '"ASQL" must be not nil.');
  if not Assigned(AFieldDefs) then
    raise EJDOSQL.Create(Self, '"AFieldDefs" must be not nil.');
  FFieldDefs := AFieldDefs;
  FScript := ASQL;
  FOrdered := True;
  FTableName := ATableName;
end;

constructor TJDOSQL.Create(ASQL: TStrings; AFieldDefs: TFieldDefs;
  const ATableName: string);
begin
  Create(nil, ASQL, AFieldDefs, ATableName);
end;

function TJDOSQL.IsComposed: Boolean;
begin
  Result := (FStatementType <> stUnknown) or (FScript.Count > 0);
end;

procedure TJDOSQL.CheckTableName;
begin
  if FTableName = ES then
    raise EJDOSQL.Create(Self, SEmptyTableNameError);
end;

procedure TJDOSQL.CheckKey;
begin
  if Trim(Key) = ES then
    raise EJDOSQL.Create(Self, SEmptyKeyError);
end;

function TJDOSQL.FormatCols(const AToken: string;
  const ASkipKey, APairs: Boolean): string;
var
  VFieldName: string;
  I, C: Integer;
begin
  Result := ES;
  C := FFieldDefs.Count;
  if C = 0 then
  begin
    if FTableAlias <> ES then
      Result := FTableAlias + DT + AK
    else
      Result := AK
  end
  else
    for I := 0 to Pred(C) do
    begin
      VFieldName := FFieldDefs[I].Name;
      if ASkipKey and (CompareText(VFieldName, FKey) = 0) then
        Continue;
      if APairs then
        Result += VFieldName;
      if (FTableAlias <> ES) and (AToken = ES) and (not APairs) then
        Result += FTableAlias + DT + VFieldName
      else
        Result += AToken + VFieldName;
      if Succ(I) < C then
        Result += CS;
    end;
end;

procedure TJDOSQL.Compose(const AStatementType: TJDOStatementType);
var
  VCols: string;
  VIsWhere: Boolean;
begin
  if AStatementType <> FStatementType then
    FScript.Clear
  else
    Exit;
  case AStatementType of
    stSelect:
      begin
        CheckTableName;
        if FPutBegin <> ES then
          VCols := FPutBegin
        else
          VCols := FormatCols(ES, False, False);
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
        CheckTableName;
        FScript.Add(SQL_INSERT_TOKEN + FTableName +
          SP + PS + FormatCols(ES, False, False) + PE +
          SQL_VALUES_TOKEN + PS + FormatCols(CO, False, False) + PE);
      end;
    stUpdate:
      begin
        CheckTableName;
        CheckKey;
        FScript.Add(SQL_UPDATE_TOKEN + FTableName + SQL_SET_TOKEN +
          FormatCols(SQL_EQ_PARAM_TOKEN, True, True) + SQL_WHERE_TOKEN + FKey +
          SQL_EQ_PARAM_TOKEN + FKey);
      end;
    stDelete:
      begin
        CheckTableName;
        CheckKey;
        FScript.Add(SQL_DELETE_TOKEN + SQL_FROM_TOKEN + FTableName +
          SQL_WHERE_TOKEN + FKey + SQL_EQ_PARAM_TOKEN + FKey);
      end;
  end;
  FStatementType := AStatementType;
end;

procedure TJDOSQL.Reset;
begin
  FPutBegin := ES;
  FPutMiddle := ES;
  FPutEnd := ES;
  if FStatementType <> stUnknown then
    FStatementType := stUnknown;
end;

procedure TJDOSQL.Clear;
begin
  Reset;
  FScript.Clear;
end;

procedure TJDOSQL.Put(const ASQL: string; const AType: TJDOPutTypes);
begin
  case AType of
    ptBegin: FPutBegin += ASQL + SP;
    ptMiddle: FPutMiddle += ASQL + SP;
    ptEnd: FPutEnd += ASQL + SP;
  end;
end;

procedure TJDOSQL.Like(const AKey: string; const ACaseInsensitive: Boolean);
begin
  if ACaseInsensitive then
    FLike := SQL_LOWER_TOKEN + PS + AKey + PE + SQL_LIKE_TOKEN + AKey + PE
  else
    FLike := AKey + SQL_LIKE_TOKEN + AKey + PE;
end;
