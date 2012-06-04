(*
  JDOTable implementation include
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

constructor TJDOTable.Create(ADataBase: TJDODataBase; const ATableName: string);
begin
  inherited Create;
  FQuery := TJDOQuery.Create(nil);
  FColumns := FQuery.FieldDefs;
  FSQL := TJDOSQL.Create(FQuery.SQL, FColumns, ATableName);
  SetDataBase(ADataBase);
  FAutoSetup := True;
  FPersisted := True;
end;

destructor TJDOTable.Destroy;
begin
  FSQL.Free;
  FQuery.Free;
  inherited Destroy;
end;

procedure TJDOTable.Apply;
begin
  if not FDataBase.Transaction.Active then
    FDataBase.Transaction.StartTransaction;
  try
    FDataBase.Transaction.Commit;
  except
    FDataBase.Transaction.Rollback;
    raise;
  end;
end;

procedure TJDOTable.Setup;
var
  I, C: Integer;
  VCols: string = ES;
  VItem: TJSONObject;
begin
  if Count = 0 then
    raise EJDOTable.Create(Self, SEmptyTableError);
  VItem := Objects[0];
  C := VItem.Count;
  if C = 0 then
    raise EJDOTable.Create(Self, SEmptyObjectError);
  for I := 0 to Pred(C) do
  begin
    VCols += VItem.Names[I];
    if Succ(I) < C then
      VCols += CS;
  end;
  FQuery.SQL.Text := SQL_SELECT_TOKEN + VCols + SQL_FROM_TOKEN +
    FSQL.TableName + SQL_NOTHING_WHERE_TOKEN;
  FQuery.Open;
  FQuery.Close;
  FQuery.SQL.Clear;
end;

procedure TJDOTable.Reset;
begin
  FSQL.Reset;
end;

procedure TJDOTable.SetDataBase(const AValue: TJDODataBase);
begin
  FDataBase := AValue;
  FQuery.DataBase := AValue.Connection;
end;

function TJDOTable.GetDataBase: TJDODataBase;
begin
  Result := FDataBase;
end;

procedure TJDOTable.SetDateAsString(const AValue: Boolean);
begin
  FQuery.DateAsString := AValue;
end;

procedure TJDOTable.SetMode(const AValue: TJDOModeTypes);
begin
  if FMode <> AValue then
  begin
    FMode := AValue;
    FSQL.Reset;
  end;
end;

procedure TJDOTable.SetName(const AValue: string);
begin
  FSQL.TableName := AValue;
  FSQL.Reset;
end;

procedure TJDOTable.SetKey;
var
  I: Integer;
  VIndexDef: TIndexDef;
begin
  if FSQL.Key = ES then
    for I := 0 to Pred(FQuery.ServerIndexDefs.Count) do
    begin
      VIndexDef := FQuery.ServerIndexDefs[I];
      if ixPrimary in VIndexDef.Options then
        FSQL.Key := VIndexDef.Fields;
    end;
end;

function TJDOTable.GetDateAsString: Boolean;
begin
  Result := FQuery.DateAsString;
end;

function TJDOTable.GetName: string;
begin
  Result := FSQL.TableName;
end;

procedure TJDOTable.Post;
begin
  if FAutoSetup and (FColumns.Count = 0) then
    Setup;
  if not FSQL.IsComposed then
    case FMode of
      mtInsert: FSQL.Compose(stInsert);
      mtUpdate:
        begin
          SetKey;
          FSQL.Compose(stUpdate);
        end;
      mtDelete:
        begin
          SetKey;
          FSQL.Compose(stDelete);
        end;
    end;
  if Count > 0 then
    FQuery.FromJSONArray(Self)
  else
    FQuery.ExecSQL;
  if FPersisted then
    Apply;
end;

function TJDOTable.Open: Boolean;
var
  P: Integer;
begin
  if not FSQL.IsComposed then
    FSQL.Compose(stSelect);
  if Count > 0 then
    FQuery.FromJSONArray(Self);
  Result := FQuery.Open;
  if FQuery.RecordCount > 0 then
    try
      FQuery.DisableControls;
      P := FQuery.RecNo;
      FQuery.First;
      Clear;
      while not FQuery.EOF do
      begin
        Add(FQuery.CreateJSONObject);
        FQuery.Next;
      end;
    finally
      FQuery.RecNo := P;
      FQuery.EnableControls;
    end;
  if FPersisted then
    Apply;
end;

procedure TJDOTable.Like(const AValue: string; const AKey: ShortString;
  const AOptions: TJDOLikeOptions);
begin
  FSQL.Like(AKey, loCaseInsensitive in AOptions);
  FSQL.Compose(stSelect);
  if loPartialKey in AOptions then
    FQuery.Params.ParamByName(AKey).AsString := AnsiQuotedStr(AValue, PT)
  else
    FQuery.Params.ParamByName(AKey).AsString := AValue;
end;

procedure TJDOTable.First;
begin
  FQuery.First;
end;

procedure TJDOTable.Last;
begin
  FQuery.Last;
end;

procedure TJDOTable.Next;
begin
  FQuery.Next;
end;

procedure TJDOTable.Prior;
begin
  FQuery.Prior;
end;

function TJDOTable.Get: TJSONObject;
begin
  Result := Items[Pred(FQuery.RecNo)] as TJSONObject;
end;

function TJDOTable.Find(const AName: string; const AValue: string): TJSONObject;
var
  I: Integer;
  VItem: TJSONObject;
begin
  for I := 0 to Pred(Count) do
  begin
    VItem := Items[I] as TJSONObject;
    if SameText(VItem[AName].AsString, AValue) then
    begin
      Result := VItem;
      Break;
    end;
  end;
end;

function TJDOTable.Find(const AName: string; const AValue: Boolean): TJSONObject;
var
  I: Integer;
  VItem: TJSONObject;
begin
  for I := 0 to Pred(Count) do
  begin
    VItem := Items[I] as TJSONObject;
    if VItem[AName].AsBoolean = AValue then
    begin
      Result := VItem;
      Break;
    end;
  end;
end;

function TJDOTable.Find(const AName: string; const AValue: Integer): TJSONObject;
var
  I: Integer;
  VItem: TJSONObject;
begin
  for I := 0 to Pred(Count) do
  begin
    VItem := Items[I] as TJSONObject;
    if VItem[AName].AsInteger = AValue then
    begin
      Result := VItem;
      Break;
    end;
  end;
end;

function TJDOTable.Find(const AName: string; const AValue: Double): TJSONObject;
var
  I: Integer;
  VItem: TJSONObject;
begin
  for I := 0 to Pred(Count) do
  begin
    VItem := Items[I] as TJSONObject;
    if VItem[AName].AsFloat = AValue then
    begin
      Result := VItem;
      Break;
    end;
  end;
end;

function TJDOTable.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AName);
end;

function TJDOTable.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AName);
end;