(*
  JDOQuery implementation include
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

constructor TJDOCustomQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TSQLConnection then
  begin
    SetDatabase(AOwner as TSQLConnection);
    Transaction := TSQLConnection(AOwner).Transaction;
  end;
  if AOwner is TJDOCustomDataBase then
  begin
    SetDatabase((AOwner as TJDOCustomDataBase).Connection);
    Transaction := TJDOCustomDataBase(AOwner).Transaction;
  end;
end;

procedure TJDOCustomQuery.ReadJSONObject(AJSON: TJSONObject);
var
  I: Integer;
  VName: string;
  VParam: TParam;
  VData: TJSONData;
  VFieldDef: TFieldDef;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VParam := Params.FindParam(VName);
    if not Assigned(VParam) then
      Continue;
    VData := AJSON.Items[I];
    if VData.IsNull then
      VParam.Clear
    else
    begin
      VFieldDef := FieldDefs.Find(VName);
      case VFieldDef.DataType of
        ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
          ftInterface, ftIDispatch: VParam.Clear;
        ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
          ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
          ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
          ftVariant,ftGuid: VParam.AsString := VData.AsString;
        ftSmallint, ftInteger, ftLargeint, ftWord,
          ftAutoInc: VParam.AsInteger := VData.AsInteger;
        ftBoolean: VParam.AsBoolean := VData.AsBoolean;
        ftFloat, ftCurrency, ftBCD, ftFMTBcd: VParam.AsFloat := VData.AsFloat;
        ftDate, ftTime, ftDateTime, ftTimeStamp:
          if FDateAsString then
            VParam.AsDateTime := StrToDateTime(VData.AsString)
          else
            VParam.AsDateTime := VData.AsFloat;
      end;
    end;
  end;
end;

function TJDOCustomQuery.CreateJSONObject: TJSONObject;
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  Result := TJSONObject.Create;
  for I := 0 to Pred(Fields.Count) do
  begin
    VField := Fields[I];
    VFieldType := GetJSONFieldType(VField.DataType);
    VFieldName := VField.FieldName;
    if (VFieldType = FT_NULL) or VField.IsNull then
    begin
      Result.Add(VFieldName);
      Continue;
    end;
    if VFieldType = FT_STRING then
      Result.Add(VFieldName, VField.AsString);
    if VFieldType = FT_BOOLEAN then
      Result.Add(VFieldName, VField.AsBoolean);
    if VFieldType = FT_DATE then
      if FDateAsString then
        Result.Add(VFieldName, VField.AsString)
      else
        Result.Add(VFieldName, VField.AsFloat);
    if VFieldType = FT_FLOAT then
      Result.Add(VFieldName, VField.AsFloat);
    if VFieldType = FT_INT then
      Result.Add(VFieldName, VField.AsInteger);
  end;
end;

class function TJDOCustomQuery.GetJSONFieldType(
  const AFieldType: TFieldType): ShortString;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
      ftInterface, ftIDispatch: Result := FT_NULL;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
      ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
      ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
      ftVariant, ftGuid: Result := FT_STRING;
    ftSmallint, ftInteger, ftLargeint, ftWord,
      ftAutoInc: Result := FT_INT;
    ftBoolean: Result := FT_BOOLEAN;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := FT_FLOAT;
    ftDate, ftTime, ftDateTime, ftTimeStamp: Result := FT_DATE;
  end;
end;

function TJDOCustomQuery.ToJSONArray: TJSONArray;
var
  P: Integer;
begin
  Result := TJSONArray.Create;
  Open;
  if RecordCount = 0 then
    Exit;
  try
    DisableControls;
    P := RecNo;
    First;
    while not EOF do
    begin
      Result.Add(CreateJSONObject);
      Next;
    end;
  finally
    RecNo := P;
    EnableControls;
  end;
end;

function TJDOCustomQuery.ToJSONObject: TJSONObject;
begin
  Open;
  if RecordCount <> 0 then
    Result := CreateJSONObject
  else
    Result := TJSONObject.Create;
end;

procedure TJDOCustomQuery.FromJSONArray(AJSON: TJSONArray);
var
  I: Integer;
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SNilFieldDefsError);
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, SNilJSONParamError);
  if AJSON.JSONType <> jtArray then
    raise EJDOQuery.Create(Self, '"AJSON.JSONType" must be "jtArray".');
  for I := 0 to Pred(AJSON.Count) do
  begin
    ReadJSONObject(AJSON[I] as TJSONObject);
    ExecSQL;
  end;
end;

procedure TJDOCustomQuery.FromJSONObject(AJSON: TJSONObject);
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SNilFieldDefsError);
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, SNilJSONParamError);
  if AJSON.JSONType <> jtObject then
    raise EJDOQuery.Create(Self, '"AJSON.JSONType" must be "jtObject".');
  ReadJSONObject(AJSON);
  ExecSQL;
end;

function TJDOCustomQuery.Execute: Boolean;
begin
  ExecSQL;
  Result := RowsAffected > 0;
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

function TJDOCustomQuery.Open: Boolean;
begin
  inherited Open;
  Result := RecordCount > 0;
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

function TJDOCustomQuery.Schema: TJSONObject;
var
  I: Integer;
  VArray: TJSONArray;
  VObject: TJSONObject;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  Result := TJSONObject.Create;
  VArray := TJSONArray.Create;
  Result.Add('fields', VArray);
  for I := 0 to Pred(FieldDefs.Count) do
  begin
    VFieldDef := FieldDefs[I];
    VObject := TJSONObject.Create(['name', VFieldDef.name]);
    VArray.Add(VObject);
    VFieldType := GetJSONFieldType(VFieldDef.DataType);
    VObject.Strings['type'] := VFieldType;
    if VFieldType = FT_STRING then
      VObject.Integers['maxlen'] := VFieldDef.Size;
    if VFieldDef.Required then
      VObject.Booleans['required'] := True;
    if VFieldDef.Precision <> -1 then
      VObject.Integers['precision'] := VFieldDef.Precision;
  end;
end;

function TJDOCustomQuery.JSONSchema: TJSONStringType;
var
  I, C: Integer;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  Result := ES;
  C := FieldDefs.Count;
  if C = 0 then
  begin
    Result := '{}';
    Exit;
  end;
  for I := 0 to Pred(C) do
  begin
    VFieldDef := FieldDefs[I];
    VFieldType := GetJSONFieldType(VFieldDef.DataType);
    Result += '{ "name": "' + VFieldDef.Name + '"';
    Result += ', "type": "' + VFieldType + '"';
    if VFieldType = FT_STRING then
      Result += ', "maxlen": ' + IntToStr(VFieldDef.Size);
    if VFieldDef.Required then
      Result += ', "required": true';
    if VFieldDef.Precision <> -1 then
      Result += ', "precision": ' + IntToStr(VFieldDef.Precision);
    if Succ(I) < C then
      Result += ' }, '
    else
      Result += ' }';
  end;
end;
