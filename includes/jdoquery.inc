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

procedure TJDOCustomQuery.JSONToQuery(AJSON: TJSONObject;
  AQuery: TSQLQuery; ADateAsString: Boolean);
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
    VParam := AQuery.Params.FindParam(VName);
    if not Assigned(VParam) then
      Continue;
    VData := AJSON.Items[I];
    if VData.IsNull then
      VParam.Clear
    else
    begin
      VFieldDef := AQuery.FieldDefs.Find(VName);
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
          if ADateAsString then
            VParam.AsDateTime := StrToDateTime(VData.AsString)
          else
            VParam.AsDateTime := VData.AsFloat;
      end;
    end;
  end;
end;

procedure TJDOCustomQuery.JSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet;
  ADateAsString: Boolean);
var
  I: Integer;
  VName: string;
  VField: TField;
  VData: TJSONData;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VField := ADataSet.Fields.FindField(VName);
    if not Assigned(VField) then
      Continue;
    VData := AJSON.Items[I];
    if VData.IsNull then
      VField.Clear
    else
    begin
      case VField.DataType of
        ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
          ftInterface, ftIDispatch: VField.Clear;
        ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
          ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
          ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
          ftVariant,ftGuid: VField.AsString := VData.AsString;
        ftSmallint, ftInteger, ftLargeint, ftWord,
          ftAutoInc: VField.AsInteger := VData.AsInteger;
        ftBoolean: VField.AsBoolean := VData.AsBoolean;
        ftFloat, ftCurrency, ftBCD, ftFMTBcd: VField.AsFloat := VData.AsFloat;
        ftDate, ftTime, ftDateTime, ftTimeStamp:
          if ADateAsString then
            VField.AsDateTime := StrToDateTime(VData.AsString)
          else
            VField.AsDateTime := VData.AsFloat;
      end;
    end;
  end;
end;

procedure TJDOCustomQuery.DataSetToJSON(ADataSet: TDataSet;
  AJSON: TJSONArray);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    DataSetToJSON(ADataSet, VJSON);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;

procedure TJDOCustomQuery.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONObject);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(ADataSet.Fields.Count) do
  begin
    VField := ADataSet.Fields[I];
    VFieldType := GetJSONType(VField.DataType);
    VFieldName := VField.FieldName;
    if (VFieldType = FT_NULL) or VField.IsNull then
    begin
      AJSON.Add(VFieldName);
      Continue;
    end;
    if VFieldType = FT_STRING then
      AJSON.Add(VFieldName, VField.AsString);
    if VFieldType = FT_BOOLEAN then
      AJSON.Add(VFieldName, VField.AsBoolean);
    if VFieldType = FT_DATE then
      if FDateAsString then
        AJSON.Add(VFieldName, VField.AsString)
      else
        AJSON.Add(VFieldName, VField.AsFloat);
    if VFieldType = FT_FLOAT then
      AJSON.Add(VFieldName, VField.AsFloat);
    if VFieldType = FT_INT then
      AJSON.Add(VFieldName, VField.AsInteger);
  end;
end;

class function TJDOCustomQuery.GetJSONType(
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

function TJDOCustomQuery.GetJSONArray: TJSONArray;
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
    DataSetToJSON(Self, Result);
  finally
    RecNo := P;
    EnableControls;
  end;
end;

function TJDOCustomQuery.GetJSONObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Open;
  if RecordCount <> 0 then
    DataSetToJSON(Self, Result);
end;

procedure TJDOCustomQuery.SetJSONArray(AJSON: TJSONArray);
var
  I: Integer;
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SNilFieldDefsError);
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, SNilJSONParamError);
  for I := 0 to Pred(AJSON.Count) do
  begin
    JSONToQuery(AJSON.Objects[I], Self, FDateAsString);
    ExecSQL;
  end;
end;

procedure TJDOCustomQuery.SetJSONObject(AJSON: TJSONObject);
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SNilFieldDefsError);
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, SNilJSONParamError);
  JSONToQuery(AJSON, Self, FDateAsString);
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

procedure TJDOCustomQuery.QueryToSchema(AQuery: TSQLQuery;
  ASchema: TJSONObject);
var
  I: Integer;
  VArray: TJSONArray;
  VObject: TJSONObject;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  VArray := TJSONArray.Create;
  ASchema.Add('fields', VArray);
  for I := 0 to Pred(AQuery.FieldDefs.Count) do
  begin
    VFieldDef := AQuery.FieldDefs[I];
    VObject := TJSONObject.Create(['name', VFieldDef.name]);
    VArray.Add(VObject);
    VFieldType := GetJSONType(VFieldDef.DataType);
    VObject.Strings['type'] := VFieldType;
    if VFieldType = FT_STRING then
      VObject.Integers['maxlen'] := VFieldDef.Size;
    if VFieldDef.Required then
      VObject.Booleans['required'] := True;
    if VFieldDef.Precision <> -1 then
      VObject.Integers['precision'] := VFieldDef.Precision;
  end;
end;

function TJDOCustomQuery.Schema: TJSONObject;
begin
  Result := TJSONObject.Create;
  QueryToSchema(Self, Result);
end;

procedure TJDOCustomQuery.QueryToJSONSchema(AQuery: TSQLQuery;
  out AJSONSchema: TJSONStringType);
var
  I, C: Integer;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  C := AQuery.FieldDefs.Count;
  if C = 0 then
  begin
    AJSONSchema := '{}';
    Exit;
  end;
  for I := 0 to Pred(C) do
  begin
    VFieldDef := AQuery.FieldDefs[I];
    VFieldType := GetJSONType(VFieldDef.DataType);
    AJSONSchema += '{ "name": "' + VFieldDef.Name + '"';
    AJSONSchema += ', "type": "' + VFieldType + '"';
    if VFieldType = FT_STRING then
      AJSONSchema += ', "maxlen": ' + IntToStr(VFieldDef.Size);
    if VFieldDef.Required then
      AJSONSchema += ', "required": true';
    if VFieldDef.Precision <> -1 then
      AJSONSchema += ', "precision": ' + IntToStr(VFieldDef.Precision);
    if Succ(I) < C then
      AJSONSchema += ' }, '
    else
      AJSONSchema += ' }';
  end;
end;

function TJDOCustomQuery.JSONSchema: TJSONStringType;
begin
  QueryToJSONSchema(Self, Result);
end;
