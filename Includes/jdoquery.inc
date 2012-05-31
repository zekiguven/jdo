(*
  JDOQuery implementation include
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file Contributors.txt, included in this distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

constructor TJDOQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TSQLConnection then
  begin
    SetDatabase(AOwner as TSQLConnection);
    Transaction := TSQLConnection(AOwner).Transaction;
  end;
  if AOwner is TJDODataBase then
  begin
    SetDatabase((AOwner as TJDODataBase).Connection);
    Transaction := TJDODataBase(AOwner).Transaction;
  end;
end;

function TJDOQuery.ToJSON: TJSONArray;
var
  VField: TField;
  I, P: Integer;
  VJSONObj: TJSONObject;
  VFieldType, VFieldName: ShortString;
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
      VJSONObj := TJSONObject.Create;
      Result.Add(VJSONObj);
      for I := 0 to Pred(Fields.Count) do
      begin
        VField := Fields[I];
        case VField.DataType of
          ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
            ftInterface, ftIDispatch: VFieldType := FT_NULL;
          ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
            ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
            ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftVariant, ftGuid: VFieldType := FT_STRING;
          ftSmallint, ftInteger, ftLargeint, ftWord,
            ftAutoInc: VFieldType := FT_INT;
          ftBoolean: VFieldType := FT_BOOLEAN;
          ftFloat, ftCurrency, ftBCD, ftFMTBcd: VFieldType := FT_FLOAT;
          ftDate, ftTime, ftDateTime, ftTimeStamp: VFieldType := FT_DATE;
        end;
        VFieldName := VField.FieldName;
        if (VFieldType = FT_NULL) or VField.IsNull then
        begin
          VJSONObj.Add(VFieldName);
          Continue;
        end;
        if VFieldType = FT_STRING then
          VJSONObj.Add(VFieldName, VField.AsString);
        if VFieldType = FT_BOOLEAN then
          VJSONObj.Add(VFieldName, VField.AsBoolean);
        if VFieldType = FT_DATE then
          if FDateAsString then
            VJSONObj.Add(VFieldName, VField.AsString)
          else
            VJSONObj.Add(VFieldName, VField.AsFloat);
        if VFieldType = FT_FLOAT then
          VJSONObj.Add(VFieldName, VField.AsFloat);
        if VFieldType = FT_INT then
          VJSONObj.Add(VFieldName, VField.AsInteger);
      end;
      Next;
    end;
  finally
    RecNo := P;
    EnableControls;
  end;
end;

procedure TJDOQuery.FromJSON(AJSON: TJSONArray);
var
  I, J: Integer;
  VName: string;
  VParam: TParam;
  VData: TJSONData;
  VFieldDef: TFieldDef;
  VJSONObj: TJSONObject;
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SFieldDefsNilError);
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, '"AJSON" must be not nil.');
  if AJSON.JSONType <> jtArray then
    raise EJDOQuery.Create(Self, '"AJSON.JSONType" must be "jtArray".');
  for I := 0 to Pred(AJSON.Count) do
  begin
    VJSONObj := AJSON[I] as TJSONObject;
    for J := 0 to Pred(VJSONObj.Count) do
    begin
      VName := VJSONObj.Names[J];
      VParam := Params.FindParam(VName);
      if not Assigned(VParam) then
        Continue;
      VData := VJSONObj.Items[J];
      if VData.IsNull then
        VParam.Clear
      else
      begin
        VFieldDef := FieldDefs.Find(VName);
        if not Assigned(VFieldDef) then
          raise EJDOQuery.CreateFmt(Self, SFieldDefNotFound, [VName]);
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
    ExecSQL;
  end;
end;

function TJDOQuery.Execute: Boolean;
begin
  ExecSQL;
  Result := RowsAffected > 0;
end;

function TJDOQuery.Open: Boolean;
begin
  inherited Open;
  Result := RecordCount > 0;
end;
