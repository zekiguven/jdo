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
end;

procedure TJDOCustomQuery.InternalCheckFieldDefs;
begin
  if not Assigned(FieldDefs) then
    raise EJDOQuery.Create(Self, SNilFieldDefsError);
end;

procedure TJDOCustomQuery.InternalCheckJSONParam(AJSON: TJSONData);
begin
  if not Assigned(AJSON) then
    raise EJDOQuery.Create(Self, SNilJSONParamError);
end;

class procedure TJDOCustomQuery.JSONToQuery(AJSON: TJSONObject;
  AQuery: TSQLQuery; const ADateAsString: Boolean);
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

class procedure TJDOCustomQuery.JSONToDataSet(AJSON: TJSONObject;
  ADataSet: TDataSet; const ADateAsString: Boolean);
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

class procedure TJDOCustomQuery.DataSetToJSON(ADataSet: TDataSet;
  AJSON: TJSONArray; const ADateAsString: Boolean);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    DataSetToJSON(ADataSet, VJSON, ADateAsString);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;

class procedure TJDOCustomQuery.DataSetToJSON(ADataSet: TDataSet;
  AJSON: TJSONObject; const ADateAsString: Boolean);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(ADataSet.Fields.Count) do
  begin
    VField := ADataSet.Fields[I];
    VFieldType := TJDOCustomQuery.GetJSONType(VField.DataType);
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
      if ADateAsString then
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

function TJDOCustomQuery.GetPrimaryKey: TIndexDef;
var
  I: Integer;
begin
  for I := 0 to Pred(ServerIndexDefs.Count) do
  begin
    Result := ServerIndexDefs[I];
    if ixPrimary in Result.Options then
      Break;
    raise EJDOSQL.Create(Self, SPrimaryKeyNotFound);
  end;
end;

function TJDOCustomQuery.GetJSONValuesFromPrimaryKey(
  const AJSON: TJSONObject): Variant;
var
  I: Integer;
  VNames, VName: string;
begin
  VNames := GetPrimaryKey.Fields;
  Result := VarArrayCreate([0, Pred(IndexNamesCount(VNames))], varVariant);
  VName := NextIndexName(VNames);
  I := 0;
  while VName <> ES do
  begin
    Result[I] := AJSON[VName].Value;
    VName := NextIndexName(VNames);
    Inc(I);
  end;
end;

procedure TJDOCustomQuery.GetJSON(out AJSON: TJSONArray);
var
  VBookMark: TBookMark;
begin
  AJSON := TJSONArray.Create;
  Open;
  if RecordCount = 0 then
    Exit;
  try
    DisableControls;
    VBookMark := {$IFDEF JDO_NEW_FPC}BookMark{$ELSE}GetBookmark{$ENDIF};
    DataSetToJSON(Self, AJSON, FDateAsString);
  finally
{$IFDEF JDO_NEW_FPC}
    BookMark := VBookMark
{$ELSE}
    GotoBookmark(VBookMark)
{$ENDIF};
    EnableControls;
  end;
end;

procedure TJDOCustomQuery.GetJSON(out AJSON: TJSONObject);
begin
  AJSON := TJSONObject.Create;
  Open;
  if RecordCount <> 0 then
    DataSetToJSON(Self, AJSON, FDateAsString);
end;

procedure TJDOCustomQuery.SetJSON(AJSON: TJSONArray);
var
  I: Integer;
begin
  InternalCheckFieldDefs;
  InternalCheckJSONParam(AJSON);
  for I := 0 to Pred(AJSON.Count) do
  begin
    TJDOCustomQuery.JSONToQuery(AJSON.Objects[I], Self, FDateAsString);
    ExecSQL;
  end;
end;

procedure TJDOCustomQuery.SetJSON(AJSON: TJSONObject);
begin
  InternalCheckFieldDefs;
  InternalCheckJSONParam(AJSON);
  TJDOCustomQuery.JSONToQuery(AJSON, Self, FDateAsString);
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

procedure TJDOCustomQuery.LoadJSONFromStream(AStream: TStream);
var
  VArray: TJSONArray;
  VParser: TJSONParser;
begin
  VParser := TJSONParser.Create(AStream);
  try
    VArray := VParser.Parse as TJSONArray;
    Append(VArray);
  finally
    VArray.Free;
    VParser.Free;
  end;
end;

procedure TJDOCustomQuery.LoadJSONFromFile(const AFileName: TFileName);
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadJSONFromStream(VFile);
  finally
    VFile.Free;
  end;
end;

procedure TJDOCustomQuery.SetAsJSON(const AValue: TJSONStringType);
var
  VString: TStringStream;
begin
  VString := TStringStream.Create(AValue);
  try
    LoadJSONFromStream(VString);
  finally
    VString.Free;
  end;
end;

procedure TJDOCustomQuery.SaveJSONToStream(AStream: TStream);
var
  VArray: TJSONArray;
  VJSON: TJSONStringType;
begin
  GetJSON(VArray);
  try
    VJSON := VArray.AsJSON;
    AStream.Write(Pointer(VJSON)^, Length(VJSON));
  finally
    VArray.Free;
  end;
end;

procedure TJDOCustomQuery.SaveJSONToFile(const AFileName: TFileName);
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmCreate);
  try
    SaveJSONToStream(VFile);
  finally
    VFile.Free;
  end;
end;

function TJDOCustomQuery.GetAsJSON: TJSONStringType;
var
  VString: TStringStream;
begin
  VString := TStringStream.Create(ES);
  try
    SaveJSONToStream(VString);
    Result := VString.DataString;
  finally
    VString.Free;
  end;
end;

procedure TJDOCustomQuery.Apply(const ARetaining: Boolean);
var
  VTrans: TSQLTransaction;
begin
  VTrans := Transaction as TSQLTransaction;
  if not VTrans.Active then
    Exit;
  try
    ApplyUpdates(0);
    if ARetaining then
      VTrans.CommitRetaining
    else
      VTrans.Commit;
  except
    if ARetaining then
      VTrans.RollbackRetaining
    else
      VTrans.Rollback;
    raise;
  end;
end;

procedure TJDOCustomQuery.Undo(const ARetaining: Boolean);
var
  VTrans: TSQLTransaction;
begin
  VTrans := Transaction as TSQLTransaction;
  if not VTrans.Active then
    Exit;
  CancelUpdates;
  if ARetaining then
    VTrans.RollbackRetaining
  else
    VTrans.Rollback;
end;

procedure TJDOCustomQuery.Commit(const ARetaining: Boolean);
var
  VTrans: TSQLTransaction;
begin
  VTrans := Transaction as TSQLTransaction;
  if not VTrans.Active then
    Exit;
  try
    if ARetaining then
      VTrans.CommitRetaining
    else
      VTrans.Commit;
  except
    if ARetaining then
      VTrans.RollbackRetaining
    else
      VTrans.Rollback;
    raise;
  end;
end;

procedure TJDOCustomQuery.Rollback(const ARetaining: Boolean);
var
  VTrans: TSQLTransaction;
begin
  VTrans := Transaction as TSQLTransaction;
  if not VTrans.Active then
    Exit;
  if ARetaining then
    VTrans.RollbackRetaining
  else
    VTrans.Rollback;
end;

class procedure TJDOCustomQuery.QueryToSchema(AQuery: TSQLQuery;
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
    VFieldType := TJDOCustomQuery.GetJSONType(VFieldDef.DataType);
    VObject.Strings['type'] := VFieldType;
    if VFieldType = FT_STRING then
      VObject.Integers['maxlen'] := VFieldDef.Size;
    if VFieldDef.Required then
      VObject.Booleans['required'] := True;
    if VFieldDef.Precision <> -1 then
      VObject.Integers['precision'] := VFieldDef.Precision;
  end;
end;

procedure TJDOCustomQuery.GetSchema(out ASchema: TJSONObject);
begin
  ASchema := TJSONObject.Create;
  InternalCheckFieldDefs;
  TJDOCustomQuery.QueryToSchema(Self, ASchema);
end;

class procedure TJDOCustomQuery.QueryToSchema(AQuery: TSQLQuery;
  out ASchema: TJSONStringType);
var
  I, C: Integer;
  VFieldDef: TFieldDef;
  VFieldType: ShortString;
begin
  C := AQuery.FieldDefs.Count;
  if C = 0 then
  begin
    ASchema := '{}';
    Exit;
  end;
  for I := 0 to Pred(C) do
  begin
    VFieldDef := AQuery.FieldDefs[I];
    VFieldType := TJDOCustomQuery.GetJSONType(VFieldDef.DataType);
    ASchema += '{ "name": "' + VFieldDef.Name + '"';
    ASchema += ', "type": "' + VFieldType + '"';
    if VFieldType = FT_STRING then
      ASchema += ', "maxlen": ' + IntToStr(VFieldDef.Size);
    if VFieldDef.Required then
      ASchema += ', "required": true';
    if VFieldDef.Precision <> -1 then
      ASchema += ', "precision": ' + IntToStr(VFieldDef.Precision);
    if Succ(I) < C then
      ASchema += ' }, '
    else
      ASchema += ' }';
  end;
end;

procedure TJDOCustomQuery.GetSchema(out ASchema: TJSONStringType);
begin
  InternalCheckFieldDefs;
  TJDOCustomQuery.QueryToSchema(Self, ASchema);
end;

function TJDOCustomQuery.Field(const AFieldName: string): TField;
begin
  Result := Fields.FindField(AFieldName);
  if Result = nil then
    DatabaseErrorFmt(SFieldNotFound, [AFieldName], Self);
end;

function TJDOCustomQuery.Param(const AParamName: string): TParam;
begin
  Result := Params.FindParam(AParamName);
  if Result = nil then
    DatabaseErrorFmt(SParameterNotFound, [AParamName], Self);
end;

procedure TJDOCustomQuery.Append(AJSON: TJSONArray);
var
  I: Integer;
begin
  InternalCheckJSONParam(AJSON);
  for I := 0 to Pred(AJSON.Count) do
  begin
    inherited Append;
    TJDOCustomQuery.JSONToDataSet(AJSON.Objects[I], Self, FDateAsString);
    Post;
  end;
end;

procedure TJDOCustomQuery.Append(AJSON: TJSONObject);
begin
  InternalCheckJSONParam(AJSON);
  inherited Append;
  TJDOCustomQuery.JSONToDataSet(AJSON, Self, FDateAsString);
end;

procedure TJDOCustomQuery.Insert(AJSON: TJSONArray);
var
  I: Integer;
begin
  InternalCheckJSONParam(AJSON);
  for I := 0 to Pred(AJSON.Count) do
  begin
    inherited Insert;
    TJDOCustomQuery.JSONToDataSet(AJSON.Objects[I], Self, FDateAsString);
    Post;
  end;
end;

procedure TJDOCustomQuery.Insert(AJSON: TJSONObject);
begin
  InternalCheckJSONParam(AJSON);
  inherited Insert;
  TJDOCustomQuery.JSONToDataSet(AJSON, Self, FDateAsString);
end;

procedure TJDOCustomQuery.Edit(AJSON: TJSONArray);
var
  I: Integer;
  VBookMark: TBookMark;
  VObject: TJSONObject;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  try
    VPrimaryKey := GetPrimaryKey;
    VBookMark := {$IFDEF JDO_NEW_FPC}BookMark{$ELSE}GetBookmark{$ENDIF};
    if Pos(SC, VPrimaryKey.Fields) <> 0 then
    begin
      for I := 0 to Pred(AJSON.Count) do
      begin
        VObject := AJSON.Objects[I];
        if Locate(VPrimaryKey.Fields,
          GetJSONValuesFromPrimaryKey(VObject), []) then
        begin
          inherited Edit;
          TJDOCustomQuery.JSONToDataSet(VObject, Self, FDateAsString);
          Post;
        end;
      end;
    end
    else
      for I := 0 to Pred(AJSON.Count) do
      begin
        VObject := AJSON.Objects[I];
        if Locate(VPrimaryKey.Fields,
          VObject[VPrimaryKey.Fields].Value, []) then
        begin
          inherited Edit;
          TJDOCustomQuery.JSONToDataSet(VObject, Self, FDateAsString);
          Post;
        end;
      end;
  finally
{$IFDEF JDO_NEW_FPC}
    BookMark := VBookMark
{$ELSE}
    GotoBookmark(VBookMark)
{$ENDIF};
  end;
end;

procedure TJDOCustomQuery.Edit(AJSON: TJSONObject);
var
  VBookMark: TBookMark;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  try
    VPrimaryKey := GetPrimaryKey;
    VBookMark := {$IFDEF JDO_NEW_FPC}BookMark{$ELSE}GetBookmark{$ENDIF};
    if Pos(SC, VPrimaryKey.Fields) <> 0 then
    begin
      if Locate(VPrimaryKey.Fields, GetJSONValuesFromPrimaryKey(AJSON), []) then
      begin
        inherited Edit;
        TJDOCustomQuery.JSONToDataSet(AJSON, Self, FDateAsString);
      end;
    end
    else
      if Locate(VPrimaryKey.Fields, AJSON[VPrimaryKey.Fields].Value, []) then
      begin
        inherited Edit;
        TJDOCustomQuery.JSONToDataSet(AJSON, Self, FDateAsString);
      end;
  finally
{$IFDEF JDO_NEW_FPC}
    BookMark := VBookMark
{$ELSE}
    GotoBookmark(VBookMark)
{$ENDIF};
  end;
end;

procedure TJDOCustomQuery.Delete(AJSON: TJSONArray);
var
  I: Integer;
  VBookMark: TBookMark;
  VObject: TJSONObject;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  try
    VPrimaryKey := GetPrimaryKey;
    VBookMark := {$IFDEF JDO_NEW_FPC}BookMark{$ELSE}GetBookmark{$ENDIF};
    if Pos(SC, VPrimaryKey.Fields) <> 0 then
    begin
      for I := 0 to Pred(AJSON.Count) do
      begin
        VObject := AJSON.Objects[I];
        if Locate(VPrimaryKey.Fields,
          GetJSONValuesFromPrimaryKey(VObject), []) then
          inherited Delete;
      end;
    end
    else
      for I := 0 to Pred(AJSON.Count) do
      begin
        VObject := AJSON.Objects[I];
        if Locate(VPrimaryKey.Fields,
          VObject[VPrimaryKey.Fields].Value, []) then
          inherited Delete;
      end;
  finally
    if not IsEmpty then
{$IFDEF JDO_NEW_FPC}
      BookMark := VBookMark
{$ELSE}
      GotoBookmark(VBookMark)
{$ENDIF};
  end;
end;

procedure TJDOCustomQuery.Delete(AJSON: TJSONObject);
var
  VBookMark: TBookMark;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  try
    VPrimaryKey := GetPrimaryKey;
    VBookMark := {$IFDEF JDO_NEW_FPC}BookMark{$ELSE}GetBookmark{$ENDIF};
    if Pos(SC, VPrimaryKey.Fields) <> 0 then
    begin
      if Locate(VPrimaryKey.Fields, GetJSONValuesFromPrimaryKey(AJSON), []) then
        inherited Delete;
    end
    else
      if Locate(VPrimaryKey.Fields, AJSON[VPrimaryKey.Fields].Value, []) then
        inherited Delete;
  finally
    if not IsEmpty then
{$IFDEF JDO_NEW_FPC}
      BookMark := VBookMark
{$ELSE}
      GotoBookmark(VBookMark)
{$ENDIF};
  end;
end;
