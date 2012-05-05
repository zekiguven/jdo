(*
  JSON Data Objects unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDO;

{$I jdo.inc}

interface

uses
  JDOConsts, Classes, SysUtils, SQLdb, DB, TypInfo, Contnrs, FPJSON;

type

  EJDOException = class(Exception);

  EJDOConnection = class(EJDOException);

  EJDOQuery = class(EJDOException);

  TJDOFieldTypes = (ftNull, ftStr, ftBool, ftDate, ftFloat, ftInt);

  TJDOQuerySQLOperation = (soNone, soSelect, soInsert, soUpdate, soDelete);

  TJDOLikeOptions = set of (loCaseInsensitive, loPartialKey);

  { TJDODataBase }

  TJDODataBase = class
  private
    FConfig: TStrings;
    FConfigFileName: TFileName;
    FConnection: TSQLConnection;
    FQuery: TSQLQuery;
    FSQL: TStringList;
    FTransaction: TSQLTransaction;
    function GetFields: TFields;
    function GetParams: TParams;
    procedure InternalCreateConnection;
    procedure InternalCreateTransaction;
    procedure InternalCreateQuery;
  public
    constructor Create(const AConfigFileName: TFileName;
      const AConnect: Boolean = True);
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SetProperties;
    procedure Prepare(const ASQL: string = ES);
    function Field(const AFieldByName: string): TField;
    function Param(const AParamName: string): TParam;
    function Open: Boolean;
    function Execute: Boolean;
    procedure StartTrans;
    procedure Commit;
    procedure Rollback;
    property Config: TStrings read FConfig;
    property ConfigFileName: TFileName read FConfigFileName write FConfigFileName;
    property Connection: TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TSQLQuery read FQuery;
    property SQL: TStringList read FSQL;
    property Fields: TFields read GetFields;
    property Params: TParams read GetParams;
  end;

  { TJDOQuery }

  TJDOQuery = class
  private
    FAdditionalSQL: TStrings;
    FLike: string;
    FLikeKey: string;
    FLikeValue: string;
    FLastSQLOperation: TJDOQuerySQLOperation;
    FDataBase: TJDODataBase;
    FFields: TJSONObject;
    FItems: TObjectList;
    FOrderBy: Boolean;
    FPrimaryKey: string;
    FDateAsString: Boolean;
    FSQL: TStrings;
    FTableAlias: string;
    FTableName: string;
    function GetItems(AIndex: Integer): TJSONObject;
    procedure SetItems(AIndex: Integer; const AValue: TJSONObject);
  public
    constructor Create;
    constructor Create(ADataBase: TJDODataBase; const ATableName: string);
    destructor Destroy; override;
    procedure Prepare(const ASQLOperation: TJDOQuerySQLOperation;
      const AAdditionalSQL: string = ES); virtual;
    procedure AddField(const AFieldName: ShortString;
      const AFieldType: TJDOFieldTypes;
      const AIsPK: Boolean = False);
    procedure Like(const AValue, AKey: string;
      const AOptions: TJDOLikeOptions = []);
    function Insert(AJSONObject: TJSONObject): Boolean; virtual;
    function Insert(AJSONArray: TJSONArray): Boolean; virtual;
    function Update(AJSONObject: TJSONObject): Boolean; virtual;
    function Update(AJSONArray: TJSONArray): Boolean; virtual;
    function Delete(AJSONObject: TJSONObject): Boolean; virtual;
    function Delete(AJSONArray: TJSONArray): Boolean; virtual;
    function Open(const AAdditionalSQL: string = ES): Boolean; virtual;
    function Count: Integer;
    function First: TJSONObject;
    function Last: TJSONObject;
    function AsJSON: TJSONStringType;
    function Field(const AFieldName: string): TField;
    function Param(const AParamName: string): TParam;
    property DataBase: TJDODataBase read FDataBase write FDataBase;
    property Items[AIndex: Integer]: TJSONObject read GetItems
      write SetItems; default;
    property Fields: TJSONObject read FFields;
    property TableName: string read FTableName write FTableName;
    property TableAlias: string read FTableAlias write FTableAlias;
    property PrimaryKey: string read FPrimaryKey write FPrimaryKey;
    property SQL: TStrings read FSQL write FSQL;
    property AdditionalSQL: TStrings read FAdditionalSQL write FAdditionalSQL;
    property OrderBy: Boolean read FOrderBy write FOrderBy;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
  end;

function JDOFieldTypeToJDOFieldType(
  const AFieldType: TFieldType): ShortString;
function JDOFieldTypeToJDOFieldTypeEnum(
  const AFieldType: TFieldType): TJDOFieldTypes;
procedure JDOFieldsToJSONObject(AFields: TFields;
  AJSONFiels, AJSONObject: TJSONObject; const ADateAsString: Boolean);
procedure JDOJSONObjectToParams(AParams: TParams;
  AJSONFiels, AJSONObject: TJSONObject; const APKFieldName: string = ES);

implementation

function JDOFieldTypeToJDOFieldType(
  const AFieldType: TFieldType): ShortString;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftInterface, ftIDispatch: Result := JDO_FT_NULL;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob, ftOraClob,
      ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftVariant,
      ftGuid: Result := JDO_FT_STR;
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc: Result := JDO_FT_INT;
    ftBoolean: Result := JDO_FT_BOOL;
    DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := JDO_FT_FLOAT;
    DB.ftDate, ftTime, ftDateTime, ftTimeStamp: Result := JDO_FT_DATE;
  end;
end;

function JDOFieldTypeToJDOFieldTypeEnum(
  const AFieldType: TFieldType): TJDOFieldTypes;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftInterface, ftIDispatch: Result := ftNull;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob, ftOraClob,
      ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftVariant,
      ftGuid: Result := ftStr;
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc: Result := ftInt;
    ftBoolean: Result := ftBool;
    DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := ftFloat;
    DB.ftDate, ftTime, ftDateTime, ftTimeStamp: Result := ftDate;
  end;
end;

procedure JDOFieldsToJSONObject(AFields: TFields;
  AJSONFiels, AJSONObject: TJSONObject; const ADateAsString: Boolean);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    if AJSONFiels.Count > 0 then
    begin
      VFieldType := AJSONFiels.Items[I].AsString;
      VFieldName := AJSONFiels.Names[I];
    end
    else
    begin
      VFieldType := JDOFieldTypeToJDOFieldType(VField.DataType);
      VFieldName := VField.FieldName;
    end;
    if (VFieldType = JDO_FT_NULL) or VField.IsNull then
    begin
      AJSONObject.Add(VFieldName);
      Continue;
    end;
    if VFieldType = JDO_FT_STR then
      AJSONObject.Add(VFieldName, VField.AsString);
    if VFieldType = JDO_FT_BOOL then
      AJSONObject.Add(VFieldName, VField.AsBoolean);
    if VFieldType = JDO_FT_DATE then
    begin
      if ADateAsString then
        AJSONObject.Add(VFieldName, VField.AsString)
      else
        AJSONObject.Add(VFieldName, VField.AsFloat);
    end;
    if VFieldType = JDO_FT_FLOAT then
      AJSONObject.Add(VFieldName, VField.AsFloat);
    if VFieldType = JDO_FT_INT then
      AJSONObject.Add(VFieldName, VField.AsInteger);
  end;
end;

procedure JDOJSONObjectToParams(AParams: TParams;
  AJSONFiels, AJSONObject: TJSONObject; const APKFieldName: string);
var
  VParam: TParam;
  VField, VData: TJSONData;
  VFieldType, VName: ShortString;
  I, VJSONObjsCount: Integer;
begin
  VJSONObjsCount := AJSONObject.Count;
  if AJSONFiels.Count <> VJSONObjsCount then
    raise EJDOException.Create(SJDOJSONObjectToParamsError);
  for I := 0 to Pred(VJSONObjsCount) do
  begin
    VName := AJSONFiels.Names[I];
    VField := AJSONFiels.Items[I];
    VParam := AParams.ParamByName(VName);
    VData := AJSONObject[VName];
    if (APKFieldName <> ES) and (APKFieldName = VName) and
      (not VData.IsNull) and Assigned(VParam) then
    begin
      VParam.AsInteger := VData.AsInt64;
      Continue;
    end;
    VFieldType := VField.AsString;
    if (VFieldType = JDO_FT_NULL) or VData.IsNull or VField.IsNull then
    begin
      AParams.Clear;
      Continue;
    end;
    if VFieldType = JDO_FT_STR then
      VParam.AsString := VData.AsString;
    if VFieldType = JDO_FT_BOOL then
      VParam.AsBoolean := (VData.AsString = 'on') or VData.AsBoolean;
    if VFieldType = JDO_FT_DATE then
    begin
      if VData.JSONType = jtNumber then
        VParam.AsDateTime := VData.AsFloat
      else
        VParam.AsDateTime := StrToDateTime(VData.AsString);
    end;
    if VFieldType = JDO_FT_FLOAT then
      VParam.AsFloat := VData.AsFloat;
    if VFieldType = JDO_FT_INT then
      VParam.AsInteger := VData.AsInteger;
  end;
end;

{ TJDODataBase }

constructor TJDODataBase.Create(const AConfigFileName: TFileName;
  const AConnect: Boolean);
begin
  FConfig := TStringList.Create;
  if AConfigFileName <> ES then
  begin
    FConfigFileName := AConfigFileName;
    LoadConfig;
    InternalCreateConnection;
    InternalCreateTransaction;
    InternalCreateQuery;
    SetProperties;
    FSQL := FQuery.SQL;
    if AConnect then
      FConnection.Open;
  end;
end;

destructor TJDODataBase.Destroy;
begin
  FConfig.Free;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TJDODataBase.InternalCreateConnection;
var
  VConnectorName: ShortString;
  VConnectionDef: TConnectionDef;
begin
  VConnectorName := FConfig.Values[JDO_CONNECTOR_NAME];
  if Trim(VConnectorName) = ES then
    raise EJDOConnection.Create(SJDOConnectorNameEmptyError);
  VConnectionDef := GetConnectionDef(VConnectorName);
  if Assigned(VConnectionDef) then
    FConnection := VConnectionDef.ConnectionClass.Create(nil)
  else
    raise EJDOConnection.CreateFmt(
      SJDOConnectorUnitWasNotDeclaredError, [VConnectorName]);
end;

function TJDODataBase.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TJDODataBase.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

procedure TJDODataBase.InternalCreateTransaction;
begin
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
end;

procedure TJDODataBase.InternalCreateQuery;
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
end;

procedure TJDODataBase.LoadConfig;
begin
  if not FileExists(FConfigFileName) then
    raise EJDOConnection.CreateFmt(
      SJDOConfigFileNotFoundError, [FConfigFileName]);
  FConfig.LoadFromFile(FConfigFileName);
end;

procedure TJDODataBase.SetProperties;
var
  I: Integer;
  VPropName, VToken: ShortString;
begin
  for I := 0 to Pred(FConfig.Count) do
  begin
    VPropName := FConfig.Names[I];
    VToken := Copy(VPropName, 1, 1);
    if (CompareText(VPropName, JDO_CONNECTOR_NAME) = 0) or
      (VToken = PO) or (VToken = ES) then
      Continue;
    if IsPublishedProp(FConnection, VPropName) then
      SetPropValue(FConnection, VPropName, FConfig.Values[VPropName])
    else
      raise EJDOConnection.CreateFmt(SJDOInvalidPropInConfigFile,
        [ExtractFileName(FConfigFileName), VPropName]);
  end;
end;

procedure TJDODataBase.Prepare(const ASQL: string);
begin
  FQuery.SQL.Text := ASQL;
end;

function TJDODataBase.Field(const AFieldByName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AFieldByName);
end;

function TJDODataBase.Param(const AParamName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AParamName);
end;

function TJDODataBase.Open: Boolean;
begin
  FQuery.Open;
  Result := FQuery.RecordCount > 0;
end;

function TJDODataBase.Execute: Boolean;
begin
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected > 0;
end;

procedure TJDODataBase.StartTrans;
begin
  FTransaction.StartTransaction;
end;

procedure TJDODataBase.Commit;
begin
  FTransaction.Commit;
end;

procedure TJDODataBase.Rollback;
begin
  FTransaction.Rollback;
end;

{ TJDOQuery }

constructor TJDOQuery.Create(ADataBase: TJDODataBase;
  const ATableName: string);
begin
  FItems := TObjectList.Create(True);
  FFields := TJSONObject.Create;
  FSQL := TStringList.Create;
  FAdditionalSQL := TStringList.Create;
  FDataBase := ADataBase;
  FTableName := ATableName;
  FLastSQLOperation := soNone;
  FDateAsString := True;
  FPrimaryKey := JDO_DEFAULT_PRIMARY_KEY;
  FOrderBy := True;
end;

constructor TJDOQuery.Create;
begin
  Create(FDataBase, FTableName);
end;

destructor TJDOQuery.Destroy;
begin
  FAdditionalSQL.Free;
  FSQL.Free;
  FFields.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TJDOQuery.Prepare(const ASQLOperation: TJDOQuerySQLOperation;
  const AAdditionalSQL: string);

  function _SQLSet(const Token, PK: string;
    const SkipPK, Pairs: Boolean): string;
  var
    FN: string;
    I, C: Integer;
  begin
    C := FFields.Count;
    for I := 0 to Pred(C) do
    begin
      FN := FFields.Names[I];
      if SkipPK and (FN = PK) then
        Continue;
      if Pairs then
      begin
        Result += FN + EQ + Token + FN;
        if Succ(I) < C then
          Result += CS;
      end
      else
      begin
        Result += Token + FN;
        if Succ(I) < C then
          Result += CS;
      end;
    end;
    if Result = ES then
    begin
      if FTableAlias <> ES then
        Result := FTableAlias + DT + AK
      else
        Result := AK;
    end;
  end;

var
  VSQL: string;
begin
  if FSQL.Count > 0 then
  begin
    FLastSQLOperation := soNone;
    FDataBase.SQL.Assign(FSQL);
    FDataBase.SQL.AddStrings(FAdditionalSQL);
    Exit;
  end;
  case ASQLOperation of
    soSelect:
      begin
        VSQL := JDO_SQL_SELECT_TOKEN + _SQLSet(ES, FPrimaryKey, False, False) +
          JDO_SQL_FROM_TOKEN + FTableName;
        if FLike <> ES then
          VSQL += JDO_SQL_WHERE_TOKEN + FLike;
        if AAdditionalSQL <> ES then
          VSQL += SP + AAdditionalSQL;
        if FAdditionalSQL.Count > 0 then
          VSQL += SP + FAdditionalSQL.Text;
        if FOrderBy then
        begin
          if FTableAlias <> ES then
            VSQL += JDO_SQL_ORDER_BY_TOKEN + FTableAlias + DT + FPrimaryKey
          else
            VSQL += JDO_SQL_ORDER_BY_TOKEN + FPrimaryKey;
        end;
        FDataBase.Prepare(VSQL);
      end;
    soInsert: FDataBase.Prepare(JDO_SQL_INSERT_TOKEN + FTableName +
      SP + PS + _SQLSet(ES, FPrimaryKey, False, False) + PE +
      JDO_SQL_VALUES_TOKEN + PS + _SQLSet(CO, FPrimaryKey, False, False) + PE);
    soUpdate:
      begin
        if Trim(FPrimaryKey) = ES then
          raise EJDOQuery.Create(SJDOPrimaryKeyEmptyError);
        FDataBase.Prepare(JDO_SQL_UPDATE_TOKEN + FTableName +
          JDO_SQL_SET_TOKEN + _SQLSet(CO, FPrimaryKey, True, True) +
          JDO_SQL_WHERE_TOKEN + FPrimaryKey + JDO_SQL_EQ_PARAM_TOKEN +
          FPrimaryKey);
      end;
    soDelete:
      begin
        if Trim(FPrimaryKey) = ES then
          raise EJDOQuery.Create(SJDOPrimaryKeyEmptyError);
        FDataBase.Prepare(JDO_SQL_DELETE_TOKEN + JDO_SQL_FROM_TOKEN +
          FTableName + JDO_SQL_WHERE_TOKEN + FPrimaryKey +
          JDO_SQL_EQ_PARAM_TOKEN + FPrimaryKey);
      end;
  end;
  FLastSQLOperation := ASQLOperation;
end;

function TJDOQuery.GetItems(AIndex: Integer): TJSONObject;
begin
  Result := FItems[AIndex] as TJSONObject;
end;

procedure TJDOQuery.SetItems(AIndex: Integer; const AValue: TJSONObject);
begin
  FItems[AIndex] := AValue;
end;

procedure TJDOQuery.AddField(const AFieldName: ShortString;
  const AFieldType: TJDOFieldTypes; const AIsPK: Boolean);
var
  VFieldName: string;
begin
  if AIsPK then
    FPrimaryKey := AFieldName;
  if (FTableAlias <> ES) and (Pos(DT, AFieldName) = 0) then
    VFieldName := FTableAlias + DT + AFieldName
  else
    VFieldName := AFieldName;
  case AFieldType of
    ftNull: FFields.Add(VFieldName, JDO_FT_NULL);
    ftStr: FFields.Add(VFieldName, JDO_FT_STR);
    ftBool: FFields.Add(VFieldName, JDO_FT_BOOL);
    ftDate: FFields.Add(VFieldName, JDO_FT_DATE);
    ftFloat: FFields.Add(VFieldName, JDO_FT_FLOAT);
    ftInt: FFields.Add(VFieldName, JDO_FT_INT);
  end;
end;

procedure TJDOQuery.Like(const AValue, AKey: string;
  const AOptions: TJDOLikeOptions);
begin
  if loPartialKey in AOptions then
    FLikeValue := AnsiQuotedStr(AValue, '%')
  else
    FLikeValue := AValue;
  FLikeKey := AKey;
  if loCaseInsensitive in AOptions then
  begin
    FLikeValue := LowerCase(FLikeValue);
    FLike := JDO_SQL_LOWER_TOKEN + PS + FLikeKey + PE + JDO_SQL_LIKE_TOKEN +
      AKey + PE;
  end
  else
    FLike := FLikeKey + JDO_SQL_LIKE_TOKEN + AKey + PE;
end;

function TJDOQuery.Insert(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soInsert then
    Prepare(soInsert);
  JDOJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TJDOQuery.Insert(AJSONArray: TJSONArray): Boolean;
var
  I: Integer;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soInsert then
    Prepare(soInsert);
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    VJSONObject := AJSONArray[I] as TJSONObject;
    JDOJSONObjectToParams(FDataBase.Params, FFields, VJSONObject, FPrimaryKey);
    Result := FDataBase.Execute;
  end;
end;

function TJDOQuery.Update(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soUpdate then
    Prepare(soUpdate);
  JDOJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TJDOQuery.Update(AJSONArray: TJSONArray): Boolean;
var
  I: Integer;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soUpdate then
    Prepare(soUpdate);
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    VJSONObject := AJSONArray[I] as TJSONObject;
    JDOJSONObjectToParams(FDataBase.Params, FFields, VJSONObject, FPrimaryKey);
    Result := FDataBase.Execute;
  end;
end;

function TJDOQuery.Delete(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soDelete then
    Prepare(soDelete);
  JDOJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TJDOQuery.Delete(AJSONArray: TJSONArray): Boolean;
var
  I, VCount: Integer;
  VData: TJSONData;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soDelete then
    Prepare(soDelete);
  VCount := AJSONArray.Count;
  if VCount > 0 then
    VData := AJSONArray[0];
  case VData.JSONType of
    jtNumber:
      for I := 0 to Pred(VCount) do
      begin
        FDataBase.Param(FPrimaryKey).AsInteger := AJSONArray[I].AsInt64;
        Result := FDataBase.Execute;
      end;
    jtObject:
      for I := 0 to Pred(VCount) do
      begin
        VJSONObject := AJSONArray[I] as TJSONObject;
        JDOJSONObjectToParams(FDataBase.Params, FFields, VJSONObject,
          FPrimaryKey);
        Result := FDataBase.Execute;
      end;
  end;
end;

function TJDOQuery.Open(const AAdditionalSQL: string): Boolean;
var
  I: Integer;
  VField: TField;
  VFieldName: string;
  VItem: TJSONObject;
begin
  if FLastSQLOperation <> soSelect then
    Prepare(soSelect, AAdditionalSQL);
  if FLike <> ES then
    Param(FLikeKey).AsString := FLikeValue;
  Result := FDataBase.Open;
  FItems.Clear;
  FDataBase.Query.First;
  if Pos(AK, FDataBase.Query.SQL.Text) <> 0 then
  begin
    FDataBase.Query.First;
    while not FDataBase.Query.EOF do
    begin
      VItem := TJSONObject.Create;
      for I := 0 to Pred(FDataBase.Fields.Count) do
      begin
        VField := FDataBase.Fields[I];
        VFieldName := VField.FieldName;
        case VField.DataType of
          ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
            ftInterface, ftIDispatch: VItem.Add(VFieldName);
          ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
            ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
            ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftVariant, ftGuid:
            VItem.Add(VFieldName, VField.AsString);
          ftSmallint, ftInteger, ftLargeint, ftAutoInc:
            VItem.Add(VFieldName, VField.AsInteger);
          ftBoolean: VItem.Add(VFieldName, VField.AsBoolean);
          DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd:
            VItem.Add(VFieldName, VField.AsFloat);
          DB.ftDate, ftTime, ftDateTime, ftTimeStamp:
            if FDateAsString then
              VItem.Add(VFieldName, VField.AsString)
            else
              VItem.Add(VFieldName, VField.AsDateTime);
        end;
      end;
      FItems.Add(VItem);
      FDataBase.Query.Next;
    end;
  end
  else
    while not FDataBase.Query.EOF do
    begin
      VItem := TJSONObject.Create;
      JDOFieldsToJSONObject(FDataBase.Query.Fields, FFields, VItem,
        FDateAsString);
      FItems.Add(VItem);
      FDataBase.Query.Next;
    end;
end;

function TJDOQuery.Count: Integer;
begin
  Result := FItems.Count;
end;

function TJDOQuery.First: TJSONObject;
begin
  Result := FItems.First as TJSONObject;
end;

function TJDOQuery.Last: TJSONObject;
begin
  Result := FItems.Last as TJSONObject;
end;

function TJDOQuery.AsJSON: TJSONStringType;
var
  I, C: Integer;
begin
  C := FItems.Count;
  for I := 0 to Pred(C) do
  begin
    Result += TJSONObject(FItems[I]).AsJSON;
    if Succ(I) < C then
      Result += CS;
  end;
  Result := BS + Result + BE;
end;

function TJDOQuery.Field(const AFieldName: string): TField;
begin
  Result := FDataBase.Query.Fields.FieldByName(AFieldName);
end;

function TJDOQuery.Param(const AParamName: string): TParam;
begin
  Result := FDataBase.Query.Params.ParamByName(AParamName);
end;

end.

