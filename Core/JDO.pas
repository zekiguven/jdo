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
  JDOConsts, Classes, SysUtils, SQLdb, DB, TypInfo, Contnrs, FPJSON, FGL;

type
  EJDOException = class(Exception);

  EJDODataBase = class(EJDOException);

  EJDOQuery = class(EJDOException);

  TJDOFieldTypes = (ftNull, ftStr, ftBool, ftDate, ftFloat, ftInt);

  TJDOSQLOperation = (soNone, soSelect, soInsert, soUpdate, soDelete);

  TJDOLikeOptions = set of (loCaseInsensitive, loPartialKey);

  TJDOQueryNotifyTypes = (ntNone, ntInsert, ntUpdate, ntDelete, ntOpen, ntFirst,
    ntLast, ntClear);

  TJDOQueryNotifyEvent = procedure(
    const ANotifyType: TJDOQueryNotifyTypes) of object;

  TJDOQueryAddingItemsEvent = procedure(
    AItem: TJSONObject; const AItemNo: Integer) of object;

  TJDOSQLConnection = class(TSQLConnection)
  end;

  TJDOSQLConnectionClass = class of TJDOSQLConnection;

  TJDOSQLTransaction = class(TSQLTransaction)
  public
    procedure StartTrans(const ANativeError: Boolean = True);
    procedure RestartTrans;
  end;

  TJDOSQLTransactionClass = class of TJDOSQLTransaction;

  TJDOSQLQuery = class(TSQLQuery)
  private
    FDateAsString: Boolean;
  public
    function Open: Boolean;
    function Execute: Boolean;
    function FieldType(const AFieldType: TFieldType): ShortString;
    function FieldTypeEnum(const AFieldType: TFieldType): TJDOFieldTypes;
    procedure ReadFields(AJSONFiels, AJSONObject: TJSONObject);
    procedure WriteParams(AJSONFiels, AJSONObject: TJSONObject;
      const APrimaryKey: string = ES);
    property DateAsString: Boolean read FDateAsString write FDateAsString;
  end;

  TSQLQueryClass = class of TJDOSQLQuery;

  TJDOQuery = class;

  TJDOQueries = specialize TFPGObjectList<TJDOQuery>;

  TJDODataBase = class
  private
    FConfig: TStrings;
    FConfigFileName: TFileName;
    FConnection: TJDOSQLConnection;
    FFields: TFields;
    FOnCommit: TNotifyEvent;
    FOnExecute: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnPrepare: TNotifyEvent;
    FOnRestartTrans: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnStartTrans: TNotifyEvent;
    FParams: TParams;
    FQueries: TJDOQueries;
    FQuery: TJDOSQLQuery;
    FSQL: TStringList;
    FTransaction: TJDOSQLTransaction;
    function GetQueries: TJDOQueries;
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
    procedure StartTrans(const ANativeError: Boolean = True);
    procedure RestartTrans;
    procedure Commit;
    procedure Rollback;
    property Config: TStrings read FConfig;
    property ConfigFileName: TFileName read FConfigFileName write FConfigFileName;
    property Connection: TJDOSQLConnection read FConnection;
    property Transaction: TJDOSQLTransaction read FTransaction;
    property Query: TJDOSQLQuery read FQuery;
    property Queries: TJDOQueries read GetQueries;
    property SQL: TStringList read FSQL;
    property Fields: TFields read FFields;
    property Params: TParams read FParams;
    property OnPrepare: TNotifyEvent read FOnPrepare write FOnPrepare;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnStartTrans: TNotifyEvent read FOnStartTrans write FOnStartTrans;
    property OnRestartTrans: TNotifyEvent read FOnRestartTrans write FOnRestartTrans;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
  end;

  TJDODataBaseClass = class of TJDODataBase;

  TJDOQuery = class
  private
    FFreeObjects: Boolean;
    FIsCustomSQL: Boolean;
    FAdditionalSQL: TStrings;
    FLike: string;
    FLikeKey: string;
    FLikeValue: string;
    FLastSQLOperation: TJDOSQLOperation;
    FDataBase: TJDODataBase;
    FFields: TJSONObject;
    FItems: TObjectList;
    FOnAddingItems: TJDOQueryAddingItemsEvent;
    FOnNotify: TJDOQueryNotifyEvent;
    FOnPrepare: TNotifyEvent;
    FOrderBy: string;
    FPrimaryKey: string;
    FQuery: TJDOSQLQuery;
    FSQLOperation: TJDOSQLOperation;
    FTableAlias: string;
    FTableName: string;
    function GetAdditionalSQL: TStrings;
    function GetDateAsString: Boolean;
    function GetIsPrepared: Boolean;
    function GetItems(AIndex: Integer): TJSONObject;
    function GetSQL: TStrings;
    procedure SetDataBase(const AValue: TJDODataBase);
    procedure SetDateAsString(const AValue: Boolean);
    procedure SetItems(AIndex: Integer; const AValue: TJSONObject);
  public
    constructor Create;
    constructor Create(ADataBase: TJDODataBase; const ATableName: string);
    destructor Destroy; override;
    procedure Prepare(const ASQLOperation: TJDOSQLOperation;
      const AAdditionalSQL: string = ES); virtual;
    procedure AddField(const AFieldName: ShortString;
      const AFieldType: TJDOFieldTypes;
      const APrimaryKey: Boolean = False);
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
    procedure Clear;
    function First: TJSONObject;
    function Last: TJSONObject;
    function AsJSON: TJSONStringType;
    function AsJSONArray: TJSONArray;
    function Field(const AFieldName: string): TField;
    function Param(const AParamName: string): TParam;
    property DataBase: TJDODataBase read FDataBase write SetDataBase;
    property Query: TJDOSQLQuery read FQuery write FQuery;
    property Items[AIndex: Integer]: TJSONObject read GetItems
      write SetItems; default;
    property Fields: TJSONObject read FFields;
    property FreeObjects: Boolean read FFreeObjects write FFreeObjects;
    property TableName: string read FTableName write FTableName;
    property TableAlias: string read FTableAlias write FTableAlias;
    property PrimaryKey: string read FPrimaryKey write FPrimaryKey;
    property SQL: TStrings read GetSQL;
    property AdditionalSQL: TStrings read GetAdditionalSQL;
    property OrderBy: string read FOrderBy write FOrderBy;
    property IsPrepared: Boolean read GetIsPrepared;
    property DateAsString: Boolean read GetDateAsString write SetDateAsString;
    property SQLOperation: TJDOSQLOperation read FSQLOperation;
    property OnAddingItems: TJDOQueryAddingItemsEvent read FOnAddingItems
      write FOnAddingItems;
    property OnPrepare: TNotifyEvent read FOnPrepare write FOnPrepare;
    property OnNotify: TJDOQueryNotifyEvent read FOnNotify write FOnNotify;
  end;

  TJDOQueryClass = class of TJDOQuery;

implementation

{ TJDOSQLTransaction }

procedure TJDOSQLTransaction.StartTrans(const ANativeError: Boolean);
begin
  if (not ANativeError) and Active then
    Exit;
  StartTransaction;
end;

procedure TJDOSQLTransaction.RestartTrans;
begin
  if Active then
    Rollback;
  StartTransaction;
end;

{ TJDOSQLQuery }

function TJDOSQLQuery.Open: Boolean;
begin
  inherited Open;
  Result := RecordCount > 0;
end;

function TJDOSQLQuery.Execute: Boolean;
begin
  ExecSQL;
  Result := RowsAffected > 0;
end;

function TJDOSQLQuery.FieldType(const AFieldType: TFieldType): ShortString;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftInterface, ftIDispatch: Result := FT_NULL;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob, ftOraClob,
      ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftVariant,
      ftGuid: Result := FT_STR;
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc: Result := FT_INT;
    ftBoolean: Result := FT_BOOL;
    DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := FT_FLOAT;
    DB.ftDate, ftTime, ftDateTime, ftTimeStamp: Result := FT_DATE;
  end;
end;

function TJDOSQLQuery.FieldTypeEnum(const AFieldType: TFieldType): TJDOFieldTypes;
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

procedure TJDOSQLQuery.ReadFields(AJSONFiels, AJSONObject: TJSONObject);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(Fields.Count) do
  begin
    VField := Fields[I];
    if AJSONFiels.Count > 0 then
    begin
      VFieldType := AJSONFiels.Items[I].AsString;
      VFieldName := AJSONFiels.Names[I];
    end
    else
    begin
      VFieldType := FieldType(VField.DataType);
      VFieldName := VField.FieldName;
    end;
    if (VFieldType = FT_NULL) or VField.IsNull then
    begin
      AJSONObject.Add(VFieldName);
      Continue;
    end;
    if VFieldType = FT_STR then
      AJSONObject.Add(VFieldName, VField.AsString);
    if VFieldType = FT_BOOL then
      AJSONObject.Add(VFieldName, VField.AsBoolean);
    if VFieldType = FT_DATE then
    begin
      if FDateAsString then
        AJSONObject.Add(VFieldName, VField.AsString)
      else
        AJSONObject.Add(VFieldName, VField.AsFloat);
    end;
    if VFieldType = FT_FLOAT then
      AJSONObject.Add(VFieldName, VField.AsFloat);
    if VFieldType = FT_INT then
      AJSONObject.Add(VFieldName, VField.AsInteger);
  end;
end;

procedure TJDOSQLQuery.WriteParams(AJSONFiels, AJSONObject: TJSONObject;
  const APrimaryKey: string);
var
  VParam: TParam;
  VField, VData: TJSONData;
  VFieldType, VName: ShortString;
  I, VJSONFielsCount, VJSONObjsCount: Integer;
begin
  VJSONObjsCount := AJSONObject.Count;
  VJSONFielsCount := AJSONFiels.Count;
  if VJSONFielsCount <> VJSONObjsCount then
    raise EJDOException.CreateFmt(SJSONObjectToParamsError,
      [VJSONFielsCount, VJSONObjsCount]);
  for I := 0 to Pred(VJSONObjsCount) do
  begin
    VName := AJSONFiels.Names[I];
    VField := AJSONFiels.Items[I];
    VParam := Params.ParamByName(VName);
    VData := AJSONObject[VName];
    if (APrimaryKey <> ES) and (APrimaryKey = VName) and
      (not VData.IsNull) and Assigned(VParam) then
    begin
      VParam.AsInteger := VData.AsInt64;
      Continue;
    end;
    VFieldType := VField.AsString;
    if (VFieldType = FT_NULL) or VData.IsNull or VField.IsNull then
    begin
      Params.Clear;
      Continue;
    end;
    if VFieldType = FT_STR then
      VParam.AsString := VData.AsString;
    if VFieldType = FT_BOOL then
      VParam.AsBoolean := (VData.AsString = 'on') or VData.AsBoolean;
    if VFieldType = FT_DATE then
    begin
      if VData.JSONType = jtNumber then
        VParam.AsDateTime := VData.AsFloat
      else
        VParam.AsDateTime := StrToDateTime(VData.AsString);
    end;
    if VFieldType = FT_FLOAT then
      VParam.AsFloat := VData.AsFloat;
    if VFieldType = FT_INT then
      VParam.AsInteger := VData.AsInteger;
  end;
end;

{ TJDODataBase }

constructor TJDODataBase.Create(const AConfigFileName: TFileName;
  const AConnect: Boolean);
begin
  FConfig := TStringList.Create;
  FConfigFileName := AConfigFileName;
  LoadConfig;
  InternalCreateConnection;
  InternalCreateTransaction;
  InternalCreateQuery;
  SetProperties;
  if AConnect then
    FConnection.Open;
end;

destructor TJDODataBase.Destroy;
begin
  FConfig.Free;
  FQuery.Free;
  FQueries.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TJDODataBase.InternalCreateConnection;
var
  VConnectorType: ShortString;
  VConnectionDef: TConnectionDef;
begin
  VConnectorType := FConfig.Values[CONNECTOR_TYPE];
  if Trim(VConnectorType) = ES then
    raise EJDODataBase.Create(SEmptyConnectorTypeError);
  VConnectionDef := GetConnectionDef(VConnectorType);
  if Assigned(VConnectionDef) then
    FConnection := TJDOSQLConnectionClass(
      VConnectionDef.ConnectionClass).Create(nil)
  else
    raise EJDODataBase.CreateFmt(
      SConnectorUnitWasNotDeclaredError, [VConnectorType]);
end;

function TJDODataBase.GetQueries: TJDOQueries;
begin
  if not Assigned(FQueries) then
    FQueries := TJDOQueries.Create;
  Result := FQueries;
end;

procedure TJDODataBase.InternalCreateTransaction;
begin
  FTransaction := TJDOSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
end;

procedure TJDODataBase.InternalCreateQuery;
begin
  FQuery := TJDOSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
  FSQL := FQuery.SQL;
  FFields := FQuery.Fields;
  FParams := FQuery.Params;
end;

procedure TJDODataBase.LoadConfig;
begin
  if not FileExists(FConfigFileName) then
    raise EJDODataBase.CreateFmt(SConfigFileNotFoundError, [FConfigFileName]);
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
    if (CompareText(VPropName, CONNECTOR_TYPE) = 0) or (VToken = PO) or
      (VToken = ES) then
      Continue;
    if IsPublishedProp(FConnection, VPropName) then
      SetPropValue(FConnection, VPropName, FConfig.Values[VPropName])
    else
      raise EJDODataBase.CreateFmt(SInvalidPropInConfigFile,
        [ExtractFileName(FConfigFileName), VPropName]);
  end;
end;

procedure TJDODataBase.Prepare(const ASQL: string);
begin
  FQuery.SQL.Text := ASQL;
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
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
  Result := FQuery.Open;
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

function TJDODataBase.Execute: Boolean;
begin
  Result := FQuery.Execute;
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TJDODataBase.StartTrans(const ANativeError: Boolean);
begin
  FTransaction.StartTrans(ANativeError);
  if Assigned(FOnStartTrans) then
    FOnStartTrans(Self);
end;

procedure TJDODataBase.RestartTrans;
begin
  FTransaction.RestartTrans;
  if Assigned(FOnRestartTrans) then
    FOnRestartTrans(Self);
end;

procedure TJDODataBase.Commit;
begin
  FTransaction.Commit;
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

procedure TJDODataBase.Rollback;
begin
  FTransaction.Rollback;
  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

{ TJDOQuery }

constructor TJDOQuery.Create(ADataBase: TJDODataBase;
  const ATableName: string);
begin
  FQuery := TJDOSQLQuery.Create(nil);
  FItems := TObjectList.Create(True);
  FFields := TJSONObject.Create;
  SetDataBase(ADataBase);
  FPrimaryKey := DEFAULT_PRIMARY_KEY;
  FOrderBy := DEFAULT_PRIMARY_KEY;
  FQuery.DateAsString := True;
  FFreeObjects := True;
  FTableName := ATableName;
end;

constructor TJDOQuery.Create;
begin
  Create(nil, ES);
end;

destructor TJDOQuery.Destroy;
begin
  FAdditionalSQL.Free;
  FFields.Free;
  FItems.Free;
  FQuery.Free;
  inherited Destroy;
end;

procedure TJDOQuery.Prepare(const ASQLOperation: TJDOSQLOperation;
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
  if FIsCustomSQL then
  begin
    FLastSQLOperation := soNone;
    FSQLOperation := soNone;
    Exit;
  end;
  FSQLOperation := ASQLOperation;
  case ASQLOperation of
    soSelect:
      begin
        VSQL := SQL_SELECT_TOKEN + _SQLSet(ES, FPrimaryKey, False, False) +
          SQL_FROM_TOKEN + FTableName;
        if FLike <> ES then
          VSQL += SQL_WHERE_TOKEN + FLike;
        if AAdditionalSQL <> ES then
          VSQL += SP + AAdditionalSQL;
        if Assigned(FAdditionalSQL) and (FAdditionalSQL.Count > 0) then
          VSQL += SP + FAdditionalSQL.Text;
        if FOrderBy <> ES then
        begin
          if FTableAlias <> ES then
            VSQL += SQL_ORDER_BY_TOKEN + FTableAlias + DT + FOrderBy
          else
            VSQL += SQL_ORDER_BY_TOKEN + FOrderBy;
        end;
        FQuery.SQL.Text := VSQL;
      end;
    soInsert: FQuery.SQL.Text := SQL_INSERT_TOKEN + FTableName +
      SP + PS + _SQLSet(ES, FPrimaryKey, False, False) + PE +
      SQL_VALUES_TOKEN + PS + _SQLSet(CO, FPrimaryKey, False, False) + PE;
    soUpdate:
      begin
        if Trim(FPrimaryKey) = ES then
          raise EJDOQuery.Create(SEmptyPrimaryKeyError);
        FQuery.SQL.Text := SQL_UPDATE_TOKEN + FTableName + SQL_SET_TOKEN +
          _SQLSet(CO, FPrimaryKey, True, True) + SQL_WHERE_TOKEN + FPrimaryKey +
          SQL_EQ_PARAM_TOKEN + FPrimaryKey;
      end;
    soDelete:
      begin
        if Trim(FPrimaryKey) = ES then
          raise EJDOQuery.Create(SEmptyPrimaryKeyError);
        FQuery.SQL.Text := SQL_DELETE_TOKEN + SQL_FROM_TOKEN + FTableName +
          SQL_WHERE_TOKEN + FPrimaryKey + SQL_EQ_PARAM_TOKEN + FPrimaryKey;
      end;
  end;
  FLastSQLOperation := ASQLOperation;
  if Assigned(FOnPrepare) then
    FOnPrepare(Self);
end;

function TJDOQuery.GetItems(AIndex: Integer): TJSONObject;
begin
  Result := FItems[AIndex] as TJSONObject;
end;

function TJDOQuery.GetAdditionalSQL: TStrings;
begin
  if not Assigned(FAdditionalSQL) then
    FAdditionalSQL := TStringList.Create;
  Result := FAdditionalSQL;
end;

function TJDOQuery.GetDateAsString: Boolean;
begin
  Result := FQuery.DateAsString;
end;

function TJDOQuery.GetIsPrepared: Boolean;
begin
  Result := FQuery.SQL.Text <> ES;
end;

function TJDOQuery.GetSQL: TStrings;
begin
  FIsCustomSQL := True;
  Result := FQuery.SQL;
end;

procedure TJDOQuery.SetDataBase(const AValue: TJDODataBase);
begin
  FDataBase := AValue;
  FQuery.DataBase := nil;
  FQuery.Transaction := nil;
  if Assigned(AValue) then
  begin
    if FDataBase.Queries.IndexOf(Self) = -1 then
      FDataBase.Queries.Add(Self);
    FQuery.DataBase := FDataBase.Connection;
    FQuery.Transaction := FDataBase.Transaction;
  end;
end;

procedure TJDOQuery.SetDateAsString(const AValue: Boolean);
begin
  FQuery.DateAsString := AValue;
end;

procedure TJDOQuery.SetItems(AIndex: Integer; const AValue: TJSONObject);
begin
  FItems[AIndex] := AValue;
end;

procedure TJDOQuery.AddField(const AFieldName: ShortString;
  const AFieldType: TJDOFieldTypes; const APrimaryKey: Boolean);
var
  VFieldName: string;
begin
  if APrimaryKey then
    FPrimaryKey := AFieldName;
  if (FTableAlias <> ES) and (Pos(DT, AFieldName) = 0) then
    VFieldName := FTableAlias + DT + AFieldName
  else
    VFieldName := AFieldName;
  case AFieldType of
    ftNull: FFields.Add(VFieldName, FT_NULL);
    ftStr: FFields.Add(VFieldName, FT_STR);
    ftBool: FFields.Add(VFieldName, FT_BOOL);
    ftDate: FFields.Add(VFieldName, FT_DATE);
    ftFloat: FFields.Add(VFieldName, FT_FLOAT);
    ftInt: FFields.Add(VFieldName, FT_INT);
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
    FLike := SQL_LOWER_TOKEN + PS + FLikeKey + PE + SQL_LIKE_TOKEN + AKey + PE;
  end
  else
    FLike := FLikeKey + SQL_LIKE_TOKEN + AKey + PE;
end;

function TJDOQuery.Insert(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soInsert then
    Prepare(soInsert);
  FQuery.WriteParams(FFields, AJSONObject);
  Result := FQuery.Execute;
  if FFreeObjects then
    FreeAndNil(AJSONObject);
  if Assigned(FOnNotify) then
    FOnNotify(ntInsert);
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
    FQuery.WriteParams(FFields, VJSONObject, FPrimaryKey);
    Result := FQuery.Execute;
  end;
  if FFreeObjects then
    FreeAndNil(AJSONArray);
  if Assigned(FOnNotify) then
    FOnNotify(ntInsert);
end;

function TJDOQuery.Update(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soUpdate then
    Prepare(soUpdate);
  FQuery.WriteParams(FFields, AJSONObject);
  Result := FQuery.Execute;
  if FFreeObjects then
    FreeAndNil(AJSONObject);
  if Assigned(FOnNotify) then
    FOnNotify(ntUpdate);
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
    FQuery.WriteParams(FFields, VJSONObject, FPrimaryKey);
    Result := FQuery.Execute;
  end;
  if FFreeObjects then
    FreeAndNil(AJSONArray);
  if Assigned(FOnNotify) then
    FOnNotify(ntUpdate);
end;

function TJDOQuery.Delete(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soDelete then
    Prepare(soDelete);
{$IFDEF JDO_DELETE_WITH_VARIANTS}
  FQuery.Params.ParamByName(FPrimaryKey).Value :=
    AJSONObject[FPrimaryKey].Value;
{$ELSE}
  FQuery.Params.ParamByName(FPrimaryKey).AsInteger :=
    AJSONObject[FPrimaryKey].AsInt64;
{$ENDIF}
  Result := FQuery.Execute;
  if FFreeObjects then
    FreeAndNil(AJSONObject);
  if Assigned(FOnNotify) then
    FOnNotify(ntDelete);
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
{$IFDEF JDO_DELETE_WITH_VARIANTS}
        FQuery.Params.ParamByName(FPrimaryKey).Value := AJSONArray[I].Value;
{$ELSE}
        FQuery.Params.ParamByName(FPrimaryKey).AsInteger := AJSONArray[I].AsInt64;
{$ENDIF}
        Result := FQuery.Execute;
      end;
    jtObject:
      for I := 0 to Pred(VCount) do
      begin
        VJSONObject := AJSONArray[I] as TJSONObject;
{$IFDEF JDO_DELETE_WITH_VARIANTS}
        FQuery.Params.ParamByName(FPrimaryKey).Value :=
          VJSONObject[FPrimaryKey].Value;
{$ELSE}
        FQuery.Params.ParamByName(FPrimaryKey).AsInteger :=
          VJSONObject[FPrimaryKey].AsInt64;
{$ENDIF}
        Result := FQuery.Execute;
      end;
  end;
  if FFreeObjects then
    FreeAndNil(AJSONArray);
  if Assigned(FOnNotify) then
    FOnNotify(ntDelete);
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
  Result := FQuery.Open;
  FItems.Clear;
  FQuery.First;
  if Pos(AK, FQuery.SQL.Text) <> 0 then
  begin
    FQuery.First;
    while not FQuery.EOF do
    begin
      VItem := TJSONObject.Create;
      for I := 0 to Pred(FQuery.Fields.Count) do
      begin
        VField := FQuery.Fields[I];
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
            if FQuery.DateAsString then
              VItem.Add(VFieldName, VField.AsString)
            else
              VItem.Add(VFieldName, VField.AsDateTime);
        end;
      end;
      FItems.Add(VItem);
      if Assigned(FOnAddingItems) then
        FOnAddingItems(VItem, FQuery.RecNo);
      FQuery.Next;
    end;
  end
  else
    while not FQuery.EOF do
    begin
      VItem := TJSONObject.Create;
      FQuery.DateAsString := FQuery.DateAsString;
      FQuery.ReadFields(FFields, VItem);
      FItems.Add(VItem);
      if Assigned(FOnAddingItems) then
        FOnAddingItems(VItem, FQuery.RecNo);
      FQuery.Next;
    end;
  if Assigned(FOnNotify) then
    FOnNotify(ntOpen);
end;

function TJDOQuery.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TJDOQuery.Clear;
begin
  FIsCustomSQL := False;
  FQuery.SQL.Clear;
  FItems.Clear;
  FFields.Clear;
  if Assigned(FAdditionalSQL) then
    FAdditionalSQL.Clear;
  FLastSQLOperation := soNone;
  if Assigned(FOnNotify) then
    FOnNotify(ntClear);
end;

function TJDOQuery.First: TJSONObject;
begin
  Result := FItems.First as TJSONObject;
  if Assigned(FOnNotify) then
    FOnNotify(ntFirst);
end;

function TJDOQuery.Last: TJSONObject;
begin
  Result := FItems.Last as TJSONObject;
  if Assigned(FOnNotify) then
    FOnNotify(ntLast);
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

function TJDOQuery.AsJSONArray: TJSONArray;
var
  I: Integer;
  A: TJSONArray;
begin
  A := TJSONArray.Create;
  for I := 0 to Pred(FItems.Count) do
    A.Add((FItems[I] as TJSONObject).Clone);
  Result := A;
end;

function TJDOQuery.Field(const AFieldName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AFieldName);
end;

function TJDOQuery.Param(const AParamName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AParamName);
end;

end.

