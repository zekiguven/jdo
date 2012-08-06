(*
  JSON Data Objects unit
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo/

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDO;

{$I jdo.inc}

interface

uses
  JDOConsts, JDOUtils, Classes, SysUtils, SQLdb, DB, TypInfo, Variants,
  FPJSON, JSONParser, DBConst;

type
  TJDOPutTypes = (ptBegin, ptMiddle, ptEnd);

  TJDOStatementType = (jstUnknown, jstSelect, jstInsert, jstUpdate, jstDelete);

  TJDOComposeEvent = procedure(const AStatementType: TJDOStatementType) of object;

  IJDOAboutComponent = interface
    ['{05155F56-E531-4284-AA35-9E78C18A5773}']
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    property About: string read GetAbout write SetAbout stored False;
  end;

  { EJDO }

  EJDO = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string);
    constructor CreateFmt(AInstance: TObject; const AMsg: string;
      const AArgs: array of const);
  end;

  EJDOConfig = class(EJDO);

  EJDOSQL = class(EJDO);

  EJDOQuery = class(EJDO);

  EJDODataBase = class(EJDO);

  TJDOCustomConfiguratorClass = class of TJDOCustomConfigurator;

  TJDOConfiguratorClass = class of TJDOConfigurator;

  TJDOCustomSQLClass = class of TJDOCustomSQL;

  TJDOSQLClass = class of TJDOSQL;

  TJDOCustomQueryClass = class of TJDOCustomQuery;

  TJDOQueryClass = class of TJDOQuery;

  TJDOCustomDataBaseClass = class of TJDOCustomDataBase;

  TJDODataBaseClass = class of TJDODataBase;

  { TJDOCustomConfigurator }

  TJDOCustomConfigurator = class(TComponent, IJDOAboutComponent)
  private
{$IFDEF JDO_CRYPT}
    FCryptKey: string;
{$ENDIF}
    FConfiguration: string;
    FConfigFile: TStrings;
    FTarget: TComponent;
    function GetAbout: string;
    function GetValues(AName: string): string;
    procedure SetAbout({%H-}AValue: string);
    procedure SetConfiguration(const AValue: string);
    procedure SetTarget(AValue: TComponent);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent;
      const AConfiguration: string); overload;
    destructor Destroy; override;
    procedure Configure;
    property About: string read GetAbout write SetAbout stored False;
{$IFDEF JDO_CRYPT}
    class function Crypt(const AStr, AKey: string): string;
    class function DeCrypt(const AStr, AKey: string): string;
    property CryptKey: string read FCryptKey write FCryptKey;
{$ENDIF}
    property Configuration: string read FConfiguration write SetConfiguration;
    property Target: TComponent read FTarget write SetTarget;
    property Values[AName: string]: string read GetValues; default;
  end;

  { TJDOConfigurator }

  TJDOConfigurator = class(TJDOCustomConfigurator)
  published
    property About;
{$IFDEF JDO_CRYPT}
    property CryptKey;
{$ENDIF}
    property Configuration;
    property Target;
  end;

  { TJDOCustomSQL }

  TJDOCustomSQL = class(TComponent, IJDOAboutComponent)
  private
    FLike: string;
    FOnCompose: TJDOComposeEvent;
    FPutBegin: string;
    FPutMiddle: string;
    FPutEnd: string;
    FOrderBy: string;
    FOrdered: Boolean;
    FQuery: TSQLQuery;
    FStatementType: TJDOStatementType;
    FTableAlias: string;
    FTableName: string;
    FWhere: string;
    function GetAbout: string;
    procedure InternalCheckFieldDefs(const ACount: Integer);
    procedure InternalCheckServerIndexDefs(const ACount: Integer);
    procedure SetAbout({%H-}AValue: string);
    procedure SetQuery(AValue: TSQLQuery);
  protected
    procedure CheckTableName;
    procedure CheckQuery;
    procedure PrepareCols(const AStatementType: TJDOStatementType;
      out ACols, AValues: string); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AQuery: TSQLQuery;
      const ATableName: string = ES); overload;
    destructor Destroy; override;
    function Compose(const AStatementType: TJDOStatementType;
      const AIntoSQL: Boolean = False): Boolean; virtual;
    procedure ComposeAll;
    procedure Reset;
    procedure Clear;
    procedure Put(const ASQL: string; const AType: TJDOPutTypes = ptMiddle;
      const ALineBreak: ShortString = SP);
    procedure Like(const AField: string; const ACaseInsensitive: Boolean = False);
    property About: string read GetAbout write SetAbout stored False;
    property OrderBy: string read FOrderBy write FOrderBy;
    property Ordered: Boolean read FOrdered write FOrdered default True;
    property Where: string read FWhere write FWhere;
    property StatementType: TJDOStatementType read FStatementType;
    property TableAlias: string read FTableAlias write FTableAlias;
    property TableName: string read FTableName write FTableName;
    property Query: TSQLQuery read FQuery write SetQuery;
    property OnCompose: TJDOComposeEvent read FOnCompose write FOnCompose;
  end;

  { TJDOSQL }

  TJDOSQL = class(TJDOCustomSQL)
  published
    property About;
    property OrderBy;
    property Ordered;
    property Where;
    property StatementType;
    property TableAlias;
    property TableName;
    property Query;
    property OnCompose;
  end;

  { TJDOCustomQuery }

  TJDOCustomQuery = class(TSQLQuery, IJDOAboutComponent)
  private
    FDateAsString: Boolean;
    FOnExecute: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    function GetAbout: string;
    function GetAsJSON: TJSONStringType;
    procedure SetAbout({%H-}AValue: string);
    procedure SetAsJSON(const AValue: TJSONStringType);
  protected
    procedure InternalCheckFieldDefs;
    procedure InternalCheckJSONParam(AJSON: TJSONData);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetJSONType(const AFieldType: TFieldType): ShortString;
    function GetPrimaryKey: TIndexDef;
    function GetJSONValuesFromPrimaryKey(const AJSON: TJSONObject): Variant;
    class procedure JSONToQuery(AJSON: TJSONObject; AQuery: TSQLQuery;
      const ADateAsString: Boolean);
    class procedure JSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet;
      const ADateAsString: Boolean);
    class procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
      const ADateAsString: Boolean);
    class procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONObject;
      const ADateAsString: Boolean);
    class procedure QueryToSchema(AQuery: TSQLQuery; ASchema: TJSONObject); overload;
    class procedure QueryToSchema(AQuery: TSQLQuery; out ASchema: TJSONStringType); overload;
    procedure GetJSON(out AJSON: TJSONArray); overload;
    procedure GetJSON(out AJSON: TJSONObject); overload;
    procedure SetJSON(AJSON: TJSONArray); overload;
    procedure SetJSON(AJSON: TJSONObject); overload;
    procedure LoadJSONFromStream(AStream: TStream);
    procedure LoadJSONFromFile(const AFileName: TFileName);
    procedure SaveJSONToStream(AStream: TStream);
    procedure SaveJSONToFile(const AFileName: TFileName);
    procedure Apply(const ARetaining: Boolean = False);
    procedure Undo(const ARetaining: Boolean = False);
    procedure Commit(const ARetaining: Boolean = False);
    procedure Rollback(const ARetaining: Boolean = False);
    function Execute: Boolean;
    function Open: Boolean;
    procedure GetSchema(out ASchema: TJSONObject); overload;
    procedure GetSchema(out ASchema: TJSONStringType); overload;
    function Field(const AFieldName: string): TField;
    function Param(const AParamName: string): TParam;
    procedure Append(AJSON: TJSONArray); overload;
    procedure Append(AJSON: TJSONObject); overload;
    procedure Insert(AJSON: TJSONArray); overload;
    procedure Insert(AJSON: TJSONObject); overload;
    procedure Edit(AJSON: TJSONArray); overload;
    procedure Edit(AJSON: TJSONObject); overload;
    procedure Delete(AJSON: TJSONArray); overload;
    procedure Delete(AJSON: TJSONObject); overload;
    property About: string read GetAbout write SetAbout stored False;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;

  { TJDOQuery }

  TJDOQuery = class(TJDOCustomQuery)
  published
    property About;
    property DateAsString;
    property OnExecute;
    property OnOpen;
  end;

  { TJDOCustomDataBase }

  TJDOCustomDataBase = class(TSQLConnector, IJDOAboutComponent)
  private
    FConfig: TJDOConfigurator;
    FQuery: TJDOQuery;
{$IFDEF JDO_CRYPT}
    FCryptKey: string;
    procedure SetCryptKey(AValue: string);
{$ENDIF}
    function GetAbout: string;
    function GetConfiguration: string;
    procedure SetAbout({%H-}AValue: string);
    procedure SetConfiguration(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    procedure Connect;
    procedure Disconnect;
    function InTransaction: Boolean;
    procedure StartTransaction(const ANativeError: Boolean); overload;
    procedure RestartTransaction;
    procedure Commit(const ARetaining: Boolean); overload;
    procedure Rollback(const ARetaining: Boolean); overload;
    property About: string read GetAbout write SetAbout stored False;
{$IFDEF JDO_CRYPT}
    property CryptKey: string read FCryptKey write SetCryptKey;
{$ENDIF}
    property Configuration: string read GetConfiguration write SetConfiguration;
    property Query: TJDOQuery read FQuery;
  end;

  { TJDODataBase }

  TJDODataBase = class(TJDOCustomDataBase)
  published
    property About;
{$IFDEF JDO_CRYPT}
    property CryptKey;
{$ENDIF}
    property Configuration;
  end;

implementation

{ EJDO }

constructor EJDO.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt(ERROR_MASK, [AInstance.ClassName, AMsg]);
end;

constructor EJDO.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(ERROR_MASK, [AInstance.ClassName, AMsg]), AArgs);
end;

{ TJDOCustomConfigurator }

constructor TJDOCustomConfigurator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigFile := TStringList.Create;
  FConfigFile.Delimiter := SC;
end;

constructor TJDOCustomConfigurator.Create(AOwner: TComponent;
  const AConfiguration: string);
begin
  Create(AOwner, AConfiguration);
end;

destructor TJDOCustomConfigurator.Destroy;
begin
  Target := nil;
  FConfigFile.Free;
  inherited Destroy;
end;

{$IFDEF JDO_CRYPT}
class function TJDOCustomConfigurator.Crypt(const AStr, AKey: string): string;
begin
  Result := StrToHex(EncryptStr(AStr, AKey));
end;

class function TJDOCustomConfigurator.DeCrypt(const AStr, AKey: string): string;
begin
  Result := DecryptStr(HexToStr(AStr), AKey);
end;
{$ENDIF}

function TJDOCustomConfigurator.GetValues(AName: string): string;
begin
  Result := FConfigFile.Values[AName];
end;

function TJDOCustomConfigurator.GetAbout: string;
begin
  Result := ES;
end;

procedure TJDOCustomConfigurator.SetAbout(AValue: string);
begin
end;

procedure TJDOCustomConfigurator.SetConfiguration(const AValue: string);
begin
  if FConfiguration <> AValue then
  begin
    FConfiguration := AValue;
    if (Pos(SC, AValue) <> 0) or (Pos(EQ, AValue) <> 0) then
      FConfigFile.DelimitedText := AValue
    else
    begin
      if Trim(AValue) = ES then
        Exit;
      if not FileExists(AValue) then
        raise EJDOConfig.CreateFmt(Self, SCfgFileNotFoundError, [AValue]);
      FConfigFile.LoadFromFile(AValue);
    end;
    Configure;
  end;
end;

procedure TJDOCustomConfigurator.SetTarget(AValue: TComponent);
begin
  if (AValue = FTarget) or (AValue = Self) then
    Exit;
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := AValue;
  if Assigned(FTarget) then
  begin
    FTarget.FreeNotification(Self);
    Configure;
  end;
end;

procedure TJDOCustomConfigurator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTarget) then
    FTarget := nil;
end;

procedure TJDOCustomConfigurator.Configure;
var
  I: Integer;
  VValue: string;
  VPropName, VToken: ShortString;
begin
  if not Assigned(FTarget) then
    Exit;
  for I := 0 to Pred(FConfigFile.Count) do
  begin
    VPropName := FConfigFile.Names[I];
    VToken := Copy(VPropName, 1, 1);
    if (VToken = PO) or (VToken = ES) then
      Continue;
    VValue := FConfigFile.Values[VPropName];
    if IsPublishedProp(FTarget, VPropName) then
{$IFDEF JDO_CRYPT}
    begin
      if FCryptKey <> ES then
        VValue := Trim(DeCrypt(VValue, FCryptKey));
{$ENDIF}
      SetPropValue(FTarget, VPropName, VValue);
{$IFDEF JDO_CRYPT}
    end;
{$ENDIF}
  end;
end;

{ TJDOCustomSQL }

constructor TJDOCustomSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrdered := True;
end;

constructor TJDOCustomSQL.Create(AOwner: TComponent;
  AQuery: TSQLQuery; const ATableName: string);
begin
  Create(AOwner);
  FQuery := AQuery;
  CheckQuery;
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

function TJDOCustomSQL.GetAbout: string;
begin
  Result := ES;
end;

procedure TJDOCustomSQL.SetAbout(AValue: string);
begin
end;

procedure TJDOCustomSQL.InternalCheckServerIndexDefs(const ACount: Integer);
begin
  if ACount = 0 then
    raise EJDOSQL.Create(Self, SEmptyServerIndexDefsCountError);
end;

procedure TJDOCustomSQL.SetQuery(AValue: TSQLQuery);
begin
  if AValue = FQuery then
    Exit;
  if Assigned(FQuery) then
    FQuery.RemoveFreeNotification(Self);
  FQuery := AValue;
  if Assigned(FQuery) then
    FQuery.FreeNotification(Self);
end;

procedure TJDOCustomSQL.PrepareCols(const AStatementType: TJDOStatementType;
  out ACols, AValues: string);

  procedure GetValues(out S: TStrings; out IxDef: TIndexDef;
    const C: Integer; const ValPK: Boolean);
  var
    I: Integer;
  begin
    S := TStringList.Create;
    S.Delimiter := SC;
    for I := 0 to Pred(C) do
    begin
      IxDef := FQuery.ServerIndexDefs[I];
      if ixPrimary in IxDef.Options then
      begin
        S.DelimitedText := IxDef.Fields;
        Exit;
      end;
    end;
    if ValPK then
      raise EJDOSQL.Create(Self, SPrimaryKeyNotFound);
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
          try
            GetValues(VValues, VIndexDef, C, False);
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
        try
          GetValues(VValues, VIndexDef, C, True);
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

function TJDOCustomSQL.Compose(const AStatementType: TJDOStatementType;
  const AIntoSQL: Boolean): Boolean;
var
  VIsWhere: Boolean;
  VCols, VValues, VSQL: string;
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
          VSQL := SQL_INSERT_TOKEN + SP + FTableName + SP + PS + VCols + PE +
            SP + SQL_VALUES_TOKEN + SP + PS + VValues + PE;
          if AIntoSQL then
            FQuery.SQL.Text := VSQL
          else
            FQuery.InsertSQL.Text := VSQL;
        end;
      jstUpdate:
        begin
          PrepareCols(jstUpdate, VCols, VValues);
          VSQL := SQL_UPDATE_TOKEN + SP + FTableName + SP + SQL_SET_TOKEN + SP +
            VCols + SP + SQL_WHERE_TOKEN + SP + VValues;
          if AIntoSQL then
            FQuery.SQL.Text := VSQL
          else
            FQuery.UpdateSQL.Text := VSQL;
        end;
      jstDelete:
        begin
          PrepareCols(jstDelete, VCols, VValues);
          VSQL := SQL_DELETE_TOKEN + SP + SQL_FROM_TOKEN + SP + FTableName +
            SP + SQL_WHERE_TOKEN + SP + VValues;
          if AIntoSQL then
            FQuery.SQL.Text := VSQL
          else
            FQuery.DeleteSQL.Text := VSQL;
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

procedure TJDOCustomSQL.Like(const AField: string; const ACaseInsensitive: Boolean);
begin
  if ACaseInsensitive then
    FLike := SQL_LOWER_TOKEN + PS + AField + PE
  else
    FLike := AField;
  FLike += SP + SQL_LIKE_TOKEN + SP + PS + CO + AField + PE;
end;

{ TJDOCustomQuery }

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
  DisableControls;
  VBookMark := GetBookmark;
  try
    DataSetToJSON(Self, AJSON, FDateAsString);
  finally
    GotoBookmark(VBookMark);
    FreeBookmark(VBookmark);
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
  if not Assigned(AStream) then
    Exit;
  AStream.Position := 0;
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

function TJDOCustomQuery.GetAbout: string;
begin
  Result := ES;
end;

procedure TJDOCustomQuery.SetAbout(AValue: string);
begin
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
  VBookMark := GetBookmark;
  try
    VPrimaryKey := GetPrimaryKey;
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
    GotoBookmark(VBookMark);
    FreeBookmark(VBookmark);
  end;
end;

procedure TJDOCustomQuery.Edit(AJSON: TJSONObject);
var
  VBookMark: TBookMark;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  VBookMark := GetBookmark;
  try
    VPrimaryKey := GetPrimaryKey;
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
    GotoBookmark(VBookMark);
    FreeBookmark(VBookmark);
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
  VBookMark := GetBookmark;
  try
    VPrimaryKey := GetPrimaryKey;
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
      GotoBookmark(VBookMark);
    FreeBookmark(VBookmark);
  end;
end;

procedure TJDOCustomQuery.Delete(AJSON: TJSONObject);
var
  VBookMark: TBookMark;
  VPrimaryKey: TIndexDef;
begin
  InternalCheckJSONParam(AJSON);
  VBookMark := GetBookmark;
  try
    VPrimaryKey := GetPrimaryKey;
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
      GotoBookmark(VBookMark);
    FreeBookmark(VBookmark);
  end;
end;

{ TJDOCustomDataBase }

constructor TJDOCustomDataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := TJDOConfigurator.Create(Self);
  Transaction := TSQLTransaction.Create(Self);
  FQuery := TJDOQuery.Create(Self);
  FConfig.Target := Self;
  FQuery.DataBase := Self;
  FQuery.Transaction := Transaction;
  Transaction.DataBase := Self;
  Transaction.Name := 'Transaction';
end;

constructor TJDOCustomDataBase.Create(AOwner: TComponent;
  const AConfiguration: string; const AConnect: Boolean);
begin
  Create(AOwner);
  FConfig.Configuration := AConfiguration;
  Connected := AConnect;
end;

{$IFDEF JDO_CRYPT}
procedure TJDOCustomDataBase.SetCryptKey(AValue: string);
begin
  if AValue <> FCryptKey then
  begin
    FCryptKey := AValue;
    FConfig.CryptKey := AValue;
    FConfig.Configure;
  end;
end;
{$ENDIF}

procedure TJDOCustomDataBase.Loaded;
begin
  inherited Loaded;
  if (Trim(ConnectorType) <> ES) and not
    Assigned(GetConnectionDef(ConnectorType)) then
    raise EJDODataBase.CreateFmt(Self,
      SConnUnitWasNotDeclaredError, [ConnectorType]);;
end;

procedure TJDOCustomDataBase.Connect;
begin
  Connected := True;
end;

procedure TJDOCustomDataBase.Disconnect;
begin
  Connected := False;
end;

function TJDOCustomDataBase.GetConfiguration: string;
begin
  Result := FConfig.Configuration;
end;

function TJDOCustomDataBase.GetAbout: string;
begin
  Result := ES;
end;

procedure TJDOCustomDataBase.SetAbout(AValue: string);
begin
end;

procedure TJDOCustomDataBase.SetConfiguration(const AValue: string);
begin
  FConfig.Configuration := AValue;
end;

function TJDOCustomDataBase.InTransaction: Boolean;
begin
  Result := Transaction.Active;
end;

procedure TJDOCustomDataBase.StartTransaction(const ANativeError: Boolean);
begin
  if (not ANativeError) and Transaction.Active then
    Exit;
  Transaction.StartTransaction;
end;

procedure TJDOCustomDataBase.RestartTransaction;
begin
  if Transaction.Active then
    Transaction.Rollback;
  Transaction.StartTransaction;
end;

procedure TJDOCustomDataBase.Commit(const ARetaining: Boolean);
begin
  if ARetaining then
    Transaction.CommitRetaining
  else
    Transaction.Commit;
end;

procedure TJDOCustomDataBase.Rollback(const ARetaining: Boolean);
begin
  if ARetaining then
    Transaction.RollbackRetaining
  else
    Transaction.Rollback;
end;

end.

