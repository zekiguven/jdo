(*
  JDOQuery interface include
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

  EJDOQuery = class(EJDOException);

  TJDOCustomQuery = class(TSQLQuery)
  private
    FDateAsString: Boolean;
    FOnExecute: TNotifyEvent;
    FOnOpen: TNotifyEvent;
  protected
    procedure InternalCheckFieldDefs;
    procedure InternalCheckJSONParam(AJSON: TJSONData);
  public
    constructor Create(AOwner: TComponent); override;
    class function GetJSONType(const AFieldType: TFieldType): ShortString;
    function GetPrimaryKey: TIndexDef;
    function GetJSONValuesFromPrimaryKey(const AJSON: TJSONObject): Variant;
    procedure JSONToQuery(AJSON: TJSONObject; AQuery: TSQLQuery;
      ADateAsString: Boolean);
    procedure JSONToDataSet(AJSON: TJSONObject; ADataSet: TDataSet;
      ADateAsString: Boolean);
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray);
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONObject);
    function GetJSONArray: TJSONArray;
    function GetJSONObject: TJSONObject;
    procedure SetJSONArray(AJSON: TJSONArray);
    procedure SetJSONObject(AJSON: TJSONObject);
    procedure LoadJSONFromStream(AStream: TStream);
    procedure LoadJSONFromFile(const AFileName: TFileName);
    procedure LoadJSONFromString(const AString: TJSONStringType);
    procedure SaveJSONToStream(AStream: TStream);
    procedure SaveJSONToFile(const AFileName: TFileName);
    function SaveJSONToString: TJSONStringType;
    procedure Apply(const ARetaining: Boolean = False);
    procedure Undo(const ARetaining: Boolean = False);
    function Execute: Boolean;
    function Open: Boolean;
    procedure QueryToSchema(AQuery: TSQLQuery; ASchema: TJSONObject);
    function Schema: TJSONObject;
    procedure QueryToJSONSchema(AQuery: TSQLQuery;
      out AJSONSchema: TJSONStringType);
    function JSONSchema: TJSONStringType;
    procedure Append(AJSON: TJSONArray); overload;
    procedure Append(AJSON: TJSONObject); overload;
    procedure Insert(AJSON: TJSONArray); overload;
    procedure Insert(AJSON: TJSONObject); overload;
    procedure Edit(AJSON: TJSONArray); overload;
    procedure Edit(AJSON: TJSONObject); overload;
    procedure Delete(AJSON: TJSONArray); overload;
    procedure Delete(AJSON: TJSONObject); overload;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    property JSON: TJSONStringType read SaveJSONToString write LoadJSONFromString;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;

  TJDOCustomQueryClass = class of TJDOCustomQuery;

  TJDOQuery = class(TJDOCustomQuery)
  published
    property DateAsString;
    property OnExecute;
    property OnOpen;
  end;

  TJDOQueryClass = class of TJDOQuery;
