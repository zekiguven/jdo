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
    function GetAsJSON: TJSONStringType;
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
    class procedure QueryToSchema(AQuery: TSQLQuery; ASchema: TJSONObject);
    class procedure QueryToSchema(AQuery: TSQLQuery; out ASchema: TJSONStringType);
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
    property DateAsString: Boolean read FDateAsString write FDateAsString;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
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
