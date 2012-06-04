(*
  JDOTable interface include
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

  EJDOTable = class(EJDOException);

  TJDOModeTypes = (mtInsert, mtUpdate, mtDelete);

  TJDOLikeOptions = set of TLocateOption;

  TJDOTable = class(TJSONArray)
  private
    FAutoSetup: Boolean;
    FColumns: TFieldDefs;
    FDataBase: TJDODataBase;
    FMode: TJDOModeTypes;
    FPersisted: Boolean;
    FQuery: TJDOQuery;
    FSQL: TJDOSQL;
    function GetDataBase: TJDODataBase;
    function GetDateAsString: Boolean;
    function GetName: string;
    procedure SetDataBase(const AValue: TJDODataBase);
    procedure SetDateAsString(const AValue: Boolean);
    procedure SetMode(const AValue: TJDOModeTypes);
    procedure SetName(const AValue: string);
  protected
    procedure SetKey;
  public
    constructor Create(ADataBase: TJDODataBase; const ATableName: string);
    destructor Destroy; override;
    procedure Apply;
    procedure Post;
    procedure Setup;
    procedure Reset;
    function Open: Boolean;
    procedure Like(const AValue: string; const AKey: ShortString;
      const AOptions: TJDOLikeOptions = []);
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prior;
    function Get: TJSONObject;
    function Find(const AName: string; const AValue: string): TJSONObject;
    function Find(const AName: string; const AValue: Boolean): TJSONObject;
    function Find(const AName: string; const AValue: Integer): TJSONObject;
    function Find(const AName: string; const AValue: Double): TJSONObject;
    function Field(const AName: string): TField;
    function Param(const AName: string): TParam;
    property AutoSetup: Boolean read FAutoSetup write FAutoSetup;
    property Columns: TFieldDefs read FColumns;
    property DateAsString: Boolean read GetDateAsString write SetDateAsString;
    property DataBase: TJDODataBase read GetDataBase write SetDataBase;
    property Mode: TJDOModeTypes read FMode write SetMode;
    property Name: string read GetName write SetName;
    property Persisted: Boolean read FPersisted write FPersisted;
    property Query: TJDOQuery read FQuery;
    property SQL: TJDOSQL read FSQL;
  end;

  TJDOTableClass = class of TJDOTable;