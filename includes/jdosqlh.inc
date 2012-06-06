(*
  JDOSQL interface include
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

  EJDOSQL = class(EJDOException);

  TJDOPutTypes = (ptBegin, ptMiddle, ptEnd);

  TJDOStatementType = (stUnknown, stSelect, stInsert, stUpdate, stDelete);

  TJDOComposeEvent = procedure(const AStatementType: TJDOStatementType) of object;

  TJDOCustomSQL = class(TComponent)
  private
    FLike: string;
    FFieldDefs: TFieldDefs;
    FIndexDefs: TServerIndexDefs;
    FOnCompose: TJDOComposeEvent;
    FPutBegin: string;
    FPutMiddle: string;
    FPutEnd: string;
    FOrderBy: string;
    FOrdered: Boolean;
    FScript: TStrings;
    FStatementType: TJDOStatementType;
    FTableAlias: string;
    FTableName: string;
    FWhere: string;
    procedure InternalCheckFieldDefs(const ACount: Integer);
    procedure InternalCheckIndexDefs(const ACount: Integer);
  protected
    function IsComposed: Boolean;
    procedure CheckTableName;
    procedure PrepareCols(const AStatementType: TJDOStatementType;
      out ACols, AValues: string);
  public
    constructor Create(AOwner: TComponent; ASQL: TStrings;
      AFieldDefs: TFieldDefs; AIndexDefs: TServerIndexDefs;
      const ATableName: string = ES); overload;
    constructor Create(ASQL: TStrings; AFieldDefs: TFieldDefs;
      AIndexDefs: TServerIndexDefs; const ATableName: string = ES); overload;
    procedure Compose(const AStatementType: TJDOStatementType);
    procedure Reset;
    procedure Clear;
    procedure Put(const ASQL: string; const AType: TJDOPutTypes = ptMiddle);
    procedure Like(const AKey: string; const ACaseInsensitive: Boolean = False);
    property Composed: Boolean read IsComposed;
    property FieldDefs: TFieldDefs read FFieldDefs;
    property IndexDefs: TServerIndexDefs read FIndexDefs;
    property OrderBy: string read FOrderBy write FOrderBy;
    property Ordered: Boolean read FOrdered write FOrdered;
    property Where: string read FWhere write FWhere;
    property Script: TStrings read FScript;
    property StatementType: TJDOStatementType read FStatementType;
    property TableAlias: string read FTableAlias write FTableAlias;
    property TableName: string read FTableName write FTableName;
    property OnCompose: TJDOComposeEvent read FOnCompose write FOnCompose;
  end;

  TJDOCustomSQLClass = class of TJDOCustomSQL;

  TJDOSQL = class(TJDOCustomSQL)
  published
    property Composed;
    property OrderBy;
    property Ordered;
    property Where;
    property Script;
    property TableAlias;
    property TableName;
    property OnCompose;
  end;

  TJDOSQLClass = class of TJDOSQL;
