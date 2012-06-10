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

  TJDOStatementType = (jstUnknown, jstSelect, jstInsert, jstUpdate, jstDelete);

  TJDOComposeEvent = procedure(const AStatementType: TJDOStatementType) of object;

  TJDOCustomSQL = class(TComponent)
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
    procedure InternalCheckFieldDefs(const ACount: Integer);
    procedure InternalCheckServerIndexDefs(const ACount: Integer);
  protected
    procedure CheckTableName;
    procedure CheckQuery;
    procedure PrepareCols(const AStatementType: TJDOStatementType;
      out ACols, AValues: string);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AQuery: TSQLQuery;
      const ATableName: string = ES); overload;
    destructor Destroy; override;
    function Compose(const AStatementType: TJDOStatementType): Boolean;
    procedure Reset;
    procedure Clear;
    procedure Put(const ASQL: string; const AType: TJDOPutTypes = ptMiddle;
      const ALineBreak: ShortString = SP);
    procedure Like(const AKey: string; const ACaseInsensitive: Boolean = False);
    property OrderBy: string read FOrderBy write FOrderBy;
    property Ordered: Boolean read FOrdered write FOrdered default True;
    property Where: string read FWhere write FWhere;
    property StatementType: TJDOStatementType read FStatementType;
    property TableAlias: string read FTableAlias write FTableAlias;
    property TableName: string read FTableName write FTableName;
    property Query: TSQLQuery read FQuery write FQuery;
    property OnCompose: TJDOComposeEvent read FOnCompose write FOnCompose;
  end;

  TJDOCustomSQLClass = class of TJDOCustomSQL;

  TJDOSQL = class(TJDOCustomSQL)
  published
    property OrderBy;
    property Ordered;
    property Where;
    property StatementType;
    property TableAlias;
    property TableName;
    property Query;
    property OnCompose;
  end;

  TJDOSQLClass = class of TJDOSQL;
