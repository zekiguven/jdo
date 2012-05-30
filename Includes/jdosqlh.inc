(*
  JDOSQL interface include
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

  EJDOSQL = class(EJDOException);

  TJDOPutTypes = (ptBegin, ptMiddle, ptEnd);

  TJDOSQL = class(TComponent)
  private
    FFieldDefs: TFieldDefs;
    FPutBegin: string;
    FPutMiddle: string;
    FPutEnd: string;
    FKey: string;
    FOrderBy: string;
    FOrdered: Boolean;
    FScript: TStrings;
    FStatementType: TStatementType;
    FTableAlias: string;
    FTableName: string;
    FWhere: string;
  protected
    procedure CheckTableName;
    procedure CheckKey;
  public
    constructor Create(AOwner: TComponent; ASQL: TStrings;
      AFieldDefs: TFieldDefs; const ATableName: string = ES); overload;
    constructor Create(ASQL: TStrings; AFieldDefs: TFieldDefs;
      const ATableName: string = ES); overload;
    function FormatCols(const AToken: string;
      const ASkipKey, APairs: Boolean): string;{$IFDEF JDO_INLINE}inline;{$ENDIF}
    procedure Compose(const AStatementType: TStatementType);
    procedure Clear;
    procedure Put(const ASQL: string; const AType: TJDOPutTypes = ptMiddle);
    property Key: string read FKey write FKey;
    property OrderBy: string read FOrderBy write FOrderBy;
    property Ordered: Boolean read FOrdered write FOrdered;
    property Where: string read FWhere write FWhere;
    property StatementType: TStatementType read FStatementType;
    property TableAlias: string read FTableAlias write FTableAlias;
    property TableName: string read FTableName write FTableName;
  end;

  TJDOSQLClass = class of TJDOSQL;
