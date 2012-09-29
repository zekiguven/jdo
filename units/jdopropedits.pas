(*
  JDO property editors unit
  Copyright (C) 2012-2014 Silvio Clecio.

  http://silvioprog.github.com/jdo/

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOPropEdits;

{$I jdo.inc}

interface

uses
  JDO, JDOConsts, frmJDOAbout, SQLdb, PropEdits, Classes, SysUtils,
  SrcEditorIntf, CodeToolManager, CodeCache;

type

  { TJDOAboutPropertyEditor }

  TJDOAboutPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  { TJDOConnectorTypePropertyEditor }

  TJDOConnectorTypePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
    procedure SetValue(const ANewValue: AnsiString); override;
  end;

  { TJDOTableNamePropertyEditor }

  TJDOTableNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

procedure AddConnUnit(const ATypeName: string);
procedure RemoveAllConnUnit;

implementation

uses
{$IFDEF HASIBCONNECTION}
  ibconnection
{$ENDIF}
{$IFDEF HASMSSQLCONNECTION}
  // mssqlconn provide both MS SQL Server and Sybase ASE connectors.
  , mssqlconn
{$ENDIF}
  , odbcconn
{$IFDEF HASPQCONNECTION}
  , pqconnection
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
  , oracleconnection
{$ENDIF}
{$IFDEF HASMYSQL4CONNECTION}
  , mysql40conn, mysql41conn
{$ENDIF}
  , mysql50conn
{$IFDEF HASMYSQL51CONNECTION}
  , mysql51conn
{$ENDIF}
{$IFDEF HASMYSQL55CONNECTION}
  , mysql55conn
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
  , sqlite3conn
{$ENDIF};

const
  ConnUnitNames: array[0..11] of string = ('IBConnection', 'MSSQLConn',
    'MSSQLConn', 'ODBCConn', 'PQConnection', 'OracleConnection', 'MySQL40Conn',
    'MySQL41Conn', 'MySQL50Conn', 'MySQL51Conn', 'MySQL55Conn', 'SQLite3Conn');

function CodeBuffer: TCodeBuffer;
var
  VSrcEdit: TSourceEditorInterface;
begin
  VSrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if Assigned(VSrcEdit) then
    Result := TCodeBuffer(VSrcEdit.CodeToolsBuffer);
end;

procedure AddConnUnit(const ATypeName: string);
var
  VUnit: string;
  VCode: TCodeBuffer;
begin
  VCode := CodeBuffer;
  if VCode = nil then
    Exit;
{$IFDEF HASIBCONNECTION}
  if SameText(ATypeName, TIBConnectionDef.TypeName) then
    VUnit := 'IBConnection';
{$ENDIF}
{$IFDEF HASMSSQLCONNECTION}
  if SameText(ATypeName, TMSSQLConnectionDef.TypeName) then
    VUnit := 'MSSQLConn';
  if SameText(ATypeName, TSybaseConnectionDef.TypeName) then
    VUnit := 'MSSQLConn';
{$ENDIF}
  if SameText(ATypeName, TODBCConnectionDef.TypeName) then
    VUnit := 'ODBCConn';
{$IFDEF HASPQCONNECTION}
  if SameText(ATypeName, TPQConnectionDef.TypeName) then
    VUnit := 'PQConnection';
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
  if SameText(ATypeName, TOracleConnectionDef.TypeName) then
    VUnit := 'OracleConnection';
{$ENDIF}
{$IFDEF HASMYSQL4CONNECTION}
  if SameText(ATypeName, TMySQL40ConnectionDef.TypeName) then
    VUnit := 'MySQL40Conn';
  if SameText(ATypeName, TMySQL41ConnectionDef.TypeName) then
    VUnit := 'MySQL41Conn';
{$ENDIF}
  if SameText(ATypeName, TMySQL50ConnectionDef.TypeName) then
    VUnit := 'MySQL50Conn';
{$IFDEF HASMYSQL51CONNECTION}
  if SameText(ATypeName, TMySQL51ConnectionDef.TypeName) then
    VUnit := 'MySQL51Conn';
{$ENDIF}
{$IFDEF HASMYSQL55CONNECTION}
  if SameText(ATypeName, TMySQL55ConnectionDef.TypeName) then
    VUnit := 'MySQL55Conn';
{$ENDIF}
{$IF FPC_FULLVERSION>=21242}
  {$IFDEF HASSQLITE3CONNECTION}
  if SameText(ATypeName, TSQLite3ConnectionDef.TypeName) then
    VUnit := 'SQLite3Conn';
  {$ENDIF}
{$ENDIF}
  CodeToolBoss.AddUnitToMainUsesSection(VCode, VUnit, ES);
end;

procedure RemoveAllConnUnit;
var
  I: Integer;
  VCode: TCodeBuffer;
begin
  VCode := CodeBuffer;
  if VCode = nil then
    Exit;
  for I := 0 to High(ConnUnitNames) do
    CodeToolBoss.RemoveUnitFromAllUsesSections(VCode, ConnUnitNames[I]);
end;

{ TJDOAboutPropertyEditor }

procedure TJDOAboutPropertyEditor.Edit;
begin
  TfrJDOAbout.Execute;
end;

function TJDOAboutPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

function TJDOAboutPropertyEditor.GetValue: AnsiString;
begin
  Result := '(About)';
end;

{ TJDOConnectorTypePropertyEditor }

function TJDOConnectorTypePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TJDOConnectorTypePropertyEditor.GetValues(AProc: TGetStrProc);
begin
{$IFDEF HASIBCONNECTION}
  AProc(TIBConnectionDef.TypeName);
{$ENDIF}
{$IFDEF HASMSSQLCONNECTION}
  AProc(TMSSQLConnectionDef.TypeName);
  AProc(TSybaseConnectionDef.TypeName);
{$ENDIF}
  AProc(TODBCConnectionDef.TypeName);
{$IFDEF HASPQCONNECTION}
  AProc(TPQConnectionDef.TypeName);
{$ENDIF}
{$IFDEF HASORACLECONNECTION}
  AProc(TOracleConnectionDef.TypeName);
{$ENDIF}
{$IFDEF HASMYSQL4CONNECTION}
  AProc(TMySQL40ConnectionDef.TypeName);
  AProc(TMySQL41ConnectionDef.TypeName);
{$ENDIF}
  AProc(TMySQL50ConnectionDef.TypeName);
{$IFDEF HASMYSQL51CONNECTION}
  AProc(TMySQL51ConnectionDef.TypeName);
{$ENDIF}
{$IFDEF HASMYSQL55CONNECTION}
  AProc(TMySQL55ConnectionDef.TypeName);
{$ENDIF}
{$IFDEF HASSQLITE3CONNECTION}
  AProc(TSQLite3ConnectionDef.TypeName);
{$ENDIF}
end;

procedure TJDOConnectorTypePropertyEditor.SetValue(const ANewValue: AnsiString);
begin
  inherited;
  if ANewValue <> ES then
  begin
    RemoveAllConnUnit;
    AddConnUnit(ANewValue);
  end;
end;

{ TJDOTableNamePropertyEditor }

function TJDOTableNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TJDOTableNamePropertyEditor.GetValues(AProc: TGetStrProc);
var
  I: Integer;
  VSQL: TJDOSQL;
  VTables: TStrings;
  VDB: TSQLConnection;
begin
  VSQL := GetComponent(0) as TJDOSQL;
  if not Assigned(VSQL) then
    Exit;
  if not Assigned(VSQL.Query) then
    Exit;
  VDB := VSQL.Query.DataBase as TSQLConnection;
  if not Assigned(VDB) then
    Exit;
  VDB.Close;
  VDB.Open;
  VTables := TStringList.Create;
  try
    VDB.GetTableNames(VTables);
    for I := 0 to Pred(VTables.Count) do
      AProc(VTables[I]);
  finally
    VTables.Free;
  end;
end;

end.
