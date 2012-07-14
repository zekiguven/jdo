(*
  JDO property editors unit
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

unit JDOPropEdits;

{$I jdo.inc}

interface

uses
  JDO, frmJDOAbout, SQLdb, PropEdits, Classes, SysUtils, SrcEditorIntf,
  CodeToolManager, CodeCache;

type
  TJDOAboutPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  TJDOConnectorTypePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
    procedure SetValue(const ANewValue: AnsiString); override;
  end;

  TJDOTableNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

procedure AddConnUnit(const ATypeName: string);
procedure RemoveAllConnUnit;

implementation

uses
{$IFDEF JDO_NEW_FPC}
  mysql55conn,
  mssqlconn,
{$ELSE}
  sqlite3def,
{$ENDIF}
  IBConnection,
{$IFDEF JDO_HASMYSQL4CONNECTION}
  mysql40conn,
{$ENDIF}
{$IFDEF JDO_HASORACLECONNECTION}
  oracleconnection,
{$ENDIF}
{$IFDEF JDO_HASPQCONNECTION}
  pqconnection,
{$ENDIF}
{$IFDEF JDO_HASSQLITE3CONNECTION}
  sqlite3conn,
{$ENDIF}
  mysql41conn,
  mysql50conn,
  mysql51conn,
  odbcconn;

const
  ConnUnitNames: array[0..10] of string = ('IBConnection', 'MySQL40Conn',
    'MySQL41Conn', 'MySQL50Conn', 'MySQL51Conn', 'MySQL55Conn', 'ODBCConn',
    'OracleConnection', 'PQConnection', 'SQLite3Conn', 'MSSQLConn');

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
  if SameText(ATypeName, TIBConnectionDef.TypeName) then
    VUnit := 'IBConnection';
{$IFDEF JDO_HASMYSQL4CONNECTION}
  if SameText(ATypeName, TMySQL40ConnectionDef.TypeName) then
    VUnit := 'MySQL40Conn';
{$ENDIF}
  if SameText(ATypeName, TMySQL41ConnectionDef.TypeName) then
    VUnit := 'MySQL41Conn';
  if SameText(ATypeName, TMySQL50ConnectionDef.TypeName) then
    VUnit := 'MySQL50Conn';
  if SameText(ATypeName, TMySQL51ConnectionDef.TypeName) then
    VUnit := 'MySQL51Conn';
{$IFDEF JDO_NEW_FPC}
  if SameText(ATypeName, TMySQL55ConnectionDef.TypeName) then
    VUnit := 'MySQL55Conn';
{$ENDIF}
  if SameText(ATypeName, TODBCConnectionDef.TypeName) then
    VUnit := 'ODBCConn';
{$IFDEF HASORACLECONNECTION}
  if SameText(ATypeName, TOracleConnectionDef.TypeName) then
    VUnit := 'OracleConnection';
{$ENDIF}
{$IFDEF JDO_HASPQCONNECTION}
  if SameText(ATypeName, TPQConnectionDef.TypeName) then
    VUnit := 'PQConnection';
{$ENDIF}
{$IFDEF JDO_HASSQLITE3CONNECTION}
  if SameText(ATypeName, TSQLite3ConnectionDef.TypeName) then
    VUnit := 'SQLite3Conn';
{$ENDIF}
{$IFDEF JDO_NEW_FPC}
  if SameText(ATypeName, TMSSQLConnectionDef.TypeName) then
    VUnit := 'MSSQLConn';
{$ENDIF}
  CodeToolBoss.AddUnitToMainUsesSection(VCode, VUnit, '');
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
  AProc(TIBConnectionDef.TypeName);
{$IFDEF JDO_HASMYSQL4CONNECTION}
  AProc(TMySQL40ConnectionDef.TypeName);
{$ENDIF}
  AProc(TMySQL41ConnectionDef.TypeName);
  AProc(TMySQL50ConnectionDef.TypeName);
  AProc(TMySQL51ConnectionDef.TypeName);
{$IFDEF JDO_NEW_FPC}
  AProc(TMySQL55ConnectionDef.TypeName);
{$ENDIF}
  AProc(TODBCConnectionDef.TypeName);
{$IFDEF HASORACLECONNECTION}
  AProc(TOracleConnectionDef.TypeName);
{$ENDIF}
{$IFDEF JDO_HASPQCONNECTION}
  AProc(TPQConnectionDef.TypeName);
{$ENDIF}
{$IFDEF JDO_HASSQLITE3CONNECTION}
  AProc(TSQLite3ConnectionDef.TypeName);
{$ENDIF}
{$IFDEF JDO_NEW_FPC}
  AProc(TMSSQLConnectionDef.TypeName);
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
