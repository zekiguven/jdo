(*
  JDO property editors unit
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

unit JDOPropEdits;

{$I jdo.inc}

interface

uses
  JDO, SQLdb, PropEdits, Classes, SysUtils, SrcEditorIntf, CodeToolManager,
  CodeCache;

type
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
  IBConnection,
  mysql40conn,
  mysql41conn,
  mysql50conn,
  mysql51conn,
  mysql55conn,
  odbcconn,
  oracleconnection,
  pqconnection,
  sqlite3conn,
  mssqlconn;

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
  if SameText(ATypeName, TMySQL40ConnectionDef.TypeName) then
    VUnit := 'MySQL40Conn';
  if SameText(ATypeName, TMySQL41ConnectionDef.TypeName) then
    VUnit := 'MySQL41Conn';
  if SameText(ATypeName, TMySQL50ConnectionDef.TypeName) then
    VUnit := 'MySQL50Conn';
  if SameText(ATypeName, TMySQL51ConnectionDef.TypeName) then
    VUnit := 'MySQL51Conn';
  if SameText(ATypeName, TMySQL55ConnectionDef.TypeName) then
    VUnit := 'MySQL55Conn';
  if SameText(ATypeName, TODBCConnectionDef.TypeName) then
    VUnit := 'ODBCConn';
  if SameText(ATypeName, TOracleConnectionDef.TypeName) then
    VUnit := 'OracleConnection';
  if SameText(ATypeName, TPQConnectionDef.TypeName) then
    VUnit := 'PQConnection';
  if SameText(ATypeName, TSQLite3ConnectionDef.TypeName) then
    VUnit := 'SQLite3Conn';
  if SameText(ATypeName, TMSSQLConnectionDef.TypeName) then
    VUnit := 'MSSQLConn';
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

{ TJDOConnectorTypePropertyEditor }

function TJDOConnectorTypePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TJDOConnectorTypePropertyEditor.GetValues(AProc: TGetStrProc);
begin
  AProc(TIBConnectionDef.TypeName);
  AProc(TMySQL40ConnectionDef.TypeName);
  AProc(TMySQL41ConnectionDef.TypeName);
  AProc(TMySQL50ConnectionDef.TypeName);
  AProc(TMySQL51ConnectionDef.TypeName);
  AProc(TMySQL55ConnectionDef.TypeName);
  AProc(TODBCConnectionDef.TypeName);
  AProc(TOracleConnectionDef.TypeName);
  AProc(TPQConnectionDef.TypeName);
  AProc(TSQLite3ConnectionDef.TypeName);
  AProc(TMSSQLConnectionDef.TypeName);
end;

procedure TJDOConnectorTypePropertyEditor.SetValue(const ANewValue: AnsiString);
begin
  inherited;
  RemoveAllConnUnit;
  AddConnUnit(ANewValue);
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
