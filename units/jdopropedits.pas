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
  PropEdits, Classes, SrcEditorIntf, CodeToolManager, CodeCache;

type
  TJDOConnectorTypePropertyEditor = class(TStringPropertyEditor)
  private
    FOldValue: AnsiString;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
    procedure SetValue(const ANewValue: AnsiString); override;
  end;

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
var
  VUnit: string;
  VCode: TCodeBuffer;
  VSrcEdit: TSourceEditorInterface;
begin
  inherited;
  VSrcEdit := SourceEditorManagerIntf.ActiveEditor;
  if VSrcEdit = nil then
    Exit;
  VCode := TCodeBuffer(VSrcEdit.CodeToolsBuffer);
  if VCode = nil then
    Exit;
  CodeToolBoss.RemoveUnitFromAllUsesSections(VCode, FOldValue);
  if ANewValue = TIBConnectionDef.TypeName then
    VUnit := 'IBConnection';
  if ANewValue = TMySQL40ConnectionDef.TypeName then
    VUnit := 'MySQL40Conn';
  if ANewValue = TMySQL41ConnectionDef.TypeName then
    VUnit := 'MySQL41Conn';
  if ANewValue = TMySQL50ConnectionDef.TypeName then
    VUnit := 'MySQL50Conn';
  if ANewValue = TMySQL51ConnectionDef.TypeName then
    VUnit := 'MySQL51Conn';
  if ANewValue = TMySQL55ConnectionDef.TypeName then
    VUnit := 'MySQL55Conn';
  if ANewValue = TODBCConnectionDef.TypeName then
    VUnit := 'ODBCConn';
  if ANewValue = TOracleConnectionDef.TypeName then
    VUnit := 'OracleConnection';
  if ANewValue = TPQConnectionDef.TypeName then
    VUnit := 'PQConnection';
  if ANewValue = TSQLite3ConnectionDef.TypeName then
    VUnit := 'SQLite3Conn';
  if ANewValue = TMSSQLConnectionDef.TypeName then
    VUnit := 'MSSQLConn';
  FOldValue := VUnit;
  CodeToolBoss.AddUnitToMainUsesSection(VCode, VUnit, '');
end;

end.
