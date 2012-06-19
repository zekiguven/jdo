(*
  JDO register components unit
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

unit JDOReg;

{$I jdo.inc}

interface

uses
  JDO, JDOCompEdits, JDOPropEdits, LResources, Classes, ComponentEditors,
  PropEdits, SQLdb;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JDO', [TJDODataBase, TJDOQuery, TJDOSQL,
    TJDOConfigurator]);
  RegisterComponentEditor(TJDOConfigurator, TJDOConfiguratorComponentEditor);
  RegisterComponentEditor(TJDOSQL, TJDOSQLComponentEditor);
  RegisterComponentEditor(TJDODataBase, TJDODataBaseComponentEditor);
  RegisterComponentEditor(TJDOQuery, TJDOQueryComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDOConfigurator,
    'Configuration', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDODataBase,
    'Configuration', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDODataBase,
    'ConnectorType', TJDOConnectorTypePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDODataBase,
    'DatabaseName', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJDOStatementType), TJDOSQL,
    'StatementType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSQLTransaction), TJDODataBase,
    'Transaction', THiddenPropertyEditor);
end;

initialization
  {$i jdoreg.lrs}

end.
