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
  JDO, JDOCompEdits, Classes, ComponentEditors, PropEdits, SQLdb;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JDO', [TJDOConfigurator, TJDOSQL, TJDODataBase,
    TJDOQuery]);
  RegisterComponentEditor(TJDOSQL, TJDOSQLComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDOConfigurator,
    'Configuration', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TJDODataBase,
    'Configuration', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSQLTransaction), TJDODataBase,
    'Transaction', THiddenPropertyEditor);
end;

end.