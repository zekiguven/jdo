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
  PropEdits, Classes;

type
  TJDOConnectorTypePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(AProc: TGetStrProc); override;
  end;

implementation

function TJDOConnectorTypePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TJDOConnectorTypePropertyEditor.GetValues(AProc: TGetStrProc);
begin
  AProc('Firebird');
  AProc('MySQL 4.0');
  AProc('MySQL 4.1');
  AProc('MySQL 5.0');
  AProc('MySQL 5.1');
  AProc('MySQL 5.5');
  AProc('ODBC');
  AProc('Oracle');
  AProc('PostGreSQL');
  AProc('SQLite3');
  AProc('Sybase');
end;

end.
