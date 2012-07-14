(*
  JDO Tool menu inft unit
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

unit jdomenuintf;

{$I jdo.inc}

interface

uses
  frmjdotool, MenuIntf;

const
  JDO_TOOL_MENU_NAME = 'JDOToolMenu';
  JDO_TOOL_MENU_DESCRIPTION = 'JDO Tool';

procedure Register;

implementation

procedure OpenToolProc({%H-}ASender: TObject);
begin
  TfrJDOTool.Execute;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(mnuTools, JDO_TOOL_MENU_NAME, JDO_TOOL_MENU_DESCRIPTION,
    nil, @OpenToolProc, nil, 'jdotool');
end;

end.

