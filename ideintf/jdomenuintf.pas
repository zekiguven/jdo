(*
  JDO menu inft unit
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

unit jdomenuintf;

{$I jdo.inc}

interface

uses
  frmjdosqltool, MenuIntf, IDECommands, LCLType, Classes;

const
  JDO_SQL_TOOL_MENU_NAME = 'JDOSQLToolMenu';
  JDO_SQL_TOOL_MENU_DESCRIPTION = 'JDO SQL Tool';

procedure Register;

implementation

procedure OpenSQLToolProc({%H-}ASender: TObject);
begin
  TfrJDOSQLTool.Execute;
end;

procedure Register;
var
  VKey: TIDEShortCut;
  VCmd: TIDECommand;
  VCat: TIDECommandCategory;
begin
  VKey := IDEShortCut(VK_J, [ssCtrl, ssShift], VK_UNKNOWN, []);
  VCat := IDECommandList.FindCategoryByName(CommandCategoryCustomName);
  VCmd := RegisterIDECommand(VCat, JDO_SQL_TOOL_MENU_NAME,
    JDO_SQL_TOOL_MENU_DESCRIPTION, VKey, nil, @OpenSQLToolProc);
  RegisterIDEMenuCommand(mnuTools, JDO_SQL_TOOL_MENU_NAME,
    JDO_SQL_TOOL_MENU_DESCRIPTION, nil, @OpenSQLToolProc, VCmd, 'jdosqltool');
end;

end.

