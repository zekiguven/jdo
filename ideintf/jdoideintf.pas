(*
  JDO IDE inft unit
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

unit JDOIDEIntf;

{$I jdo.inc}

interface

uses
  LazIDEIntf, SysUtils;

function GetExpertsConfigPath: string;
function GetExpertsConfigFileName: string;

implementation

function GetExpertsConfigPath: string;
begin
  if Assigned(LazarusIDE) then
    Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath)
  else
    Result := '';
end;

function GetExpertsConfigFileName: string;
begin
  Result := GetExpertsConfigPath + 'jdo.xml';
end;

end.

