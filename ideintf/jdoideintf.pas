(*
  JDO Tool IDE inft unit
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

unit jdoideintf;

{$I jdo.inc}

interface

uses
  LazIDEIntf, SysUtils;

function GetExpertsConfigPath: string;
function GetExpertsConfigFileName: string;

implementation

function GetExpertsConfigPath: string;
begin
  Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath);
end;

function GetExpertsConfigFileName: string;
begin
  Result := GetExpertsConfigPath + 'jdo.xml';
end;

end.
