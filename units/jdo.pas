(*
  JSON Data Objects unit
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

unit JDO;

{$I jdo.inc}

interface

uses
{$IFDEF JDO_CRYPT}
  BlowFish,
{$ENDIF}
  Classes, SysUtils, SQLdb, DB, FPJSON, TypInfo, Variants, JSONParser, DBConst;

{$i jdoconstsh.inc}

type
  IJDOAboutComponent = interface
    ['{05155F56-E531-4284-AA35-9E78C18A5773}']
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    property About: string read GetAbout write SetAbout stored False;
  end;

  EJDO = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string);
    constructor CreateFmt(AInstance: TObject; const AMsg: string;
      const AArgs: array of const);
  end;

{$i jdoconfigh.inc}
{$i jdosqlh.inc}
{$i jdoqueryh.inc}
{$i jdodatabaseh.inc}
{$i jdoutilsh.inc}

implementation

constructor EJDO.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt(ERROR_MASK, [AInstance.ClassName, AMsg]);
end;

constructor EJDO.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(ERROR_MASK, [AInstance.ClassName, AMsg]), AArgs);
end;

{$i jdoconfig.inc}
{$i jdosql.inc}
{$i jdoquery.inc}
{$i jdodatabase.inc}
{$i jdoutils.inc}

end.

