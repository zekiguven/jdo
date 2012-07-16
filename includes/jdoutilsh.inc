(*
  JDOUtils interface include
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

function TimeOf(AData: TJSONData): TTime;
function DateOf(AData: TJSONData): TDate;
function DateTimeOf(AData: TJSONData): TDateTime;
function TimeOf(AData: TJSONData; const ADefValue: TTime): TTime;
function DateOf(AData: TJSONData; const ADefValue: TDate): TDate;
function DateTimeOf(AData: TJSONData; const ADefValue: TDateTime): TDateTime;
function NextIndexName(var AFields: string; const ADelimiter: Char = SC): string;
function IndexNamesCount(const AFields: string; const ADelimiter: Char = SC): Integer;
function StrToHex(const AStr: string): string;
function HexToStr(const AHex: string): string;
{$IFDEF JDO_CRYPT}
function CryptStr(const AStr, AKey: string): string;
function DeCryptStr(const AStr, AKey: string): string;
{$ENDIF}
