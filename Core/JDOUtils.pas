(*
  JDO Utils unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOUtils;

{$I jdo.inc}

interface

uses
  SysUtils, FPJSON;

function TimeOf(AData: TJSONData): TTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateOf(AData: TJSONData): TDate;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateTimeOf(AData: TJSONData): TDateTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function TimeOfDef(AData: TJSONData; const ADefValue: TTime): TTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateOfDef(AData: TJSONData; const ADefValue: TDate): TDate;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateTimeOfDef(AData: TJSONData; const ADefValue: TDateTime): TDateTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}

implementation

function TimeOf(AData: TJSONData): TTime;
begin
  Result := Frac(AData.AsFloat);
end;

function DateOf(AData: TJSONData): TDate;
begin
  Result := Trunc(AData.AsFloat);
end;

function DateTimeOf(AData: TJSONData): TDateTime;
begin
  Result := AData.AsFloat;
end;

function TimeOfDef(AData: TJSONData; const ADefValue: TTime): TTime;
begin
  case AData.JSONType of
    jtNumber: Result := Frac(AData.AsFloat);
    jtString: Result := StrToTimeDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;

function DateOfDef(AData: TJSONData; const ADefValue: TDate): TDate;
begin
  case AData.JSONType of
    jtNumber: Result := Trunc(AData.AsFloat);
    jtString: Result := StrToDateDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;

function DateTimeOfDef(AData: TJSONData; const ADefValue: TDateTime): TDateTime;
begin
  case AData.JSONType of
    jtNumber: Result := AData.AsFloat;
    jtString: Result := StrToDateTimeDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;

end.

