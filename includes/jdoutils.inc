(*
  JDOUtils implementation include
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

function TimeOf(AData: TJSONData; const ADefValue: TTime): TTime;
begin
  case AData.JSONType of
    jtNumber: Result := Frac(AData.AsFloat);
    jtString: Result := StrToTimeDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;

function DateOf(AData: TJSONData; const ADefValue: TDate): TDate;
begin
  case AData.JSONType of
    jtNumber: Result := Trunc(AData.AsFloat);
    jtString: Result := StrToDateDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;

function DateTimeOf(AData: TJSONData; const ADefValue: TDateTime): TDateTime;
begin
  case AData.JSONType of
    jtNumber: Result := AData.AsFloat;
    jtString: Result := StrToDateTimeDef(AData.AsString, ADefValue);
  else
    Result := ADefValue;
  end;
end;
