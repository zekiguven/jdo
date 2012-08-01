(*
  JDOFieldHelper unit
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

unit JDOFieldHelper;

{$i jdo.inc}

interface

uses
  DB, FPJSON, SysUtils;

type
  TJDOFieldHelper = class helper for TField
  private
    function GetAsChar: Char;
    function GetAsDate: TDate;
    function GetAsJSON: TJSONStringType;
    function GetAsSmallInt: SmallInt;
    function GetAsTime: TTime;
    function GetAsTrimString: string;
    procedure SetAsChar(AValue: Char);
    procedure SetAsDate(AValue: TDate);
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetAsSmallInt(AValue: SmallInt);
    procedure SetAsTime(AValue: TTime);
    procedure SetAsTrimString(AValue: string);
  public
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property AsTrimString: string read GetAsTrimString write SetAsTrimString;
  end;

implementation

function TJDOFieldHelper.GetAsChar: Char;
begin
  Result := PChar(AsString)^;
end;

function TJDOFieldHelper.GetAsDate: TDate;
begin
  Result := Trunc(AsDateTime);
end;

function TJDOFieldHelper.GetAsJSON: TJSONStringType;
begin
  Result := StringToJSONString(AsString);
end;

function TJDOFieldHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TJDOFieldHelper.GetAsTime: TTime;
begin
  Result := Frac(AsDateTime);
end;

function TJDOFieldHelper.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

procedure TJDOFieldHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TJDOFieldHelper.SetAsDate(AValue: TDate);
begin
  AsDateTime := AValue;
end;

procedure TJDOFieldHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TJDOFieldHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TJDOFieldHelper.SetAsTime(AValue: TTime);
begin
  AsDateTime := AValue;
end;

procedure TJDOFieldHelper.SetAsTrimString(AValue: string);
begin
  AsString := Trim(AValue);
end;

end.

