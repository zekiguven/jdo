(*
  FieldCHelper unit
  Copyright (C) 2012-2014 Silvio Clecio.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit FieldCHelper;

{$mode objfpc}{$H+}

interface

uses
  JDOUtils, DB, FPJSON, SysUtils;

type
  TFieldCHelper = class helper for TField
  private
    function GetAsBase64: string;
    function GetAsChar: Char;
    function GetAsDate: TDate;
    function GetAsJSON: TJSONStringType;
    function GetAsLowerString: string;
    function GetAsSmallInt: SmallInt;
    function GetAsTime: TTime;
    function GetAsTrimString: string;
    function GetAsUpperString: string;
    procedure SetAsBase64(AValue: string);
    procedure SetAsChar(AValue: Char);
    procedure SetAsDate(AValue: TDate);
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetAsLowerString(AValue: string);
    procedure SetAsSmallInt(AValue: SmallInt);
    procedure SetAsTime(AValue: TTime);
    procedure SetAsTrimString(AValue: string);
    procedure SetAsUpperString(AValue: string);
  public
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property AsTrimString: string read GetAsTrimString write SetAsTrimString;
    property AsLowerString: string read GetAsLowerString write SetAsLowerString;
    property AsUpperString: string read GetAsUpperString write SetAsUpperString;
    property AsBase64: string read GetAsBase64 write SetAsBase64;
  end;

implementation

function TFieldCHelper.GetAsBase64: string;
begin
  Result := StrToBase64(AsString);
end;

function TFieldCHelper.GetAsChar: Char;
begin
  Result := PChar(AsString)^;
end;

function TFieldCHelper.GetAsDate: TDate;
begin
  Result := Trunc(AsDateTime);
end;

function TFieldCHelper.GetAsJSON: TJSONStringType;
begin
  Result := StringToJSONString(AsString);
end;

function TFieldCHelper.GetAsLowerString: string;
begin
  Result := LowerCase(AsString);
end;

function TFieldCHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TFieldCHelper.GetAsTime: TTime;
begin
  Result := Frac(AsDateTime);
end;

function TFieldCHelper.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TFieldCHelper.GetAsUpperString: string;
begin
  Result := UpperCase(AsString);
end;

procedure TFieldCHelper.SetAsBase64(AValue: string);
begin
  AsString := Base64ToStr(AValue);
end;

procedure TFieldCHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TFieldCHelper.SetAsDate(AValue: TDate);
begin
  AsDateTime := AValue;
end;

procedure TFieldCHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TFieldCHelper.SetAsLowerString(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TFieldCHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TFieldCHelper.SetAsTime(AValue: TTime);
begin
  AsDateTime := AValue;
end;

procedure TFieldCHelper.SetAsTrimString(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TFieldCHelper.SetAsUpperString(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

end.

