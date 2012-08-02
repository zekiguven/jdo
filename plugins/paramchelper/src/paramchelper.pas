(*
  ParamCHelper unit
  Copyright (C) 2012-2014 Silvio Clecio.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit ParamCHelper;

{$mode objfpc}{$H+}

interface

uses
  JDOUtils, DB, FPJSON, SysUtils;

type
  TParamCHelper = class helper for TParam
  private
    function GetAsBase64: string;
    function GetAsChar: Char;
    function GetAsJSON: TJSONStringType;
    function GetAsLowerString: string;
    function GetAsSmallInt: SmallInt;
    function GetAsTrimString: string;
    function GetAsUpperString: string;
    procedure SetAsBase64(AValue: string);
    procedure SetAsChar(AValue: Char);
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetAsLowerString(AValue: string);
    procedure SetAsSmallInt(AValue: SmallInt);
    procedure SetAsTrimString(AValue: string);
    procedure SetAsUpperString(AValue: string);
  public
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property AsTrimString: string read GetAsTrimString write SetAsTrimString;
    property AsLowerString: string read GetAsLowerString write SetAsLowerString;
    property AsUpperString: string read GetAsUpperString write SetAsUpperString;
    property AsBase64: string read GetAsBase64 write SetAsBase64;
  end;

implementation

function TParamCHelper.GetAsBase64: string;
begin
  Result := StrToBase64(AsString);
end;

function TParamCHelper.GetAsChar: Char;
begin
  Result := PChar(AsString)^;
end;

function TParamCHelper.GetAsJSON: TJSONStringType;
begin
  Result := StringToJSONString(AsString);
end;

function TParamCHelper.GetAsLowerString: string;
begin
  Result := LowerCase(AsString);
end;

function TParamCHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TParamCHelper.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TParamCHelper.GetAsUpperString: string;
begin
  Result := UpperCase(AsString);
end;

procedure TParamCHelper.SetAsBase64(AValue: string);
begin
  AsString := Base64ToStr(AValue);
end;

procedure TParamCHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TParamCHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TParamCHelper.SetAsLowerString(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TParamCHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TParamCHelper.SetAsTrimString(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TParamCHelper.SetAsUpperString(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

end.

