(*
  JDOParamHelper unit
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

unit JDOParamHelper;

{$i jdo.inc}

interface

uses
  JDOUtils, DB, FPJSON, SysUtils;

type
  TJDOParamHelper = class helper for TParam
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

function TJDOParamHelper.GetAsBase64: string;
begin
  Result := StrToBase64(AsString);
end;

function TJDOParamHelper.GetAsChar: Char;
begin
  Result := PChar(AsString)^;
end;

function TJDOParamHelper.GetAsJSON: TJSONStringType;
begin
  Result := StringToJSONString(AsString);
end;

function TJDOParamHelper.GetAsLowerString: string;
begin
  Result := LowerCase(AsString);
end;

function TJDOParamHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TJDOParamHelper.GetAsTrimString: string;
begin
  Result := Trim(AsString);
end;

function TJDOParamHelper.GetAsUpperString: string;
begin
  Result := UpperCase(AsString);
end;

procedure TJDOParamHelper.SetAsBase64(AValue: string);
begin
  AsString := Base64ToStr(AValue);
end;

procedure TJDOParamHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TJDOParamHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TJDOParamHelper.SetAsLowerString(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TJDOParamHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TJDOParamHelper.SetAsTrimString(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TJDOParamHelper.SetAsUpperString(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

end.
