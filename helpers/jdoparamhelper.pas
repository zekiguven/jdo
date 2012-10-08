(*
  JDOParamHelper unit
  Copyright (C) 2012-2014 Silvio Clecio.

  http://silvioprog.github.com/jdo/

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
  JDOUtils, JDOConsts, DB, FPJSON, SysUtils;

type
  TJDOParamHelper = class helper for TParam
  private
    function GetAsBase64: string;
    function GetAsChar: Char;
    function GetAsDate: TDate;
    function GetAsJSON: TJSONStringType;
    function GetAsLowerStr: string;
    function GetAsQuotedStr: string;
    function GetAsSmallInt: SmallInt;
    function GetAsTime: TTime;
    function GetAsTrimStr: string;
    function GetAsUpperStr: string;
    procedure SetAsBase64(AValue: string);
    procedure SetAsChar(AValue: Char);
    procedure SetAsDate(AValue: TDate);
    procedure SetAsJSON(AValue: TJSONStringType);
    procedure SetAsLowerStr(AValue: string);
    procedure SetAsQuotedStr(AValue: string);
    procedure SetAsSmallInt(AValue: SmallInt);
    procedure SetAsTime(AValue: TTime);
    procedure SetAsTrimStr(AValue: string);
    procedure SetAsUpperStr(AValue: string);
  public
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsTime: TTime read GetAsTime write SetAsTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsJSON: TJSONStringType read GetAsJSON write SetAsJSON;
    property AsTrimStr: string read GetAsTrimStr write SetAsTrimStr;
    property AsLowerStr: string read GetAsLowerStr write SetAsLowerStr;
    property AsUpperStr: string read GetAsUpperStr write SetAsUpperStr;
    property AsBase64: string read GetAsBase64 write SetAsBase64;
    property AsQuotedStr: string read GetAsQuotedStr write SetAsQuotedStr;
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

function TJDOParamHelper.GetAsDate: TDate;
begin
  Result := Trunc(AsDateTime);
end;

function TJDOParamHelper.GetAsJSON: TJSONStringType;
begin
  Result := StringToJSONString(AsString);
end;

function TJDOParamHelper.GetAsLowerStr: string;
begin
  Result := LowerCase(AsString);
end;

function TJDOParamHelper.GetAsQuotedStr: string;
begin
  Result := AnsiQuotedStr(AsString, AP);
end;

function TJDOParamHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TJDOParamHelper.GetAsTime: TTime;
begin
  Result := Frac(AsDateTime);
end;

function TJDOParamHelper.GetAsTrimStr: string;
begin
  Result := Trim(AsString);
end;

function TJDOParamHelper.GetAsUpperStr: string;
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

procedure TJDOParamHelper.SetAsDate(AValue: TDate);
begin
  AsDateTime := Trunc(AValue);
end;

procedure TJDOParamHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TJDOParamHelper.SetAsLowerStr(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TJDOParamHelper.SetAsQuotedStr(AValue: string);
begin
  AsString := AnsiQuotedStr(AValue, AP);
end;

procedure TJDOParamHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TJDOParamHelper.SetAsTime(AValue: TTime);
begin
  AsDateTime := Frac(AValue);
end;

procedure TJDOParamHelper.SetAsTrimStr(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TJDOParamHelper.SetAsUpperStr(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

end.
