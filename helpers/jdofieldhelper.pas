(*
  JDOFieldHelper unit
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

unit JDOFieldHelper;

{$i jdo.inc}

interface

uses
  JDOUtils, JDOConsts, DB, FPJSON, SysUtils;

type
  TJDOFieldHelper = class helper for TField
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
    function GetBeginsWith: string;
    function GetContains: string;
    function GetEndsWith: string;
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
    procedure SetBeginsWith(AValue: string);
    procedure SetContains(AValue: string);
    procedure SetEndsWith(AValue: string);
  public
    procedure Show;
    procedure Hide;
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
    property Contains: string read GetContains write SetContains;
    property EndsWith: string read GetEndsWith write SetEndsWith;
    property BeginsWith: string read GetBeginsWith write SetBeginsWith;
  end;

implementation

function TJDOFieldHelper.GetAsBase64: string;
begin
  Result := StrToBase64(AsString);
end;

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

function TJDOFieldHelper.GetAsLowerStr: string;
begin
  Result := LowerCase(AsString);
end;

function TJDOFieldHelper.GetAsQuotedStr: string;
begin
  Result := AnsiQuotedStr(AsString, AP);
end;

function TJDOFieldHelper.GetAsSmallInt: SmallInt;
begin
  Result := AsInteger;
end;

function TJDOFieldHelper.GetAsTime: TTime;
begin
  Result := Frac(AsDateTime);
end;

function TJDOFieldHelper.GetAsTrimStr: string;
begin
  Result := Trim(AsString);
end;

function TJDOFieldHelper.GetAsUpperStr: string;
begin
  Result := UpperCase(AsString);
end;

function TJDOFieldHelper.GetBeginsWith: string;
begin
  Result := AsString + PT;
end;

function TJDOFieldHelper.GetContains: string;
begin
  Result := PT + AsString + PT;
end;

function TJDOFieldHelper.GetEndsWith: string;
begin
  Result := PT + AsString;
end;

procedure TJDOFieldHelper.SetAsBase64(AValue: string);
begin
  AsString := Base64ToStr(AValue);
end;

procedure TJDOFieldHelper.SetAsChar(AValue: Char);
begin
  AsString := AValue;
end;

procedure TJDOFieldHelper.SetAsDate(AValue: TDate);
begin
  AsDateTime := Trunc(AValue);
end;

procedure TJDOFieldHelper.SetAsJSON(AValue: TJSONStringType);
begin
  AsString := JSONStringToString(AValue);
end;

procedure TJDOFieldHelper.SetAsLowerStr(AValue: string);
begin
  AsString := LowerCase(AValue);
end;

procedure TJDOFieldHelper.SetAsQuotedStr(AValue: string);
begin
  AsString := AnsiQuotedStr(AValue, AP);
end;

procedure TJDOFieldHelper.SetAsSmallInt(AValue: SmallInt);
begin
  AsInteger := AValue;
end;

procedure TJDOFieldHelper.SetAsTime(AValue: TTime);
begin
  AsDateTime := Frac(AValue);
end;

procedure TJDOFieldHelper.SetAsTrimStr(AValue: string);
begin
  AsString := Trim(AValue);
end;

procedure TJDOFieldHelper.SetAsUpperStr(AValue: string);
begin
  AsString := UpperCase(AValue);
end;

procedure TJDOFieldHelper.SetBeginsWith(AValue: string);
begin
  AsString := AValue + PT;
end;

procedure TJDOFieldHelper.SetContains(AValue: string);
begin
  AsString := PT + AValue + PT;
end;

procedure TJDOFieldHelper.SetEndsWith(AValue: string);
begin
  AsString := PT + AValue;
end;

procedure TJDOFieldHelper.Show;
begin
  Visible := True;
end;

procedure TJDOFieldHelper.Hide;
begin
  Visible := False;
end;

end.
