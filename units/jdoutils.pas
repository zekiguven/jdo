(*
  JDOUtils unit
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

unit JDOUtils;

{$i jdo.inc}

interface

uses
{$IFDEF JDO_CRYPT}
  BlowFish,
{$ENDIF}
  JDOConsts, Classes, SysUtils, FPJSON, Base64;

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
function EncryptStr(const AStr, AKey: string): string;
function DecryptStr(const AStr, AKey: string): string;
{$ENDIF}
function StrToBase64(S: string): string;
function Base64ToStr(const S: string;
  const AMode: TBase64DecodingMode = bdmMIME): string;
function FileToBase64(const AFileName: TFileName): string;
procedure Base64ToFile(const S: string; const AFileName: TFileName;
  const AMode: TBase64DecodingMode = bdmMIME);

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

function NextIndexName(var AFields: string; const ADelimiter: Char): string;
var
  I: Integer;
begin
  I := Pos(ADelimiter, AFields);
  if I = 0 then
    I := Succ(Length(AFields));
  Result := Copy(AFields, 1, Pred(I));
  System.Delete(AFields, 1, I);
end;

function IndexNamesCount(const AFields: string; const ADelimiter: Char): Integer;
var
  I: Integer;
  B: Boolean;
begin
  Result := 0;
  for I := 1 to Length(AFields) do
    if AFields[I] = ADelimiter then
    begin
      Inc(Result);
      B := True;
    end;
  if B then
    Inc(Result);
end;

function StrToHex(const AStr: string): string;
var
  I: Integer;
begin
  Result:= ES;
  for I := 1 to Length(AStr) do
    Result += IntToHex(Ord(AStr[I]), 2);
end;

function HexToStr(const AHex: string): string;
var
  I: Integer;
begin
  Result:= ES;
  for I := 1 to Length(AHex) div 2 do
    Result := Result + Char(StrToInt(DS + Copy(AHex, (I - 1) * 2 + 1, 2)));
end;

{$IFDEF JDO_CRYPT}
function EncryptStr(const AStr, AKey: string): string;
var
  VInput: TStringStream;
  VBF: TBlowFishEncryptStream;
begin
  VInput := TStringStream.Create(ES);
  VBF := TBlowFishEncryptStream.Create(AKey, VInput);
  try
    VBF.Write(AStr[1], Length(AStr));
  finally
    VBF.Free;
    Result := VInput.DataString;
    VInput.Free;
  end;
end;

function DecryptStr(const AStr, AKey: string): string;
var
  VOutput: TStringStream;
  VBF: TBlowFishDeCryptStream;
begin
  VOutput := TStringStream.Create(AStr);
  VBF := TBlowFishDeCryptStream.Create(AKey, VOutput);
  try
    SetLength(Result, VOutput.Size);
    VBF.Read(Result[1], VOutput.Size);
  finally
    VBF.Free;
    VOutput.Free;
  end;
end;
{$ENDIF}

function StrToBase64(S: string): string;
var
  VSrcStream, VDestStream: TStringStream;
begin
  VSrcStream := TStringStream.Create(S);
  try
    VDestStream := TStringStream.Create('');
    try
      with TBase64EncodingStream.Create(VDestStream) do
        try
          CopyFrom(VSrcStream, VSrcStream.Size);
        finally
          Free;
        end;
      Result := VDestStream.DataString;
    finally
      VDestStream.Free;
    end;
  finally
    VSrcStream.Free;
  end;
end;

function Base64ToStr(const S: string; const AMode: TBase64DecodingMode): string;
var
  VDecoder: TBase64DecodingStream;
  VSrcStream, VDestStream: TStringStream;
begin
  VSrcStream := TStringStream.Create(S);
  try
    VDestStream := TStringStream.Create('');
    try
      VDecoder := TBase64DecodingStream.Create(VSrcStream, AMode);
      VDestStream.CopyFrom(VDecoder, VDecoder.Size);
    finally
      VDecoder.Free;
    end;
    Result := VDestStream.DataString;
  finally
    VDestStream.Free;
    VSrcStream.Free;
  end;
end;

function FileToBase64(const AFileName: TFileName): string;
var
  VFile: TFileStream;
  VStream: TStringStream;
begin
  VFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    VStream := TStringStream.Create('');
    with TBase64EncodingStream.Create(VStream) do
      try
        CopyFrom(VFile, VFile.Size);
      finally
        Free;
      end;
    Result := VStream.DataString;
  finally
    VStream.Free;
    VFile.free;
  end;
end;

procedure Base64ToFile(const S: string; const AFileName: TFileName;
  const AMode: TBase64DecodingMode);
var
  VFile: TFileStream;
  VStream: TStringStream;
  VDecoder: TBase64DecodingStream;
begin
  VFile := TFileStream.Create(AFileName, fmCreate);
  try
    VStream := TStringStream.Create(S);
    VStream.Position := 0;
    VDecoder := TBase64DecodingStream.Create(VStream, AMode);
    VFile.CopyFrom(VDecoder, VDecoder.Size);
  finally
    VDecoder.Free;
    VStream.Free;
    VFile.Free;
  end;
end;

end.
