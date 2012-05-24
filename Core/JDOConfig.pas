(*
  JDO Config unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOConfig;

{$I jdo.inc}

interface

uses
  JDOConsts, JDOClasses, SysUtils, Classes, TypInfo;

type
  EJDOConfig = class(EJDOException);

  TJDOConfigurator = class
  private
    FConfigFile: TStrings;
    FConfiguration: string;
    FTarget: TPersistent;
    function GetConfiguration: string;
    function GetValues(AName: string): string;
    procedure SetConfiguration(const AValue: string);
  public
    constructor Create(const AConfiguration: string);
    destructor Destroy; override;
    procedure Configure;
    procedure Load;
    property Target: TPersistent read FTarget write FTarget;
    property Configuration: string read GetConfiguration write SetConfiguration;
    property Values[AName: string]: string read GetValues; default;
  end;

implementation

{ TJDOConfigurator }

constructor TJDOConfigurator.Create(const AConfiguration: string);
begin
  FConfigFile := TStringList.Create;
  FConfigFile.Delimiter := SC;
  FConfiguration := AConfiguration;
  Load;
end;

destructor TJDOConfigurator.Destroy;
begin
  FConfigFile.Free;
  inherited Destroy;
end;

function TJDOConfigurator.GetConfiguration: string;
begin
  Result := FConfigFile.CommaText;
end;

function TJDOConfigurator.GetValues(AName: string): string;
begin
  Result := FConfigFile.Values[AName];
end;

procedure TJDOConfigurator.SetConfiguration(const AValue: string);
begin
  FConfiguration := AValue;
  FConfigFile.CommaText := AValue;
end;

procedure TJDOConfigurator.Load;
begin
  if (Pos(SC, FConfiguration) <> 0) and (Pos(EQ, FConfiguration) <> 0) then
    FConfigFile.DelimitedText := FConfiguration
  else
  begin
    if not FileExists(FConfiguration) then
      raise EJDOConfig.CreateFmt(Self, SConfigFileNotFoundError,
        [FConfiguration]);
    FConfigFile.LoadFromFile(FConfiguration);
  end;
end;

procedure TJDOConfigurator.Configure;
var
  I: Integer;
  VPropName, VToken: ShortString;
begin
  if not Assigned(FTarget) then
    raise EJDOConfig.Create(Self, SNilTargetError);
  for I := 0 to Pred(FConfigFile.Count) do
  begin
    VPropName := FConfigFile.Names[I];
    VToken := Copy(VPropName, 1, 1);
    if (VToken = PO) or (VToken = ES) then
      Continue;
    if IsPublishedProp(FTarget, VPropName) then
      SetPropValue(FTarget, VPropName, FConfigFile.Values[VPropName]);
  end;
end;

end.

