(*
  JDOConfig implementation include
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
      raise EJDOConfig.CreateFmt(Self, SCfgFileNotFoundError, [FConfiguration]);
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
