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

constructor TJDOCustomConfigurator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigFile := TStringList.Create;
  FConfigFile.Delimiter := SC;
end;

constructor TJDOCustomConfigurator.Create(AOwner: TComponent;
  const AConfiguration: string);
begin
  Create(AOwner, AConfiguration);
end;

destructor TJDOCustomConfigurator.Destroy;
begin
  Target := nil;
  FConfigFile.Free;
  inherited Destroy;
end;

function TJDOCustomConfigurator.GetValues(AName: string): string;
begin
  Result := FConfigFile.Values[AName];
end;

procedure TJDOCustomConfigurator.SetConfiguration(const AValue: string);
begin
  if FConfiguration <> AValue then
  begin
    FConfiguration := AValue;
    if (Pos(SC, AValue) <> 0) or (Pos(EQ, AValue) <> 0) then
      FConfigFile.DelimitedText := AValue
    else
    begin
      if Trim(AValue) = ES then
        Exit;
      if not FileExists(AValue) then
        raise EJDOConfig.CreateFmt(Self, SCfgFileNotFoundError, [AValue]);
      FConfigFile.LoadFromFile(AValue);
    end;
    Configure;
  end;
end;

procedure TJDOCustomConfigurator.SetTarget(AValue: TComponent);
begin
  if (AValue = FTarget) or (AValue = Self) then
    Exit;
  if Assigned(FTarget) then
    FTarget.RemoveFreeNotification(Self);
  FTarget := AValue;
  if Assigned(FTarget) then
  begin
    FTarget.FreeNotification(Self);
    Configure;
  end;
end;

procedure TJDOCustomConfigurator.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    Configure;
end;

procedure TJDOCustomConfigurator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTarget) then
    FTarget := nil;
end;

procedure TJDOCustomConfigurator.Configure;
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
