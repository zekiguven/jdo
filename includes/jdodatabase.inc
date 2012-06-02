(*
  JDODataBase implementation include
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

constructor TJDODataBase.Create(AOwner: TComponent;
  const AConfiguration: string; const AConnect: Boolean);
begin
  inherited Create(AOwner);
  FConfig := TJDOConfigurator.Create(AConfiguration);
  InternalCreateConnection;
  InternalCreateTransaction;
  InternalCreateQuery;
  FConfig.Target := FConnection;
  FConfig.Configure;
  if AConnect then
    FConnection.Open;
end;

constructor TJDODataBase.Create(const AConfiguration: string;
  const AConnect: Boolean);
begin
  Create(nil, AConfiguration, AConnect);
end;

destructor TJDODataBase.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TJDODataBase.InternalCreateConnection;
var
  VConnType: ShortString;
  VConnDef: TConnectionDef;
begin
  VConnType := FConfig[CONNECTOR_TYPE];
  if Trim(VConnType) = ES then
    raise EJDODataBase.Create(Self, SEmptyConnTypeError);
  VConnDef := GetConnectionDef(VConnType);
  if Assigned(VConnDef) then
    FConnection := TSQLConnectionClass(VConnDef.ConnectionClass).Create(Self)
  else
    raise EJDODataBase.CreateFmt(Self, SConnUnitWasNotDeclaredError, [VConnType]);
end;

procedure TJDODataBase.InternalCreateTransaction;
begin
  FTransaction := TSQLTransaction.Create(Self);
  FTransaction.DataBase := FConnection;
end;

procedure TJDODataBase.InternalCreateQuery;
begin
  FQuery := TJDOQuery.Create(Self);
end;

procedure TJDODataBase.StartTrans(const ANativeError: Boolean);
begin
  if (not ANativeError) and FTransaction.Active then
    Exit;
  FTransaction.StartTransaction;
  if Assigned(FOnStartTrans) then
    FOnStartTrans(Self);
end;

procedure TJDODataBase.RestartTrans;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
  if Assigned(FOnRestartTrans) then
    FOnRestartTrans(Self);
end;

procedure TJDODataBase.Commit;
begin
  FTransaction.Commit;
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

procedure TJDODataBase.Rollback;
begin
  FTransaction.Rollback;
  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;
