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

constructor TJDOCustomDataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := TJDOConfigurator.Create(Self);
  Transaction := TSQLTransaction.Create(Self);
  FQuery := TJDOQuery.Create(Self);
  FConfig.Target := Self;
  FQuery.DataBase := Self;
  FQuery.Transaction := Transaction;
  Transaction.DataBase := Self;
  Transaction.Name := 'Transaction';
end;

constructor TJDOCustomDataBase.Create(AOwner: TComponent;
  const AConfiguration: string; const AConnect: Boolean);
begin
  Create(AOwner);
  FConfig.Configuration := AConfiguration;
  Connected := AConnect;
end;

procedure TJDOCustomDataBase.Loaded;
begin
  inherited Loaded;
  if (Trim(ConnectorType) <> ES) and not
    Assigned(GetConnectionDef(ConnectorType)) then
    raise EJDODataBase.CreateFmt(Self,
      SConnUnitWasNotDeclaredError, [ConnectorType]);;
end;

procedure TJDOCustomDataBase.Connect;
begin
  Connected := True;
end;

procedure TJDOCustomDataBase.Disconnect;
begin
  Connected := False;
end;

function TJDOCustomDataBase.GetConfiguration: string;
begin
  Result := FConfig.Configuration;
end;

procedure TJDOCustomDataBase.SetConfiguration(const AValue: string);
begin
  FConfig.Configuration := AValue;
end;

procedure TJDOCustomDataBase.SetCryptKey(AValue: string);
begin
  if AValue <> FCryptKey then
  begin
    FCryptKey := AValue;
    FConfig.CryptKey := AValue;
    FConfig.Configure;
  end;
end;

function TJDOCustomDataBase.InTransaction: Boolean;
begin
  Result := Transaction.Active;
end;

procedure TJDOCustomDataBase.StartTransaction(const ANativeError: Boolean);
begin
  if (not ANativeError) and Transaction.Active then
    Exit;
  Transaction.StartTransaction;
end;

procedure TJDOCustomDataBase.RestartTransaction;
begin
  if Transaction.Active then
    Transaction.Rollback;
  Transaction.StartTransaction;
end;

procedure TJDOCustomDataBase.Commit(const ARetaining: Boolean);
begin
  if ARetaining then
    Transaction.CommitRetaining
  else
    Transaction.Commit;
end;

procedure TJDOCustomDataBase.Rollback(const ARetaining: Boolean);
begin
  if ARetaining then
    Transaction.RollbackRetaining
  else
    Transaction.Rollback;
end;
