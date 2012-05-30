(*
  JDODataBase interface include
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file Contributors.txt, included in this distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

  EJDODataBase = class(EJDOException);

  TJDODataBase = class(TComponent)
  private
    FConfig: TJDOConfigurator;
    FConnection: TSQLConnection;
    FOnCommit: TNotifyEvent;
    FOnRestartTrans: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnStartTrans: TNotifyEvent;
    FQuery: TJDOQuery;
    FTransaction: TSQLTransaction;
    procedure InternalCreateConnection;
    procedure InternalCreateTransaction;
    procedure InternalCreateQuery;
  public
    constructor Create(AOwner: TComponent; const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    constructor Create(const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    destructor Destroy; override;
    procedure StartTrans(const ANativeError: Boolean = True);
    procedure RestartTrans;
    procedure Commit;
    procedure Rollback;
    property Connection: TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TJDOQuery read FQuery;
    property OnStartTrans: TNotifyEvent read FOnStartTrans write FOnStartTrans;
    property OnRestartTrans: TNotifyEvent read FOnRestartTrans write FOnRestartTrans;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
  end;

  TJDODataBaseClass = class of TJDODataBase;
