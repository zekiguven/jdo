(*
  JDODataBase interface include
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

  EJDODataBase = class(EJDOException);

  TJDODataBase = class(TComponent)
  private
    FOnCommitRetaining: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnRollbackRetaining: TNotifyEvent;
    FTables: TFPObjectList;
    FConfig: TJDOConfigurator;
    FConnection: TSQLConnection;
    FOnCommit: TNotifyEvent;
    FOnRestartTrans: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnStartTrans: TNotifyEvent;
    FQuery: TJDOQuery;
    FTransaction: TSQLTransaction;
    function GetConnected: Boolean;
    function GetDatabaseName: string;
    function GetHostName: string;
    function GetPassword: string;
    function GetTables(AName: string): TJDOTable;
    function GetUserName: string;
    procedure InternalCreateConnection;
    procedure InternalCreateTransaction;
    procedure InternalCreateQuery;
    procedure SetDatabaseName(const AValue: string);
    procedure SetHostName(const AValue: string);
    procedure SetPassword(const AValue: string);
    procedure SetUserName(const AValue: string);
  public
    constructor Create(AOwner: TComponent; const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    constructor Create(const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function InTransaction: Boolean;
    procedure StartTrans(const ANativeError: Boolean = True);
    procedure RestartTrans;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    property Connection: TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TJDOQuery read FQuery;
    property Tables[AName: string]: TJDOTable read GetTables;
    property Connected: Boolean read GetConnected;
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property HostName: string read GetHostName write SetHostName;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnStartTrans: TNotifyEvent read FOnStartTrans write FOnStartTrans;
    property OnRestartTrans: TNotifyEvent read FOnRestartTrans write FOnRestartTrans;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnCommitRetaining: TNotifyEvent read FOnCommitRetaining
      write FOnCommitRetaining;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
    property OnRollbackRetaining: TNotifyEvent read FOnRollbackRetaining
      write FOnRollbackRetaining;
  end;

  TJDODataBaseClass = class of TJDODataBase;
