(*
  JDODataBase interface include
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

  EJDODataBase = class(EJDO);

  TJDOCustomDataBase = class(TSQLConnector, IJDOAboutComponent)
  private
    FConfig: TJDOConfigurator;
    FQuery: TJDOQuery;
{$IFDEF JDO_CRYPT}
    FCryptKey: string;
    procedure SetCryptKey(AValue: string);
{$ENDIF}
    function GetAbout: string;
    function GetConfiguration: string;
    procedure SetAbout({%H-}AValue: string);
    procedure SetConfiguration(const AValue: string);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AConfiguration: string;
      const AConnect: Boolean = True); overload;
    procedure Connect;
    procedure Disconnect;
    function InTransaction: Boolean;
    procedure StartTransaction(const ANativeError: Boolean); overload;
    procedure RestartTransaction;
    procedure Commit(const ARetaining: Boolean); overload;
    procedure Rollback(const ARetaining: Boolean); overload;
    property About: string read GetAbout write SetAbout stored False;
{$IFDEF JDO_CRYPT}
    property CryptKey: string read FCryptKey write SetCryptKey;
{$ENDIF}
    property Configuration: string read GetConfiguration write SetConfiguration;
    property Query: TJDOQuery read FQuery;
  end;

  TJDOCustomDataBaseClass = class of TJDOCustomDataBase;

  TJDODataBase = class(TJDOCustomDataBase)
  published
    property About;
{$IFDEF JDO_CRYPT}
    property CryptKey;
{$ENDIF}
    property Configuration;
  end;

  TJDODataBaseClass = class of TJDODataBase;
