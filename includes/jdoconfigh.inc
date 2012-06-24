(*
  JDOConfig interface include
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

  EJDOConfig = class(EJDOException);

  TJDOCustomConfigurator = class(TComponent)
  private
{$IFDEF JDO_CRYPT}
    FCryptKey: string;
{$ENDIF}
    FConfiguration: string;
    FConfigFile: TStrings;
    FTarget: TComponent;
    function GetValues(AName: string): string;
    procedure SetConfiguration(const AValue: string);
    procedure SetTarget(AValue: TComponent);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent;
      const AConfiguration: string); overload;
    destructor Destroy; override;
    procedure Configure;
{$IFDEF JDO_CRYPT}
    class function Crypt(const AStr, AKey: string): string;
    class function DeCrypt(const AStr, AKey: string): string;
    property CryptKey: string read FCryptKey write FCryptKey;
{$ENDIF}
    property Configuration: string read FConfiguration write SetConfiguration;
    property Target: TComponent read FTarget write SetTarget;
    property Values[AName: string]: string read GetValues; default;
  end;

  TJDOCustomConfiguratorClass = class of TJDOCustomConfigurator;

  TJDOConfigurator = class(TJDOCustomConfigurator)
  published
    property Configuration;
{$IFDEF JDO_CRYPT}
    property CryptKey;
{$ENDIF}
    property Target;
  end;

  TJDOConfiguratorClass = class of TJDOConfigurator;
