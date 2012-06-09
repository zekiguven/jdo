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
    FConfiguration: string;
    FConfigFile: TStrings;
    FTarget: TComponent;
    function GetValues(AName: string): string;
    procedure SetConfiguration(const AValue: string);
    procedure SetTarget(AValue: TComponent);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure;
    property Target: TComponent read FTarget write SetTarget;
    property Configuration: string read FConfiguration write SetConfiguration;
    property Values[AName: string]: string read GetValues; default;
  end;

  TJDOCustomConfiguratorClass = class of TJDOCustomConfigurator;

  TJDOConfigurator = class(TJDOCustomConfigurator)
  published
    property Configuration;
    property Target;
  end;

  TJDOConfiguratorClass = class of TJDOConfigurator;
