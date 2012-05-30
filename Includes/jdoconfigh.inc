(*
  JDOConfig interface include
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

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

  TJDOConfiguratorClass = class of TJDOConfigurator;
