(*
  JDO component editors unit
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

unit JDOCompEdits;

{$I jdo.inc}

interface

uses
  JDO, JDOPropEdits, ComponentEditors, PropEdits, Dialogs, Controls;

type
  TJDOConfiguratorComponentEditor = class(TDefaultComponentEditor)
  private
    procedure DoDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDODataBaseComponentEditor = class(TDefaultComponentEditor)
  private
    procedure DoDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDOSQLComponentEditor = class(TDefaultComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TJDOConfiguratorComponentEditor }

procedure TJDOConfiguratorComponentEditor.DoDialog;
var
  VDialog: TOpenDialog;
  VCfg: TJDOConfigurator;
  VHook: TPropertyEditorHook;
begin
  VDialog := TOpenDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    GetHook(VHook);
    VCfg := GetComponent as TJDOConfigurator;
    VCfg.Configuration := VDialog.FileName;
    if Assigned(VHook) then
    begin
      VHook.Modified(Self);
      VHook.RefreshPropertyValues;
    end;
  finally
    VDialog.Free;
  end;
end;

procedure TJDOConfiguratorComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: inherited;
    2: DoDialog;
  end;
end;

function TJDOConfiguratorComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := SMenuSep;
    2: Result := SOpenConfigFile;
  end;
end;

function TJDOConfiguratorComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TJDODataBaseComponentEditor }

procedure TJDODataBaseComponentEditor.DoDialog;
var
  VDB: TJDODataBase;
  VDialog: TOpenDialog;
  VIsConnected: Boolean;
  VHook: TPropertyEditorHook;
begin
  VDialog := TOpenDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    GetHook(VHook);
    VDB := GetComponent as TJDODataBase;
    VIsConnected := VDB.Connected;
    VDB.Configuration := VDialog.FileName;
    VDB.Connected := VIsConnected;
    if Assigned(VHook) then
    begin
      VHook.Modified(Self);
      VHook.RefreshPropertyValues;
      RemoveAllConnUnit;
      AddConnUnit(VDB.ConnectorType);
    end;
  finally
    VDialog.Free;
  end;
end;

procedure TJDODataBaseComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: inherited;
    2: DoDialog;
  end;
end;

function TJDODataBaseComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := SMenuSep;
    2: Result := SOpenConfigFile;
  end;
end;

function TJDODataBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TJDOSQLComponentEditor }

procedure TJDOSQLComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VSQL: TJDOSQL;
  VHook: TPropertyEditorHook;
  VIsConnected, VIsDataBase, VIsActive: Boolean;
begin
  case AIndex of
    0: inherited;
    2..6:
      begin
        VSQL := GetComponent as TJDOSQL;
        if not Assigned(VSQL.Query) then
          Exit;
        if MessageDlg(SGenSQLConfirm, mtConfirmation, mbYesNo, 0) <> mrYes then
          Exit;
        GetHook(VHook);
        with VSQL do
        begin
          try
            VIsDataBase := Assigned(Query.DataBase);
            if VIsDataBase then
              VIsConnected := Query.DataBase.Connected;
            VIsActive := Query.Active;
            Query.Close;
            Compose(jstSelect);
            Query.Open;
            Query.Close;
            case AIndex of
              1: Compose(jstSelect);
              2: Compose(jstInsert);
              3: Compose(jstUpdate);
              4: Compose(jstDelete);
              5: ComposeAll;
            end;
          finally
            Reset;
            if VIsDataBase then
              Query.DataBase.Connected := VIsConnected;
            Query.Active := VIsActive;
          end;
        end;
        if Assigned(VHook) then
          VHook.Modified(Self);
        ShowMessage(SSQLGeneratedMsg);
      end;
  end;
end;

function TJDOSQLComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := SMenuSep;
    2: Result := SGenSelSQL;
    3: Result := SGenInsSQL;
    4: Result := SGenUpdSQL;
    5: Result := SGenDelSQL;
    6: Result := SGenAllSQL;
  end;
end;

function TJDOSQLComponentEditor.GetVerbCount: Integer;
begin
  Result := 7;
end;

end.
