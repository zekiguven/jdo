(*
  JDO component editors unit
  Copyright (C) 2012-2014 Silvio Clecio.

  http://silvioprog.github.com/jdo/

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
  JDO, JDOConsts, JDOPropEdits, frmJDOAbout, frmJDOSQLTool, ComponentEditors,
  FieldsEditor, PropEdits, Dialogs, Controls, Menus,sysutils;

type

  { TJDOComponentEditor }

  TJDOComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TJDOConfiguratorComponentEditor }

  TJDOConfiguratorComponentEditor = class(TJDOComponentEditor)
  private
    procedure DoOpenDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TJDODataBaseComponentEditor }

  TJDODataBaseComponentEditor = class(TJDOComponentEditor)
  private
    procedure DoOpenDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TJDOSQLComponentEditor }

  TJDOSQLComponentEditor = class(TJDOComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(AIndex: Integer; const AItem: TMenuItem); override;
  end;

  { TJDOQueryComponentEditor }

  TJDOQueryComponentEditor = class(TFieldsComponentEditor)
  private
    procedure DoOpenDialog;
    procedure DoSaveDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

const
  VERB_ABOUT = 'About JDO ...';
  VERB_SQL_TOOL = 'JDO SQL Tool...';

implementation

{ TJDOComponentEditor }

procedure TJDOComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: TfrJDOAbout.Execute;
    2: TfrJDOSQLTool.Execute;
  end;
end;

function TJDOComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := VERB_ABOUT;
    1: Result := SMenuSep;
    2: Result := VERB_SQL_TOOL;
  end;
end;

function TJDOComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TJDOConfiguratorComponentEditor }

procedure TJDOConfiguratorComponentEditor.DoOpenDialog;
var
  VDialog: TOpenDialog;
  VCfg: TJDOConfigurator;
  VHook: TPropertyEditorHook;
begin
  VDialog := TOpenDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    VCfg := GetComponent as TJDOConfigurator;
    VCfg.Configuration := VDialog.FileName;
    GetHook(VHook);
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
    0..2: inherited;
    4: DoOpenDialog;
  end;
end;

function TJDOConfiguratorComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0..2: Result := inherited GetVerb(AIndex);
    3: Result := SMenuSep;
    4: Result := SOpenConfigFile;
  end;
end;

function TJDOConfiguratorComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TJDODataBaseComponentEditor }

procedure TJDODataBaseComponentEditor.DoOpenDialog;
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
    VDB := GetComponent as TJDODataBase;
    VIsConnected := VDB.Connected;
    VDB.Configuration := VDialog.FileName;
    VDB.Connected := VIsConnected;
    GetHook(VHook);
    if Assigned(VHook) then
    begin
      if VDB.ConnectorType <> ES then
      begin
        RemoveAllConnUnit;
        AddConnUnit(VDB.ConnectorType);
      end;
      VHook.Modified(Self);
      VHook.RefreshPropertyValues;
    end;
  finally
    VDialog.Free;
  end;
end;

procedure TJDODataBaseComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0..2: inherited;
    4: DoOpenDialog;
  end;
end;

function TJDODataBaseComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0..2: Result := inherited GetVerb(AIndex);
    3: Result := SMenuSep;
    4: Result := SOpenConfigFile;
  end;
end;

function TJDODataBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{ TJDOSQLComponentEditor }

procedure TJDOSQLComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VSQL: TJDOSQL;
  VHook: TPropertyEditorHook;
  VIsConnected, VIsActive: Boolean;
begin
  case AIndex of
    0..2: inherited;
    4..8:
      begin
        VSQL := GetComponent as TJDOSQL;
        if not Assigned(VSQL.Query) then
          Exit;
        if not Assigned(VSQL.Query.DataBase) then
          Exit;
        if MessageDlg(SGenSQLConfirm, mtConfirmation, mbYesNo, 0) <> mrYes then
          Exit;
        with VSQL do
        begin
          VIsConnected := Query.DataBase.Connected;
          VIsActive := Query.Active;
          try
            Query.Close;
            Compose(jstSelect);
            Query.Open;
            Query.Close;
            case AIndex of
              4: Compose(jstSelect);
              5: Compose(jstInsert);
              6: Compose(jstUpdate);
              7: Compose(jstDelete);
              8: ComposeAll;
            end;
            Reset;
            GetHook(VHook);
            if Assigned(VHook) then
            begin
              VHook.Modified(Self);
              VHook.RefreshPropertyValues;
            end;
            ShowMessage(SSQLGeneratedMsg);
          finally
            Query.DataBase.Connected := VIsConnected;
            Query.Active := VIsActive;
          end;
        end;
      end;
  end;
end;

function TJDOSQLComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0..2: Result := inherited GetVerb(AIndex);
    3: Result := SMenuSep;
    4: Result := SGenSelSQL;
    5: Result := SGenInsSQL;
    6: Result := SGenUpdSQL;
    7: Result := SGenDelSQL;
    8: Result := SGenAllSQL;
  end;
end;

function TJDOSQLComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 6;
end;

procedure TJDOSQLComponentEditor.PrepareItem(AIndex: Integer;
  const AItem: TMenuItem);
var
  VSQL: TJDOSQL;
begin
  if AIndex in [4..8] then
  begin
    VSQL := GetComponent as TJDOSQL;
    AItem.Enabled := Assigned(VSQL.Query) and Assigned(VSQL.Query.DataBase);
  end;
end;

{ TJDOQueryComponentEditor }

procedure TJDOQueryComponentEditor.DoOpenDialog;
var
  VQuery: TJDOQuery;
  VDialog: TOpenDialog;
  VHook: TPropertyEditorHook;
begin
  VDialog := TOpenDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    VQuery := GetComponent as TJDOQuery;
    VQuery.LoadJSONFromFile(VDialog.FileName);
    VQuery.Apply(False);
    ShowMessage(SJSONLoadedMsg);
    GetHook(VHook);
    if Assigned(VHook) then
    begin
      VHook.Modified(Self);
      VHook.RefreshPropertyValues;
    end;
  finally
    VDialog.Free;
  end;
end;

procedure TJDOQueryComponentEditor.DoSaveDialog;
var
  VQuery: TJDOQuery;
  VDialog: TSaveDialog;
begin
  VDialog := TSaveDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    VQuery := GetComponent as TJDOQuery;
    VQuery.SaveJSONToFile(VDialog.FileName);
  finally
    VDialog.Free;
  end;
end;

procedure TJDOQueryComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: inherited;
    2: TfrJDOAbout.Execute;
    4: TfrJDOSQLTool.Execute;
    6: DoOpenDialog;
    7: DoSaveDialog;
  end;
end;

function TJDOQueryComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := SMenuSep;
    2: Result := VERB_ABOUT;
    3: Result := SMenuSep;
    4: Result := VERB_SQL_TOOL;
    5: Result := SMenuSep;
    6: Result := SLoadJSONFileMsg;
    7: Result := SSaveJSONFileMsg;
  end;
end;

function TJDOQueryComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 7;
end;

end.
