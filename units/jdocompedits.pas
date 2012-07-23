(*
  JDO component editors unit
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

unit JDOCompEdits;

{$I jdo.inc}

interface

uses
  JDO, JDOConsts, JDOPropEdits, frmJDOAbout, ComponentEditors, FieldsEditor,
  PropEdits, Dialogs, Controls;

type
  TJDOComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowAbout;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDOConfiguratorComponentEditor = class(TJDOComponentEditor)
  private
    procedure DoOpenDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDODataBaseComponentEditor = class(TJDOComponentEditor)
  private
    procedure DoOpenDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDOSQLComponentEditor = class(TJDOComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJDOQueryComponentEditor = class(TFieldsComponentEditor)
  private
    procedure DoShowAbout;
    procedure DoOpenDialog;
    procedure DoSaveDialog;
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

const
  VERB_ABOUT = 'About JDO ...';

implementation

{ TJDOComponentEditor }

procedure TJDOComponentEditor.DoShowAbout;
begin
  TfrJDOAbout.Execute;
end;

procedure TJDOComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    DoShowAbout;
end;

function TJDOComponentEditor.GetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := VERB_ABOUT;
end;

function TJDOComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
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
    2: DoOpenDialog;
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
    GetHook(VHook);
    VDB := GetComponent as TJDODataBase;
    VIsConnected := VDB.Connected;
    VDB.Configuration := VDialog.FileName;
    VDB.Connected := VIsConnected;
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
    0: inherited;
    2: DoOpenDialog;
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
  Result := inherited GetVerbCount + 2;
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
              2: Compose(jstSelect);
              3: Compose(jstInsert);
              4: Compose(jstUpdate);
              5: Compose(jstDelete);
              6: ComposeAll;
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
  Result := inherited GetVerbCount + 6;
end;

{ TJDOQueryComponentEditor }

procedure TJDOQueryComponentEditor.DoShowAbout;
begin
  TfrJDOAbout.Execute;
end;

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
    GetHook(VHook);
    VQuery := GetComponent as TJDOQuery;
    VQuery.LoadJSONFromFile(VDialog.FileName);
    VQuery.Apply(False);
    ShowMessage(SJSONLoadedMsg);
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
    2: DoShowAbout;
    4: DoOpenDialog;
    5: DoSaveDialog;
  end;
end;

function TJDOQueryComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := SMenuSep;
    2: Result := VERB_ABOUT;
    3: Result := SMenuSep;
    4: Result := SLoadJSONFileMsg;
    5: Result := SSaveJSONFileMsg;
  end;
end;

function TJDOQueryComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 5;
end;

end.
