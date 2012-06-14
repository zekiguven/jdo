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
  JDO, ComponentEditors, PropEdits, Dialogs, Controls;

type
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

procedure TJDODataBaseComponentEditor.DoDialog;
var
  VDB: TJDODataBase;
  VDialog: TOpenDialog;
  VHook: TPropertyEditorHook;
begin
  VDialog := TOpenDialog.Create(nil);
  try
    if not VDialog.Execute then
      Exit;
    GetHook(VHook);
    VDB := GetComponent as TJDODataBase;
    VDB.Configuration := VDialog.FileName;
    if Assigned(VHook) then
    begin
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
    1: DoDialog;
  end;
end;

function TJDODataBaseComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := 'Open configuration file ...';
  end;
end;

function TJDODataBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TJDOSQLComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VSQL: TJDOSQL;
  VHook: TPropertyEditorHook;
  VIsConnected, VIsDataBase, VIsActive: Boolean;
begin
  case AIndex of
    0: inherited;
    1:
      begin
        if MessageDlg('Generate SQL statements?', mtConfirmation,
          mbYesNo, 0) <> mrYes then
          Exit;
        GetHook(VHook);
        VSQL := GetComponent as TJDOSQL;
        with VSQL do
        begin
          if not Assigned(VSQL.Query) then
            Exit;
          try
            VIsDataBase := Assigned(Query.DataBase);
            if VIsDataBase then
              VIsConnected := Query.DataBase.Connected;
            VIsActive := Query.Active;
            Query.Close;
            Compose(jstSelect);
            Query.Open;
            Query.Close;
            ComposeAll;
          finally
            Reset;
            if VIsDataBase then
              Query.DataBase.Connected := VIsConnected;
            Query.Active := VIsActive;
          end;
        end;
        if Assigned(VHook) then
          VHook.Modified(Self);
        ShowMessage('SQL statements were generated successfully!');
      end;
  end;
end;

function TJDOSQLComponentEditor.GetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := inherited GetVerb(AIndex);
    1: Result := 'Generate SQL statements ...';
  end;
end;

function TJDOSQLComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
