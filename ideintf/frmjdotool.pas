(*
  JDO Tool unit
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

unit frmjdotool;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ComCtrls, EditBtn, ExtCtrls, SysUtils, Controls, Dialogs,
  Spin, ActnList, StdActns, Menus, SynHighlighterSQL, SynMemo, JDO;

type
  TfrJDOTool = class(TForm)
    alEdit: TActionList;
    btGenSQL: TButton;
    cbTableName: TComboBox;
    edInsert: TSynMemo;
    edDelete: TSynMemo;
    acSelAll: TEditSelectAll;
    acCopy: TEditCopy;
    edUpdate: TSynMemo;
    edTableAlias: TEdit;
    edConfig: TFileNameEdit;
    db: TJDODataBase;
    lbWrap: TLabel;
    edWrap: TSpinEdit;
    n1: TMenuItem;
    miCopy: TMenuItem;
    miSelAll: TMenuItem;
    pmEdit: TPopupMenu;
    sql: TJDOSQL;
    pnTop: TPanel;
    lbConfig: TLabel;
    lbTableName: TLabel;
    lbTableAlias: TLabel;
    pcClient: TPageControl;
    sqlsyn: TSynSQLSyn;
    edSelect: TSynMemo;
    tsSelect: TTabSheet;
    tsInsert: TTabSheet;
    tsUpdate: TTabSheet;
    tsDelete: TTabSheet;
    procedure acSelAllExecute(Sender: TObject);
    procedure btGenSQLClick(Sender: TObject);
    procedure cbTableNameEditingDone(Sender: TObject);
    procedure cbTableNameGetItems(Sender: TObject);
    procedure edTableAliasEditingDone(Sender: TObject);
    procedure edConfigEditingDone(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure Validate(const AExp: Boolean; const AMsg: string;
      const AControl: TWinControl);
  end;

var
  frJDOTool: TfrJDOTool;

implementation

{$R *.lfm}

procedure TfrJDOTool.edConfigEditingDone(Sender: TObject);
begin
  db.Configuration := edConfig.Text;
end;

procedure TfrJDOTool.FormShow(Sender: TObject);
begin
  sql.Query := db.Query;
end;

procedure TfrJDOTool.Validate(const AExp: Boolean; const AMsg: string;
  const AControl: TWinControl);
begin
  if not AExp then
  begin
    if AControl.CanFocus then
      AControl.SetFocus;
    ShowMessage(AMsg);
    Abort;
  end;
end;

procedure TfrJDOTool.cbTableNameGetItems(Sender: TObject);
var
  VItem: string;
begin
  VItem := cbTableName.Text;
  cbTableName.Clear;
  db.GetTableNames(cbTableName.Items);
  cbTableName.ItemIndex := cbTableName.Items.IndexOf(VItem);
end;

procedure TfrJDOTool.btGenSQLClick(Sender: TObject);
var
  VWrap: Integer;
begin
  Validate(edConfig.Text <> ES, 'Please specify a configuration.', edConfig);
  Validate(cbTableName.Text <> ES, 'Please specify a table name.', cbTableName);
  db.Query.Close;
  db.Query.FieldDefs.Clear;
  db.Query.SQL.Text := SQL_SELECT_TOKEN + SP + AK + SP + SQL_FROM_TOKEN + SP +
    cbTableName.Text + SP + SQL_NOTHING_WHERE_TOKEN;
  db.Query.Open;
  db.Query.Close;
  db.Query.SQL.Clear;
  sql.ComposeAll;
  VWrap := edWrap.Value;
  if VWrap > 0 then
  begin
    edSelect.Text := WrapText(db.Query.SQL.Text, VWrap);
    edInsert.Text := WrapText(db.Query.InsertSQL.Text, VWrap);
    edUpdate.Text := WrapText(db.Query.UpdateSQL.Text, VWrap);
    edDelete.Text := WrapText(db.Query.DeleteSQL.Text, VWrap);
  end
  else
  begin
    edSelect.Text := db.Query.SQL.Text;
    edInsert.Text := db.Query.InsertSQL.Text;
    edUpdate.Text := db.Query.UpdateSQL.Text;
    edDelete.Text := db.Query.DeleteSQL.Text;
  end;
end;

procedure TfrJDOTool.acSelAllExecute(Sender: TObject);
begin
  if ActiveControl is TSynMemo then
    (ActiveControl as TSynMemo).SelectAll;
end;

procedure TfrJDOTool.cbTableNameEditingDone(Sender: TObject);
begin
  sql.TableName := cbTableName.Text;
end;

procedure TfrJDOTool.edTableAliasEditingDone(Sender: TObject);
begin
  sql.TableAlias := edTableAlias.Text;
end;

end.

