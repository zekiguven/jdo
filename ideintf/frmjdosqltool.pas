(*
  JDO SQL Tool unit
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

unit frmJDOSQLTool;

{$I jdo.inc}

interface

uses
  JDO, JDOConsts, JDOIDEIntf, DB, Forms, StdCtrls, ComCtrls, EditBtn, ExtCtrls,
  SysUtils, Controls, Dialogs, Spin, ActnList, StdActns, Menus, Buttons,
  XMLPropStorage, DBGrids, SynHighlighterSQL, SynMemo, SynCompletion, Grids,
  Classes, Graphics;

type
  TfrJDOSQLTool = class(TForm)
    acGenAllSQL: TAction;
    acExecSQL: TAction;
    acCommit: TAction;
    acRollback: TAction;
    acExport: TAction;
    alTool: TActionList;
    btExport: TBitBtn;
    btCommit: TBitBtn;
    btRollback: TBitBtn;
    btExecSQL: TBitBtn;
    btClose: TBitBtn;
    btGenSQL: TBitBtn;
    cbTableName: TComboBox;
    cbFormated: TCheckBox;
    dsResult: TDatasource;
    acCut: TEditCut;
    acDelete: TEditDelete;
    acPaste: TEditPaste;
    acSelectAll: TEditSelectAll;
    acUndo: TEditUndo;
    grResult: TDBGrid;
    edInsert: TSynMemo;
    edDelete: TSynMemo;
    acCopy: TEditCopy;
    edUpdate: TSynMemo;
    edTableAlias: TEdit;
    edConfig: TFileNameEdit;
    db: TJDODataBase;
    imList: TImageList;
    lbWrap: TLabel;
    edWrap: TSpinEdit;
    edInsertStatistics: TMemo;
    edUpdateStatistics: TMemo;
    edDeleteStatistics: TMemo;
    edSelectStatistics: TMemo;
    miSelectAll: TMenuItem;
    n2: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miCut: TMenuItem;
    miUndo: TMenuItem;
    n1: TMenuItem;
    miCopy: TMenuItem;
    pnBotton: TPanel;
    pmEdit: TPopupMenu;
    sp1: TSplitter;
    sp2: TSplitter;
    sp3: TSplitter;
    sp4: TSplitter;
    sql: TJDOSQL;
    pnTop: TPanel;
    lbConfig: TLabel;
    lbTableName: TLabel;
    lbTableAlias: TLabel;
    pcClient: TPageControl;
    sqlsyn: TSynSQLSyn;
    edSelect: TSynMemo;
    scSQL: TSynCompletion;
    tsSelect: TTabSheet;
    tsInsert: TTabSheet;
    tsUpdate: TTabSheet;
    tsDelete: TTabSheet;
    xml: TXMLPropStorage;
    procedure acCommitExecute(Sender: TObject);
    procedure acExecSQLExecute(Sender: TObject);
    procedure acGenAllSQLExecute(Sender: TObject);
    procedure acRollbackExecute(Sender: TObject);
    procedure cbTableNameEditingDone(Sender: TObject);
    procedure cbTableNameGetItems(Sender: TObject);
    procedure edConfigAcceptFileName(Sender: TObject; Var Value: String);
    procedure edDeleteChange(Sender: TObject);
    procedure edInsertChange(Sender: TObject);
    procedure edSelectChange(Sender: TObject);
    procedure edTableAliasEditingDone(Sender: TObject);
    procedure edConfigEditingDone(Sender: TObject);
    procedure edUpdateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grResultDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure pcClientChange(Sender: TObject);
  private
    FBeginExec: TDateTime;
    FEndExec: TDateTime;
    procedure ShowResult(AEdit: TCustomMemo; ASplitter: TSplitter;
      AGrid: TCustomDBGrid);
    procedure HideResult(AEdit: TCustomMemo; ASplitter: TSplitter;
      AGrid: TCustomDBGrid);
    procedure HideAllResults;
    procedure UpdateExecAction(AEdit: TCustomSynMemo);
  public
    class procedure Execute;
    procedure Validate(const AExp: Boolean; const AMsg: string;
      const AControl: TWinControl);
  end;

const
  NULL_STR = '(NULL)';
  SEmptyConfig = 'Please specify a configuration.';
  SEmptyTableName = 'Please specify a table name.';

implementation

{$R *.lfm}

var
  _Form: TfrJDOSQLTool;

procedure TfrJDOSQLTool.FormCreate(Sender: TObject);
begin
  xml.FileName := GetExpertsConfigFileName;
  dsResult.DataSet := db.Query;
end;

procedure TfrJDOSQLTool.FormDestroy(Sender: TObject);
begin
  _Form := nil;
end;

procedure TfrJDOSQLTool.FormShow(Sender: TObject);
begin
  sql.Query := db.Query;
end;

procedure TfrJDOSQLTool.grResultDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  with Sender as TDBGrid do
  begin
    if Column.Field = nil then
      with Canvas do
      begin
        Font.Color := clBlue;
        TextRect(Rect, Rect.Left, Rect.Top, NULL_STR);
      end
    else
      if Column.Field.IsNull and ((Column.Field.DataType <> ftMemo) and
        (Column.Field.DataType <> ftBlob)) then
      begin
        with Canvas do
        begin
          Font.Color := clBlue;
          TextRect(Rect, Rect.Left, Rect.Top, NULL_STR);
        end;
      end;
  end;
  (Sender as TDBGrid).DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TfrJDOSQLTool.pcClientChange(Sender: TObject);
begin
  case pcClient.TabIndex of
    0:
      begin
        UpdateExecAction(edSelect);
        scSQL.Editor := edSelect;
      end;
    1:
      begin
        UpdateExecAction(edInsert);
        scSQL.Editor := edInsert;
      end;
    2:
      begin
        UpdateExecAction(edUpdate);
        scSQL.Editor := edUpdate;
      end;
    3:
      begin
        UpdateExecAction(edDelete);
        scSQL.Editor := edDelete;
      end;
  end;
end;

procedure TfrJDOSQLTool.edConfigEditingDone(Sender: TObject);
begin
  db.Configuration := edConfig.Text;
end;

procedure TfrJDOSQLTool.edUpdateChange(Sender: TObject);
begin
  UpdateExecAction(edUpdate);
end;

procedure TfrJDOSQLTool.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrJDOSQLTool.Validate(const AExp: Boolean; const AMsg: string;
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

procedure TfrJDOSQLTool.ShowResult(AEdit: TCustomMemo; ASplitter: TSplitter;
  AGrid: TCustomDBGrid);
begin
  AEdit.Clear;
  if pcClient.TabIndex = 0 then
    AEdit.Lines.Add('Records: ' + IntToStr(db.Query.RecordCount))
  else
    AEdit.Lines.Add('Rows affected: ' + IntToStr(db.Query.RowsAffected));
  AEdit.Lines.Add('Execution time: ' +
    FormatDateTime('hh:nn:ss:zzz', FEndExec - FBeginExec));
  AEdit.Show;
  if Assigned(AGrid) then
    AGrid.Show;
  if Assigned(ASplitter) then
    ASplitter.Show;
end;

procedure TfrJDOSQLTool.HideResult(AEdit: TCustomMemo; ASplitter: TSplitter;
  AGrid: TCustomDBGrid);
begin
  AEdit.Hide;
  if Assigned(ASplitter) then
    ASplitter.Hide;
  if Assigned(AGrid) then
    AGrid.Hide;
end;

procedure TfrJDOSQLTool.HideAllResults;
begin
  HideResult(edSelectStatistics, sp1, grResult);
  HideResult(edInsertStatistics, sp2, nil);
  HideResult(edUpdateStatistics, sp3, nil);
  HideResult(edDeleteStatistics, sp4, nil);
end;

procedure TfrJDOSQLTool.UpdateExecAction(AEdit: TCustomSynMemo);
begin
  acExecSQL.Enabled := Trim(AEdit.Text) <> ES;
end;

class procedure TfrJDOSQLTool.Execute;
begin
  if not Assigned(_Form) then
    _Form := Self.Create(nil);
  _Form.Show;
end;

procedure TfrJDOSQLTool.cbTableNameGetItems(Sender: TObject);
var
  item: string;
begin
  Validate(edConfig.Text <> ES, SEmptyConfig, edConfig);
  item := cbTableName.Text;
  cbTableName.Clear;
  db.GetTableNames(cbTableName.Items);
  cbTableName.ItemIndex := cbTableName.Items.IndexOf(item);
end;

procedure TfrJDOSQLTool.edConfigAcceptFileName(Sender: TObject; Var Value: String);
begin
  db.Configuration := Value;
end;

procedure TfrJDOSQLTool.edDeleteChange(Sender: TObject);
begin
  UpdateExecAction(edDelete);
end;

procedure TfrJDOSQLTool.edInsertChange(Sender: TObject);
begin
  UpdateExecAction(edInsert);
end;

procedure TfrJDOSQLTool.edSelectChange(Sender: TObject);
begin
  UpdateExecAction(edSelect);
end;

procedure TfrJDOSQLTool.acGenAllSQLExecute(Sender: TObject);
var
  wrap: Integer;
begin
  Validate(edConfig.Text <> ES, SEmptyConfig, edConfig);
  Validate(cbTableName.Text <> ES, SEmptyTableName, cbTableName);
  cbTableNameEditingDone(Sender);
  db.Query.Close;
  db.Query.FieldDefs.Clear;
  db.Query.SQL.Text := SQL_SELECT_TOKEN + SP + AK + SP + SQL_FROM_TOKEN + SP +
    cbTableName.Text + SP + SQL_NOTHING_WHERE_TOKEN;
  db.Query.Open;
  db.Query.Close;
  db.Query.SQL.Clear;
  sql.ComposeAll;
  db.Commit(False);
  wrap := edWrap.Value;
  if wrap > 0 then
  begin
    edSelect.Text := WrapText(db.Query.SQL.Text, wrap);
    edInsert.Text := WrapText(db.Query.InsertSQL.Text, wrap);
    edUpdate.Text := WrapText(db.Query.UpdateSQL.Text, wrap);
    edDelete.Text := WrapText(db.Query.DeleteSQL.Text, wrap);
  end
  else
  begin
    edSelect.Text := db.Query.SQL.Text;
    edInsert.Text := db.Query.InsertSQL.Text;
    edUpdate.Text := db.Query.UpdateSQL.Text;
    edDelete.Text := db.Query.DeleteSQL.Text;
  end;
  pcClientChange(Sender);
  HideAllResults;
end;

procedure TfrJDOSQLTool.acRollbackExecute(Sender: TObject);
begin
  db.Rollback(False);
  acCommit.Enabled := False;
  acRollback.Enabled := False;
  HideAllResults;
end;

procedure TfrJDOSQLTool.acExecSQLExecute(Sender: TObject);
begin
  Validate(edConfig.Text <> ES, SEmptyConfig, edConfig);
  cbTableNameEditingDone(Sender);
  db.Query.Close;
  db.StartTransaction(False);
  try
    case pcClient.TabIndex of
      0:
        begin
          if Trim(edSelect.Text) = ES then
            Exit;
          if edSelect.SelAvail then
            db.Query.SQL.Text := edSelect.SelText
          else
            db.Query.SQL.Text := edSelect.Text;
          FBeginExec := Now;
          db.Query.PacketRecords := -1;
          db.Query.Open;
          FEndExec := Now;
          ShowResult(edSelectStatistics, sp1, grResult);
        end;
      1:
        begin
          if Trim(edInsert.Text) = ES then
            Exit;
          if edInsert.SelAvail then
            db.Query.SQL.Text := edInsert.SelText
          else
            db.Query.SQL.Text := edInsert.Text;
          FBeginExec := Now;
          db.Query.Execute;
          FEndExec := Now;
          ShowResult(edInsertStatistics, sp2, nil);
        end;
      2:
        begin
          if Trim(edUpdate.Text) = ES then
            Exit;
          if edUpdate.SelAvail then
            db.Query.SQL.Text := edUpdate.SelText
          else
            db.Query.SQL.Text := edUpdate.Text;
          FBeginExec := Now;
          db.Query.Execute;
          FEndExec := Now;
          ShowResult(edUpdateStatistics, sp3, nil);
        end;
      3:
        begin
          if Trim(edDelete.Text) = ES then
            Exit;
          if edDelete.SelAvail then
            db.Query.SQL.Text := edDelete.SelText
          else
            db.Query.SQL.Text := edDelete.Text;
          FBeginExec := Now;
          db.Query.Execute;
          FEndExec := Now;
          ShowResult(edDeleteStatistics, sp4, nil);
        end;
    end;
    acCommit.Enabled := True;
    acRollback.Enabled := True;
  except
    db.Rollback(False);
    acCommit.Enabled := False;
    acRollback.Enabled := False;
    raise;
  end;
end;

procedure TfrJDOSQLTool.acCommitExecute(Sender: TObject);
begin
  db.Commit(False);
  acCommit.Enabled := False;
  acRollback.Enabled := False;
  HideAllResults;
end;

procedure TfrJDOSQLTool.cbTableNameEditingDone(Sender: TObject);
begin
  sql.TableName := cbTableName.Text;
end;

procedure TfrJDOSQLTool.edTableAliasEditingDone(Sender: TObject);
begin
  sql.TableAlias := edTableAlias.Text;
end;

end.

