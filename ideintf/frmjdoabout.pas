(*
  JDO About unit
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

unit frmjdoabout;

{$I jdo.inc}

interface

uses
  JDOConsts, Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls,
  LCLIntf;

type
  TfrJDOAbout = class(TForm)
    btOK: TButton;
    gbLicense: TGroupBox;
    imLogo: TImage;
    lbAuthor: TLabel;
    lbAuthorLink: TLabel;
    lbCaption: TLabel;
    lbHomePage: TLabel;
    lbHomePageLink: TLabel;
    meLicense: TMemo;
    pnBody: TPanel;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure LinkMouseEnter(ASender: TObject);
    procedure LinkMouseLeave(ASender: TObject);
    procedure LinkClick(ASender: TObject);
  public
    class function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TfrJDOAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lbAuthorLink.OnMouseEnter := @LinkMouseEnter;
  lbAuthorLink.OnMouseLeave := @LinkMouseLeave;
  lbAuthorLink.OnClick := @LinkClick;
  lbHomePageLink.OnMouseEnter := @LinkMouseEnter;
  lbHomePageLink.OnMouseLeave := @LinkMouseLeave;
  lbHomePageLink.OnClick := @LinkClick;
  ActiveControl := btOK;
  KeyPreview := True;
  lbCaption.Caption := Format(PROJECT_DESCRIPTION + ' v%s', [VERSION]);
  lbAuthorLink.Caption := AUTHOR_LINK;
  lbHomePageLink.Caption := PROJECT_LINK;
  lbAuthorLink.Cursor := crHandPoint;
  lbHomePageLink.Cursor := crHandPoint;
  lbAuthorLink.Font.Color := clBlue;
  lbHomePageLink.Font.Color := clBlue;
end;

procedure TfrJDOAbout.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #27 then
  begin
    Close;
    Key := #0;
  end;
end;

procedure TfrJDOAbout.LinkMouseEnter(ASender: TObject);
var
  VLabel: TCustomLabel;
begin
  if ASender is TCustomLabel then
  begin
    VLabel := ASender as TCustomLabel;
    VLabel.Font.Style := [fsUnderline];
  end;
end;

procedure TfrJDOAbout.LinkMouseLeave(ASender: TObject);
var
  VLabel: TCustomLabel;
begin
  if ASender is TCustomLabel then
  begin
    VLabel := ASender as TCustomLabel;
    VLabel.Font.Style := [];
  end;
end;

procedure TfrJDOAbout.LinkClick(ASender: TObject);
var
  VLabel: TCustomLabel;
begin
  if ASender is TCustomLabel then
  begin
    VLabel := ASender as TCustomLabel;
    OpenURL(VLabel.Caption);
  end;
end;

class function TfrJDOAbout.Execute: Boolean;
begin
  with Self.Create(nil) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

end.

