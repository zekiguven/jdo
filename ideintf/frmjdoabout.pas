(*
  JDO About unit
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

unit frmjdoabout;

{$I jdo.inc}

interface

uses
  JDO, Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, StdCtrls;

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
  public
    class function Execute: boolean;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TfrJDOAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ActiveControl := btOK;
  KeyPreview := True;
  lbCaption.Caption := Format(PROJECT_DESCRIPTION + ' v%s', [VERSION]);
  lbAuthorLink.Caption := AUTHOR_LINK;
  lbHomePageLink.Caption := PROJECT_LINK;
  lbAuthorLink.Cursor := crHandPoint;
  lbHomePageLink.Cursor := crHandPoint;
  lbAuthorLink.Font.Color := clBlue;
  lbHomePageLink.Font.Color := clBlue;
  lbAuthorLink.Font.Style := [fsUnderline];
  lbHomePageLink.Font.Style := [fsUnderline];
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

class function TfrJDOAbout.Execute: boolean;
begin
  with Self.Create(nil) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

end.

