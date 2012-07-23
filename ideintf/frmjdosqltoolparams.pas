(*
  JDO SQL Tool Params unit
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

unit frmJDOSQLToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, Buttons, DB;

type
  TfrJDOSQLToolParams = class(TForm)
    btOK: TBitBtn;
    btClose: TBitBtn;
    edParams: TValueListEditor;
    pnParams: TPanel;
    pnBotton: TPanel;
  public
    class procedure Execute(AParams: TParams);
  end;

implementation

{$R *.lfm}

class procedure TfrJDOSQLToolParams.Execute(AParams: TParams);
var
  I, C: Integer;
begin
  with Self.Create(nil) do
  try
    edParams.Strings.Clear;
    C := AParams.Count;
    for I := 0 to Pred(C) do
      edParams.Strings.Add(AParams[I].Name + '=');
    if ShowModal = mrOK then
      for I := 0 to Pred(edParams.Strings.Count) do
        AParams[I].Value := edParams.Strings.ValueFromIndex[I];
  finally
    Free;
  end;
end;

end.

