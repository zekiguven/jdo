(*
  JDOQuery interface include
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

  EJDOQuery = class(EJDOException);

  TJDOQuery = class(TSQLQuery)
  private
    FDateAsString: Boolean;
  protected
    procedure ReadJSONObject(AJSON: TJSONObject);
    function CreateJSONObject: TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    function ToJSONArray: TJSONArray;
    function ToJSONObject: TJSONObject;
    procedure FromJSONArray(AJSON: TJSONArray);
    procedure FromJSONObject(AJSON: TJSONObject);
    function Execute: Boolean;
    function Open: Boolean;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
  end;

  TJDOQueryClass = class of TJDOQuery;
