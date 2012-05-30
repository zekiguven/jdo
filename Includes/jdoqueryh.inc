(*
  JDOQuery interface include
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file Contributors.txt, included in this distribution.

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

  EJDOQuery = class(EJDOException);

  TJDOQuery = class(TSQLQuery)
  private
    FDateAsString: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function ToJSON: TJSONArray;
    procedure FromJSON(AJSON: TJSONArray);
    function Execute: Boolean;
    function Open: Boolean;
    property DateAsString: Boolean read FDateAsString write FDateAsString;
  end;

  TJDOQueryClass = class of TJDOQuery;
