(*
  JDO Classes unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOClasses;

{$I jdo.inc}

interface

uses
  JDOConsts, SysUtils;

type
  EJDOException = class(Exception)
  public
    constructor Create(AInstance: TObject; const AMsg: string);
    constructor CreateFmt(AInstance: TObject; const AMsg: string;
      const AArgs: array of const);
  end;

implementation

{ EJDOException }

constructor EJDOException.Create(AInstance: TObject; const AMsg: string);
begin
  inherited CreateFmt(ERROR_MASK, [AInstance.ClassName, AMsg]);
end;

constructor EJDOException.CreateFmt(AInstance: TObject; const AMsg: string;
  const AArgs: array of const);
begin
  inherited CreateFmt(Format(ERROR_MASK, [AInstance.ClassName, AMsg]), AArgs);
end;

end.

