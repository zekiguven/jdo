(*
  JDO defines
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

{$mode objfpc}{$H+}
{$DEFINE JDO_INLINE}
{.$DEFINE JDO_CRYPT}
{$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 7) and (FPC_PATCH >= 0))}
  {$DEFINE JDO_NEW_FPC}
{$ENDIF}
