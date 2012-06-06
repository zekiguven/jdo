(*
  JDOUtils interface include
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

function TimeOf(AData: TJSONData): TTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateOf(AData: TJSONData): TDate;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateTimeOf(AData: TJSONData): TDateTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function TimeOf(AData: TJSONData; const ADefValue: TTime): TTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateOf(AData: TJSONData; const ADefValue: TDate): TDate;{$IFDEF JDO_INLINE}inline;{$ENDIF}
function DateTimeOf(AData: TJSONData; const ADefValue: TDateTime): TDateTime;{$IFDEF JDO_INLINE}inline;{$ENDIF}
