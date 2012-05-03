program insert01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotInsert = 'ERROR: Could not insert.';
  SSuccessfullyInserted = '-- Successfully inserted! --';

var
  j: TJSONObject;
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  j := TJSONObject.Create;
  try
    db.StartTrans;
    try
      q.AddField('ftstr', ftStr);
      q.AddField('ftbool', ftBool);
      q.AddField('ftdate', ftDate);
      q.AddField('ftfloat', ftFloat);
      q.AddField('ftint', ftInt);
      j.Add('ftstr', 'CHIMBICA');
      j.Add('ftbool', True);
      j.Add('ftdate', Now);
      j.Add('ftfloat', 1.5);
      j.Add('ftint', 123);
      if q.Insert(j) then
        WriteLn(SSuccessfullyInserted)
      else
        WriteLn(SCouldNotInsert);
      db.Commit;
    except
      db.Rollback;
      WriteLn(SCouldNotInsert);
      raise;
    end;
  finally
    j.Free;
    q.Free;
    db.Free;
  end;
end.

