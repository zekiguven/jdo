program like01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  JDOConsts,
  SysUtils,
  FPJSON,
  PQConnection;

var
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.OrderBy := ES;
      q.Like('o', 'ftstr', [loCaseInsensitive, loPartialKey]);
      if q.Open('order by ftstr desc') then
        WriteLn(q.AsJSON)
      else
        WriteLn('No record found');
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    q.Free;
    db.Free;
  end;
end.

