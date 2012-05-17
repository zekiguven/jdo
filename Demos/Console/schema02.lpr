program schema02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
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
      q.Open;
      WriteLn(q.Schema);
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    db.Free;
  end;
end.

