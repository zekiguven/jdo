program open01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SysUtils,
  FPJSON,
  SQLite3ConnBroker;

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
      WriteLn(q.AsJSON);
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

