program config01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  JDOConsts,
  PQConnection;

var
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create(
    'connectortype=postgresql;databasename=postgres;username=postgres;' +
    'password=postgres;hostname=127.0.0.1');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      if q.Open then
        WriteLn(q.AsJSON)
      else
        WriteLn('No record found');
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    db.Free;
  end;
end.

