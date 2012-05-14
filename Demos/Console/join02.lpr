program join02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SysUtils,
  FPJSON,
  PQConnection;

const
  SQL_JOIN =
    'left join jdo_demo_detail on jdo_demo_detail.jdodemoid = jdo_demo.id';

var
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.TableAlias := q.TableName;
      if q.Open(SQL_JOIN) then
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

