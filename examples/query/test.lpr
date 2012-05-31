program test;

{$mode objfpc}{$H+}

uses
  JDO,
  FPJSON,
  SQLite3Conn,
  SysUtils;

var
  db: TJDODataBase;
  q: TJDOQuery;
  a, r: TJSONArray;
begin
  // you can also use: db := TJDODataBase.Create('connectortype=sqlite3;databasename=db.sqlite3');
  db := TJDODataBase.Create('db.cfg');

  // you can also use: q := db.Query;
  q := TJDOQuery.Create(db);

  a := TJSONArray.Create;
  db.StartTrans;
  try
    try
      WriteLn('Creating FieldDefs ...');
      q.SQL.Text := 'select * from t1';
      q.Prepare;
      q.FieldDefs.Update;
      WriteLn('Done.');

      WriteLn('Deleting all records ...');
      q.SQL.Text := 'delete from t1';
      q.Execute;
      WriteLn('Done.');

      WriteLn('Inserting records ...');
      a.Add(TJSONObject.Create(['dummy', 'Dummy string 1']));
      a.Add(TJSONObject.Create(['dummy', 'Dummy string 2']));
      q.SQL.Text := 'insert into t1 (dummy) values (:dummy)';
      q.FromJSON(a);
      WriteLn('Done.');

      WriteLn('Show inserted records ...');
      q.SQL.Text := 'select * from t1';
      r := q.ToJSON;
      q.Close;
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn('Editing records ...');
      a.Clear;
      a.Add(TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']));
      a.Add(TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']));
      q.SQL.Text := 'update t1 set dummy = :dummy where id = :id';
      q.FromJSON(a);
      WriteLn('Done.');

      WriteLn('Show edited records ...');
      q.SQL.Text := 'select * from t1';
      r := q.ToJSON;
      q.Close;
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn('Deleting records ...');
      q.SQL.Text := 'delete from t1 where id = :id';
      q.FromJSON(a);
      WriteLn('Done.');

      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    a.Free;
    db.Free;
  end;
end.

