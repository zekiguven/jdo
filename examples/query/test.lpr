program test;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  FPJSON,
  SQLite3Conn,
  SysUtils;

var
  db: TJDODataBase;
  q: TJDOQuery;
  s: string;
  a, r: TJSONArray;
begin
  // you can also use: db := TJDODataBase.Create('connectortype=sqlite3;databasename=db.sqlite3');
  db := TJDODataBase.Create(nil, 'db.cfg');

  // you can also use: q := db.Query;
  q := TJDOQuery.Create(db);

  a := TJSONArray.Create;
  try
    db.StartTransaction(True);
    try
      WriteLn('Creating FieldDefs ...');
      q.SQL.Text := 'select * from t1';
      q.Prepare;
      q.FieldDefs.Update;
      WriteLn('Done.');

      WriteLn;

      WriteLn('Deleting all records ...');
      q.SQL.Text := 'delete from t1';
      q.Execute;
      WriteLn('Done.');

      WriteLn;

      WriteLn('Inserting records ...');
      a.Add(TJSONObject.Create(['dummy', 'Dummy string 1']));
      a.Add(TJSONObject.Create(['dummy', 'Dummy string 2']));
      q.SQL.Text := 'insert into t1 (dummy) values (:dummy)';
      q.SetJSON(a);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing inserted records ...');
      q.SQL.Text := 'select * from t1';
      q.GetJSON(r);
      q.Close;
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Editing records ...');
      a.Clear;
      a.Add(TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']));
      a.Add(TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']));
      q.SQL.Text := 'update t1 set dummy = :dummy where id = :id';
      q.SetJSON(a);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing edited records ...');
      q.SQL.Text := 'select * from t1';
      q.GetJSON(r);
      q.Close;
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Deleting records ...');
      q.SQL.Text := 'delete from t1 where id = :id';
      q.SetJSON(a);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing JSON schema ...');
      q.GetSchema(s);
      WriteLn(s);
      WriteLn('Done.');

      WriteLn;

      db.Commit(False);
    except
      db.Rollback(False);
      raise;
    end;
  finally
    a.Free;
    db.Free;
  end;
end.

