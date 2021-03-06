program test;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  fpjson,
  sqlite3conn,
  sqldb,
  sysutils;

var
  db: TJDODataBase;
  q: TJDOQuery;
  a, r: TJSONArray;
  s: TJSONObject;
begin
  db := TJDODataBase.Create(nil, 'connectortype=sqlite3;databasename=db.sqlite3');
  q := TJDOQuery.Create(db);
  a := TJSONArray.Create;
  try
    db.StartTransaction(True);
    try
      q.SQL.Text := 'select * from t1 order by id';
      q.Open;

      WriteLn('Inserting records ...');
      a.Add(TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1']));
      a.Add(TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2']));
      q.Insert(a);
      q.ApplyUpdates(0);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing inserted records ...');
      q.Refresh;
      q.GetJSON(r);
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Editing records ...');
      a.Clear;
      a.Add(TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']));
      a.Add(TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']));
      q.Edit(a);
      q.ApplyUpdates(0);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing edited records ...');
      q.Refresh;
      q.GetJSON(r);
      WriteLn(r.AsJSON);
      FreeAndNil(r);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Deleting records ...');
      a.Clear;
      a.Add(TJSONObject.Create(['id', 1]));
      a.Add(TJSONObject.Create(['id', 2]));
      q.Delete(a);
      q.ApplyUpdates(0);
      WriteLn('Done.');

      WriteLn;

      WriteLn('Showing JSON schema ...');
      q.GetSchema(s);
      WriteLn(s.AsJSON);
      FreeAndNil(s);
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

