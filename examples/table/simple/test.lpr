program test;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  fpjson,
  sqlite3conn,
  sqldb;

var
  db: TJDODataBase;
  t: TJDOTable;
begin
  db := TJDODataBase.Create('connectortype=sqlite3;databasename=db.sqlite3');
  t := TJDOTable.Create(db, 't1');
  try
    t.Persisted := False;

    WriteLn('Deleting all records ...');
    t.AutoSetup := False;
    t.Mode := mtDelete;
    t.Post;
    t.AutoSetup := True;
    WriteLn('Done.');

    WriteLn('Inserting records ...');
    t.Mode := mtInsert;
    t.Add(TJSONObject.Create(['dummy', 'Dummy string 1']));
    t.Add(TJSONObject.Create(['dummy', 'Dummy string 2']));
    t.Post;
    WriteLn('Done.');

    WriteLn('Show inserted records ...');
    t.Reset;
    t.Open;
    t.Post;
    WriteLn(t.AsJSON);
    WriteLn('Done.');

    WriteLn('Editing records ...');
    t.Mode := mtUpdate;
    t.Add(TJSONObject.Create(['id', 1, 'dummy', 'Dummy string 1 - Edited']));
    t.Add(TJSONObject.Create(['id', 2, 'dummy', 'Dummy string 2 - Edited']));
    t.Post;
    WriteLn('Done.');

    WriteLn('Show edited records ...');
    t.Reset;
    t.Open;
    t.Post;
    WriteLn(t.AsJSON);
    WriteLn('Done.');

    WriteLn('Deleting records ...');
    t.Mode := mtDelete;
    t.AutoSetup := False;
    t.Post;
    WriteLn('Done.');

    t.Apply;
  finally
    t.Free;
    db.Free;
  end;
end.

