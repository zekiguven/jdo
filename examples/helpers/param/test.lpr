program test;

{$mode objfpc}{$H+}

uses
  JDO,
  JDOParamHelper,
  sqlite3conn;

var
  db: TJDODataBase;
  q: TJDOQuery;
begin
  db := TJDODataBase.Create(nil, 'connectortype=sqlite3;databasename=db.sqlite3');
  q := TJDOQuery.Create(db);
  try
    db.StartTransaction(True);
    try
      q.SQL.Text := 'select * from t1 where dummy = :dummy';

      q.Param('dummy').AsTrimString := '  Dummy / string 1  ';
      q.Open;
      WriteLn(q.AsJSON);
      q.Close;

      q.Param('dummy').AsBase64 := 'ICBEdW1teSBzdHJpbmcgMiAg';
      q.Open;
      WriteLn(q.AsJSON);
      q.Close;

      db.Commit(False);
    except
      db.Rollback(False);
      raise;
    end;
  finally
    db.Free;
  end;
end.

