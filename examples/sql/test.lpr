program test;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SQLite3Conn;

var
  db: TJDODataBase;
  q: TJDOQuery;
  sql: TJDOSQL;
begin
  db := TJDODataBase.Create(nil, 'connectortype=sqlite3;databasename=db.sqlite3');
  q := TJDOQuery.Create(db);
  sql := TJDOSQL.Create(q, q, 't1');
  try
    db.StartTransaction(True);
    try
      WriteLn('Creating FieldDefs ...');
      WriteLn;
      sql.Query := q;
      sql.Compose(jstSelect);
      q.Open;
      q.Close;

      WriteLn('All SQL statements generated:');
      WriteLn;
      sql.ComposeAll;
      WriteLn(q.SQL.Text);
      WriteLn(q.InsertSQL.Text);
      WriteLn(q.UpdateSQL.Text);
      WriteLn(q.DeleteSQL.Text);

      db.Commit(False);
    except
      db.Rollback(False);
      raise;
    end;
  finally
    db.Free;
  end;
end.

