program test;

{$mode objfpc}{$H+}

uses
  JDO,
  JDOFieldHelper,
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
      q.SQL.Text := 'select * from t1';
      q.Open;

      WriteLn('AsChar: ', q.Field('id').AsChar);
      WriteLn('AsLowerStr: ', q.Field('dummy').AsLowerStr);
      WriteLn('AsUpperStr: ', q.Field('dummy').AsUpperStr);
      WriteLn('AsJSON: ', q.Field('dummy').AsJSON);

      q.Next;

      WriteLn('AsTrimStr: ', '-', q.Field('dummy').AsTrimStr, '-');

      q.Next;

      WriteLn('AsBase64: ', q.Field('dummy').AsBase64);

      db.Commit(False);
    except
      db.Rollback(False);
      raise;
    end;
  finally
    db.Free;
  end;
end.

