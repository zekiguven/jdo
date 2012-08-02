program test;

{$mode objfpc}{$H+}

uses
  JDO,
  FieldCHelper,
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
      WriteLn('AsLowerString: ', q.Field('dummy').AsLowerString);
      WriteLn('AsUpperString: ', q.Field('dummy').AsUpperString);
      WriteLn('AsJSON: ', q.Field('dummy').AsJSON);

      q.Next;

      WriteLn('AsTrimString: ', '-', q.Field('dummy').AsTrimString, '-');

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

