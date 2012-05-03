program open02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SysUtils,
  FPJSON,
  PQConnection;

var
  q: TJDOQuery;
  j: TJSONObject;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.AddField('ftstr', ftStr);
      q.AddField('ftdate', ftDate);
      if q.Open then // Or your SQL filter, e.g:  q.Open('id in (1, 2)');
      begin
        j := q[0];
        WriteLn('Objects: ', q.Count);
        WriteLn('Item 0: ', j.AsJSON);
        WriteLn('Item 1: ', q[1].AsJSON);
        WriteLn(
          'ftstr: ', j['ftstr'].AsString,
          ' ftdate: ', j['ftdate'].AsString);
        WriteLn('First object: ', q.First.AsJSON);
        WriteLn('Last object: ', q.Last.AsJSON);
        WriteLn('AsJSON: ', q.AsJSON);
      end
      else
        WriteLn('No record found');
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    q.Free;
    db.Free;
  end;
end.

