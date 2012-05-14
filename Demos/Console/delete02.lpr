program delete02;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  SysUtils,
  FPJSON,
  PQConnection;

resourcestring
  SCouldNotDelete = 'ERROR: Could not delete.';
  SSuccessfullyDeleted = '-- Successfully deleted! --';

var
  a: TJSONArray;
  j, j2: TJSONObject;
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  j := TJSONObject.Create;
  j2 := TJSONObject.Create;
  try
    db.StartTrans;
    try
      q.AddField('id', ftInt, True);
      j.Add('id', 1);
      a := TJSONArray.Create;
      a.Add(j);
      j2.Add('id', 2);
      a.Add(j2);
      if q.Delete(a) then
        WriteLn(SSuccessfullyDeleted)
      else
        WriteLn(SCouldNotDelete);
      db.Commit;
    except
      db.Rollback;
      WriteLn(SCouldNotDelete);
      raise;
    end;
  finally
    db.Free;
  end;
end.

