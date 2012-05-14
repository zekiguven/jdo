program delete01;

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
  j: TJSONObject;
  q: TJDOQuery;
  db: TJDODataBase;
begin
  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.AddField('id', ftInt, True);
      j := TJSONObject.Create(['id', 1]);
      if q.Delete(j) then
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

