program schema01;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  FPJSON,
  PQConnection;

var
  q: TJDOQuery;
  db: TJDODataBase;
  metaData: TJSONObject;
begin

{
  Based on:
  http://docs.sencha.com/ext-js/3-4/#!/api/Ext.data.JsonReader
}


  db := TJDODataBase.Create('db.cfg');
  q := TJDOQuery.Create(db, 'jdo_demo');
  try
    db.StartTrans;
    try
      q.Open;
      metaData := TJSONObject.Create(
        ['metaData', q.JSONSchema, 'rows', q.AsJSONArray]);
      metaData.Objects['metaData'].Add('root', 'rows');
      WriteLn(metaData.FormatJSON);
      db.Commit;
    except
      db.Rollback;
      raise;
    end;
  finally
    metaData.Free;
    db.Free;
  end;
end.

