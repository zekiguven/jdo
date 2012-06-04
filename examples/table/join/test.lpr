program test;

{$mode objfpc}{$H+}

uses
  heaptrc,
  JDO,
  sqldb,
  sqlite3conn,
  fpjson;

var
  db: TJDODataBase;
  person, phone: TJDOTable;
begin
  db := TJDODataBase.Create('connectortype=sqlite3;databasename=db.sqlite3');
  try
    person := db.Tables['person'];
    phone := db.Tables['phone'];

    person.Persisted := False;
    phone.Persisted := False;

    person.Mode := mtInsert;
    person.Add(TJSONObject.Create(['id', 1, 'name', 'Silvio Clecio']));
    person.Add(TJSONObject.Create(['id', 2, 'name', 'Luciano Souza']));
    person.Post;

    phone.Mode := mtInsert;
    phone.Add(TJSONObject.Create(['id', 1, 'idperson', 1, 'number', '123456789']));
    phone.Add(TJSONObject.Create(['id', 2, 'idperson', 1, 'number', '987654321']));
    phone.Add(TJSONObject.Create(['id', 3, 'idperson', 2, 'number', '159478236']));
    phone.Add(TJSONObject.Create(['id', 4, 'idperson', 2, 'number', '874951632']));
    phone.Post;

    person.Reset;
    person.SQL.TableAlias := 'pe';
    person.SQL.Put('pe.name, ph.number', ptBegin);
    person.SQL.Put('inner join phone ph on ph.idperson = pe.id');
    person.Open;
    WriteLn(person.AsJSON);

    person.Apply;
  finally
    db.Free;
  end;
end.

