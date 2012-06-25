(*
  JDO CLT program unit
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

program jdoapp;

{$mode objfpc}{$H+}

uses
  JDO,
  jdoconsts,
  getopts,
  SysUtils,
  IBConnection,
  mysql40conn,
  mysql41conn,
  mysql50conn,
  mysql51conn,
  mysql55conn,
  odbcconn,
  oracleconnection,
  pqconnection,
  sqlite3conn,
  mssqlconn;

  procedure valopt(const exp: boolean; const opt: char);
  begin
    if not exp then
    begin
      WriteLn(argv[0] + ': option -' + opt + ' is required');
      halt;
    end;
  end;

  procedure whelp;
  begin
    WriteLn(Format(shelp, [ParamStr(0)]));
  end;

var
  db: TJDODataBase;
  sql: TJDOSQL;
  c: char;
  json: boolean = False;
  cfg, tablename, fout, tablealias, ck: string;
  stmts: set of TJDOStatementType = [];
begin
  if ParamCount = 0 then
  begin
    WriteLn(sabout);
    whelp;
    Exit;
  end;

  c := #0;
  repeat
    c := GetOpt('c:t:siudo:a:jk:vh');
    case c of
      'c': cfg := OptArg;
      't': tablename := OptArg;
      's': Include(stmts, jstSelect);
      'i': Include(stmts, jstInsert);
      'u': Include(stmts, jstUpdate);
      'd': Include(stmts, jstDelete);
      'o': fout := OptArg;
      'a': tablealias := OptArg;
      'j': json := True;
      'k': ck := OptArg;
      'v':
        begin
          WriteLn(sversion);
          Exit;
        end;
      'h':
        begin
          whelp;
          Exit;
        end;
      '?', ':': Exit;
    end;
  until c = EndOfOptions;

  valopt(cfg <> ES, 'c');
  valopt(tablename <> ES, 't');

  db := TJDODataBase.Create(nil);
  sql := TJDOSQL.Create(db, db.Query, tablename);
  try
    try
      db.CryptKey := ck;
      db.Configuration := cfg;

      // Create FieldDefs
      db.Query.SQL.Text := SQL_SELECT_TOKEN + SP + AK + SP +
        SQL_FROM_TOKEN + SP + tablename + SP + SQL_NOTHING_WHERE_TOKEN;
      db.Query.Open;
      db.Query.Close;
      db.Query.SQL.Clear;

      sql.TableAlias := tablealias;

      // Generate SQL for select
      if jstSelect in stmts then
        sql.Compose(jstSelect);

      // Generate SQL for insert
      if jstInsert in stmts then
      begin
        sql.Compose(jstInsert);
        db.Query.SQL.Add(ES);
        db.Query.SQL.AddStrings(db.Query.InsertSQL);
      end;

      // Generate SQL for update
      if jstUpdate in stmts then
      begin
        sql.Compose(jstUpdate);
        db.Query.SQL.Add(ES);
        db.Query.SQL.AddStrings(db.Query.UpdateSQL);
      end;

      // Generate SQL for delete
      if jstDelete in stmts then
      begin
        sql.Compose(jstDelete);
        db.Query.SQL.Add(ES);
        db.Query.SQL.AddStrings(db.Query.DeleteSQL);
      end;

      if db.Query.SQL.Count > 0 then
      begin
        if fout <> ES then
          db.Query.SQL.SaveToFile(fout)
        else
          WriteLn(le + db.Query.SQL.Text);
      end;

      db.Query.SQL.Text := SQL_SELECT_TOKEN + SP + AK + SP +
        SQL_FROM_TOKEN + SP + tablename;
      if json then
      begin
        db.Query.Open;
        WriteLn(db.Query.JSON);
      end;
    except
      on e: Exception do
        WriteLn(e.ClassName, ': ', e.Message);
    end;
  finally
    db.Free;
  end;
end.
