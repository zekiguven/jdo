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
  JDOConsts,
  JDOCMDConsts,
  GetOpts,
  SysUtils,
  FPJSON,
  IBConnection,
  MYSQL40Conn,
  MYSQL41Conn,
  MYSQL50Conn,
  MYSQL51Conn,
  MYSQL55Conn,
  ODBCConn,
  Oracleconnection,
  PQConnection,
  SQLite3Conn,
  MSSQLConn;

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
  a: TJSONArray;
  c: char;
  json: boolean = False;
  isquery: boolean = False;
  cfg, tablename, fout, tablealias, ck, query: string;
  stmts: set of TJDOStatementType = [];

{$R *.res}

begin
  if ParamCount = 0 then
  begin
    WriteLn(sabout);
    whelp;
    Exit;
  end;

  c := #0;
  repeat
    c := GetOpt('c:t:siudo:a:jk:q:vh');
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
      'q': query := Trim(OptArg);
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

  isquery := query <> ES;

  valopt(cfg <> ES, 'c');
  if not isquery then
    valopt(tablename <> ES, 't');

  db := TJDODataBase.Create(nil);
  sql := TJDOSQL.Create(db, db.Query, tablename);
  try
    try
      db.CryptKey := ck;
      db.Configuration := cfg;

      // Execute query
      if isquery then
      begin
        db.Query.SQL.Text := query;
        db.Query.Open;
        db.Commit(False);
        try
          a := TJSONArray.Create;
          db.Query.GetJSON(a);
          WriteLn(a.FormatJSON);
        finally
          a.Free;
        end;
        Exit;
      end;

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
        WriteLn(db.Query.AsJSON);
      end;
    except
      on e: Exception do
      begin
        db.Rollback(False);
        WriteLn(e.ClassName, ': ', e.Message);
      end;
    end;
  finally
    db.Free;
  end;
end.
