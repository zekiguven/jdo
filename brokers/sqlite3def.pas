unit SQLite3Def;

{$mode objfpc}{$H+}

interface

uses
  SQLdb, SQLite3Conn;

type
  TSQLite3ConnectionDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;

implementation

class function TSQLite3ConnectionDef.TypeName: string;
begin
  Result := 'SQLite3';
end;

class function TSQLite3ConnectionDef.ConnectionClass: TSQLConnectionClass;
begin
  Result := TSQLite3Connection;
end;

class function TSQLite3ConnectionDef.Description: string;
begin
  Result := 'Connect to a SQLite3 database directly via the client library';
end;

initialization
  RegisterConnection(TSQLite3ConnectionDef);

finalization
  UnRegisterConnection(TSQLite3ConnectionDef);

end.

