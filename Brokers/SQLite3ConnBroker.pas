unit SQLite3ConnBroker;

{$mode objfpc}{$H+}

interface

uses
  SQLdb, SQLite3Conn;

type

  { TSQLite3ConnDef }

  TSQLite3ConnDef = class(TConnectionDef)
    class function TypeName: string; override;
    class function ConnectionClass: TSQLConnectionClass; override;
    class function Description: string; override;
  end;

implementation

{ TSQLite3ConnDef }

class function TSQLite3ConnDef.TypeName: string;
begin
  Result := 'SQLite3';
end;

class function TSQLite3ConnDef.ConnectionClass: TSQLConnectionClass;
begin
  Result := TSQLite3Connection;
end;

class function TSQLite3ConnDef.Description: string;
begin
  Result := 'Connect to a SQLite3 database directly via the client library';
end;

initialization
  RegisterConnection(TSQLite3ConnDef);

finalization
  UnRegisterConnection(TSQLite3ConnDef);

end.

