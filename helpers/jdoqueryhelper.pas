(*
  JDOQueryHelper unit
  Copyright (C) 2012-2014 Silvio Clecio.

  http://silvioprog.github.com/jdo/

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOQueryHelper;

{$i jdo.inc}

interface

uses
  JDO, JDOConsts, DB, FPJSON, SysUtils;

type
  TJDOQueryHelper = class helper for TJDOQuery
  protected
    procedure CheckTableName(const ATableName: string);
  public
    function Table(const ATableName: string;
      const AFields: TJSONObject): TJDOCustomQuery;
    function Table(const ATableName: string;
      const AFields: array of string): TJDOCustomQuery;
    function SetParam(AJSON: TJSONObject;
      const ADateAsString: Boolean = False): TJDOCustomQuery;
    function GetField(AJSON: TJSONObject;
      const ADateAsString: Boolean = False): TJDOCustomQuery;
    function FindByJSON(const ATableName: string; AJSON: TJSONObject;
      const ADateAsString: Boolean = False): Boolean;
  end;

implementation

procedure TJDOQueryHelper.CheckTableName(const ATableName: string);
begin
  if Trim(ATableName) = ES then
    raise EJDOQuery.Create(Self, SEmptyTableNameError);
end;

function TJDOQueryHelper.Table(const ATableName: string;
  const AFields: TJSONObject): TJDOCustomQuery;
var
  I: Integer;
  VFields: string;
begin
  Result := Self;
  CheckJSONParam(AFields);
  CheckTableName(ATableName);
  VFields := ES;
  for I := 0 to Pred(AFields.Count) do
    VFields += AFields.Names[I] + CS;
  SetLength(VFields, Length(VFields) - 1);
  inherited Close;
  SQL.Text := SQL_SELECT_TOKEN + SP + VFields + SP + SQL_FROM_TOKEN + SP +
    ATableName;
  inherited Open;
end;

function TJDOQueryHelper.Table(const ATableName: string;
  const AFields: array of string): TJDOCustomQuery;
var
  VField, VFields: string;
begin
  Result := Self;
  CheckTableName(ATableName);
  if High(AFields) = 0 then
    raise EJDOQuery.Create(Self, SEmptyFieldsError);
  VFields := ES;
  for VField in AFields do
    VFields += VField + CS;
  SetLength(VFields, Length(VFields) - 1);
  Close;
  SQL.Text := SQL_SELECT_TOKEN + SP + VFields + SP + SQL_FROM_TOKEN + SP +
    ATableName + SP + SQL_NOTHING_WHERE_TOKEN;
  Open;
end;

function TJDOQueryHelper.SetParam(AJSON: TJSONObject;
  const ADateAsString: Boolean): TJDOCustomQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TJDOCustomQuery.JSONToQuery(AJSON, Self, ADateAsString);
end;

function TJDOQueryHelper.GetField(AJSON: TJSONObject;
  const ADateAsString: Boolean): TJDOCustomQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TJDOCustomQuery.DataSetToJSON(Self, AJSON, ADateAsString);
end;

function TJDOQueryHelper.FindByJSON(const ATableName: string; AJSON: TJSONObject;
  const ADateAsString: Boolean): Boolean;
var
  I: Integer;
  VName, VFields, VFieldsParams: string;
begin
  CheckTableName(ATableName);
  CheckJSONParam(AJSON);
  VFields := ES;
  VFieldsParams := ES;
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VFields += VName + CS;
    VFieldsParams += VName + SQL_EQ_PARAM_TOKEN + VName + SP +
      SQL_AND_TOKEN + SP;
  end;
  SetLength(VFields, Length(VFields) - 1);
  SetLength(VFieldsParams,
    Length(VFieldsParams) - Length(SP + SQL_AND_TOKEN + SP));
  if FieldDefs.Count = 0 then
  begin
    SQL.Text := SQL_SELECT_TOKEN + SP + VFields + SP + SQL_FROM_TOKEN + SP +
      ATableName + SP + SQL_NOTHING_WHERE_TOKEN;
    Open;
    Close;
  end;
  SQL.Text := SQL_SELECT_TOKEN + SP + VFields + SP + SQL_FROM_TOKEN + SP +
    ATableName + SP + SQL_WHERE_TOKEN + SP + VFieldsParams;
  TJDOCustomQuery.JSONToQuery(AJSON, Self, ADateAsString);
  Result := Open;
end;

end.
