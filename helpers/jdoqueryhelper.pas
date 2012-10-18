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
      const AFields: TJSONObject): TJDOQuery;
    function Table(const ATableName: string;
      const AFields: array of string): TJDOQuery;
    function SetParam(AJSON: TJSONObject;
      const ADateAsString: Boolean = False): TJDOQuery;
    function GetField(AJSON: TJSONObject;
      const ADateAsString: Boolean = False): TJDOQuery;
    function Find(AJSON: TJSONObject;
      const ADateAsString: Boolean = False): Boolean;
  end;

implementation

procedure TJDOQueryHelper.CheckTableName(const ATableName: string);
begin
  if Trim(ATableName) = ES then
    raise EJDOQuery.Create(Self, SEmptyTableNameError);
end;

function TJDOQueryHelper.Table(const ATableName: string;
  const AFields: TJSONObject): TJDOQuery;
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
end;

function TJDOQueryHelper.Table(const ATableName: string;
  const AFields: array of string): TJDOQuery;
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
  inherited Close;
  SQL.Text := SQL_SELECT_TOKEN + SP + VFields + SP + SQL_FROM_TOKEN + SP +
    ATableName;
end;

function TJDOQueryHelper.SetParam(AJSON: TJSONObject;
  const ADateAsString: Boolean): TJDOQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TJDOCustomQuery.JSONToQuery(AJSON, Self, ADateAsString);
end;

function TJDOQueryHelper.GetField(AJSON: TJSONObject;
  const ADateAsString: Boolean): TJDOQuery;
begin
  Result := Self;
  CheckJSONParam(AJSON);
  TJDOCustomQuery.DataSetToJSON(Self, AJSON, ADateAsString);
end;

function TJDOQueryHelper.Find(AJSON: TJSONObject;
  const ADateAsString: Boolean): Boolean;
var
  I: Integer;
  VName, VFieldsParams: string;
begin
  CheckJSONParam(AJSON);
  VFieldsParams := ES;
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VFieldsParams += VName + SQL_EQ_PARAM_TOKEN + VName + SP +
      SQL_AND_TOKEN + SP;
  end;
  SetLength(VFieldsParams,
    Length(VFieldsParams) - Length(SP + SQL_AND_TOKEN + SP));
  if FieldDefs.Count = 0 then
    inherited Open;
  inherited Close;
  SQL.Add(SQL_WHERE_TOKEN + SP + VFieldsParams);
  TJDOCustomQuery.JSONToQuery(AJSON, Self, ADateAsString);
  Result := inherited Open;
end;

end.
