(*
  JDOConsts unit
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo/

  All contributors:
  Plase see the file CONTRIBUTORS, included in this distribution.

  See the file LICENSE, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOConsts;

{$i jdo.inc}

interface

const
  VERSION = '6.17'; // RT version
  AUTHOR_LINK = 'https://github.com/silvioprog/';
  PROJECT_LINK = 'https://github.com/silvioprog/jdo/';
  PROJECT_DESCRIPTION = 'JSON Data Objects';
  CR = #13;
  LF = #10;
  CRLF = CR + LF;
  ES = '';
  SP = #32;
  DS = #36; // $
  PT = #37; // %
  AK = #42; // *
  CO = #58; // :
  CS = #44; // ,
  DT = #46; // .
  EQ = #61; // =
  PE = #41; // )
  PS = #40; // (
  PO = #35; // #
  SC = #59; // ;
  SQL_SELECT_TOKEN = 'select';
  SQL_FROM_TOKEN = 'from';
  SQL_WHERE_TOKEN = 'where';
  SQL_NOTHING_WHERE_TOKEN = SQL_WHERE_TOKEN + SP + '1=2';
  SQL_ORDER_BY_TOKEN = 'order by';
  SQL_INSERT_TOKEN = 'insert into';
  SQL_VALUES_TOKEN = 'values';
  SQL_UPDATE_TOKEN = 'update';
  SQL_SET_TOKEN = 'set';
  SQL_DELETE_TOKEN = 'delete';
  SQL_EQ_PARAM_TOKEN = ' = :';
  SQL_LOWER_TOKEN = 'lower';
  SQL_LIKE_TOKEN = 'like';
  SQL_AND_TOKEN = 'and';
  FT_NULL = 'null';
  FT_STRING = 'string';
  FT_BOOLEAN = 'boolean';
  FT_DATE = 'date';
  FT_FLOAT = 'float';
  FT_INT = 'int';
  ERROR_MASK = '%s: %s';
  NullDate = 0;

  SMenuSep = '-';
  SOpenConfigFile = 'Open configuration file ...';
  SGenSelSQL = 'Generates SQL for select ...';
  SGenInsSQL = 'Generates SQL for insert ...';
  SGenUpdSQL = 'Generates SQL for update ...';
  SGenDelSQL = 'Generates SQL for delete ...';
  SGenAllSQL = 'Generates all SQL ...';
  SGenSQLConfirm = 'Generates SQL?';
  SSQLGeneratedMsg = 'SQL generated successfully!';
  SLoadJSONFileMsg = 'Load JSON from file ...';
  SSaveJSONFileMsg = 'Save JSON to file ...';
  SJSONLoadedMsg = 'JSON loaded successfully!';

  // Error msgs
  SEmptyServerIndexDefsCountError = '"ServerIndexDefs.Count" must not be "0".';
  SEmptyFieldDefsCountError = '"FieldDefs.Count" must not be "0".';
  SEmptyConnTypeError = 'The "ConnectorType" must not be empty.';
  SConnUnitWasNotDeclaredError = 'The unit for "%s" was not declared in uses clause or it''s an invalid ConnectorType.';
  SCfgFileNotFoundError = 'The config file was not found: "%s"';
  SNilQueryError = 'The "Query" must not be nil.';
  SEmptyTableNameError = 'The "TableName" must not be empty.';
  SNilFieldDefsError = '"FieldDefs" must not be nil.';
  SNilJSONParamError = '"AJSON" must not be nil.';
  SEmptyTableError = 'Empty table.';
  SEmptyObjectError = 'Empty object.';
  SPrimaryKeyNotFound = 'Primary key not found.';

implementation

end.
