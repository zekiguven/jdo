(*
  JSON Data Objects, consts unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/jdo

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit JDOConsts;

{$I jdo.inc}

interface

const
  AK = '*';
  CO = ':';
  CS = ', ';
  DT = '.';
  BS = '[ ';
  BE = ' ]';
  EQ = ' = ';
  ES = '';
  PE = ')';
  PS = '(';
  PO = '#';
  SP = ' ';
  JDO_DEFAULT_PRIMARY_KEY = 'id';
  JDO_CONNECTOR_NAME = 'connectorname';
  JDO_SQL_SELECT_TOKEN = 'select ';
  JDO_SQL_FROM_TOKEN = ' from ';
  JDO_SQL_WHERE_TOKEN = ' where ';
  JDO_SQL_ORDER_BY_TOKEN = ' order by ';
  JDO_SQL_INSERT_TOKEN = 'insert into ';
  JDO_SQL_VALUES_TOKEN = ' values ';
  JDO_SQL_UPDATE_TOKEN = 'update ';
  JDO_SQL_SET_TOKEN = ' set ';
  JDO_SQL_DELETE_TOKEN = 'delete';
  JDO_SQL_EQ_PARAM_TOKEN = ' = :';
  JDO_SQL_LOWER_TOKEN = 'lower';
  JDO_SQL_LIKE_TOKEN = ' like (:';
  JDO_FT_NULL = 'null';
  JDO_FT_STR = 'str';
  JDO_FT_BOOL = 'bool';
  JDO_FT_DATE = 'date';
  JDO_FT_FLOAT = 'float';
  JDO_FT_INT = 'int';

var
  SJDOPrimaryKeyEmptyError: string = '"PrimaryKey" must not be empty.';
  SJDOJSONObjectToParamsError: string = '"AJSONFiels.Count" may not be different from the "AJSONObject.Count".';
  SJDOConnectorNameEmptyError: string = '"ConnectorName" must not be empty.';
  SJDOConnectorUnitWasNotDeclaredError: string = 'The unit of "%s" was not declared in uses clause.';
  SJDOConfigFileNotFoundError: string = 'Config file not found: %s';
  SJDOInvalidPropInConfigFile: string = 'Invalid property in "%s" file: %s';

implementation

end.

