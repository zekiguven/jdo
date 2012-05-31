(*
  JSON Data Objects, consts unit
  Copyright (C) 2012-2014 Silvio Clecio.

  https://github.com/silvioprog/jdo

  All contributors:
  Plase see the file Contributors.txt, included in this distribution.

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
  ES = '';
  SP = #32;
  PT = #37; // %
  AK = #42; // *
  CO = #58; // :
  CS = #44; // ,
  DT = #46; // .
  BS = #91; // [
  BE = #93; // ]
  EQ = #61; // =
  PE = #41; // )
  PS = #40; // (
  PO = #35; // #
  SC = #59; // ;
  DEFAULT_PRIMARY_KEY = 'id';
  CONNECTOR_TYPE = 'connectortype';
  SQL_SELECT_TOKEN = 'select ';
  SQL_FROM_TOKEN = 'from ';
  SQL_WHERE_TOKEN = ' where ';
  SQL_ORDER_BY_TOKEN = 'order by ';
  SQL_INSERT_TOKEN = 'insert into ';
  SQL_VALUES_TOKEN = ' values ';
  SQL_UPDATE_TOKEN = 'update ';
  SQL_SET_TOKEN = ' set ';
  SQL_DELETE_TOKEN = 'delete ';
  SQL_EQ_PARAM_TOKEN = '=:';
  SQL_LOWER_TOKEN = 'lower';
  SQL_LIKE_TOKEN = ' like (:';
  FT_NULL = 'null';
  FT_STRING = 'string';
  FT_BOOLEAN = 'boolean';
  FT_DATE = 'date';
  FT_FLOAT = 'float';
  FT_INT = 'int';
  ERROR_MASK = '%s: %s';
  NullDate = 0;

  // Error msgs
  SKeyEmptyError = '"Key" must not be empty.';
  SConnTypeEmptyError = '"ConnectorType" must not be empty.';
  SConnUnitWasNotDeclaredError = 'The unit of "%s" was not declared in uses clause or it is an invalid ConnectorType.';
  SCfgFileNotFoundError = 'Config file not found: "%s"';
  STargetNilError = '"Target" must not be nil.';
  STableNameEmptyError = '"TableName" must be not empty.';
  SFieldDefsNilError = '"FieldDefs" must be not nil.';
  SFieldDefNotFound = 'FieldDef not found: "%s".';

implementation

end.

