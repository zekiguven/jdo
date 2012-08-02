(*
  JDO CLT consts unit
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

unit JDOCMDConsts;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

const
  le = LineEnding;
  sversion = '1.1';
  sdate = {$i %date%};
  scpu = {$i %fpctargetcpu%};
  sabout =
    'JDO Command Line Tool version ' + sversion + ' [' + sdate + '] for ' + scpu + le +
    'Copyright (c) 2010-2012 Silvio Clecio';
  shelp =
    '%s [options]' + le +
    '  -c    Specifies a string or a file to configure the connection (*)' + le +
    '  -t    Specifies the table name (*)' + le +
    '  -s    Generates SQL for select' + le +
    '  -i    Generates SQL for insert' + le +
    '  -u    Generates SQL for update' + le +
    '  -d    Generates SQL for delete' + le +
    '  -o    Specifies a file to output' + le +
    '  -a    Specifies an alias for the main table' + le +
    '  -j    Generates a JSON string or file' + le +
    '  -k    Specifies a key to decrypt the configuration' + le +
    '  -q    Specifies a SQL statement and run it in a query (JSON formated JSON output)' + le +
    '  -v    Shows version' + le +
    '  -h    Shows this help' + le + le +
    '  (*)   Required option';

implementation

end.
