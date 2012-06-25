unit jdoconsts;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

const
  le = LineEnding;
  sversion = '1.0';
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
    '  -v    Shows version' + le +
    '  -h    Shows this help' + le + le +
    '  (*)   Required option';

implementation

end.
