(*
  JDO defines
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

{$mode objfpc}{$H+}

{$DEFINE JDO_CRYPT}

{$IFNDEF win64}
  {$DEFINE HASMYSQL4CONNECTION}
  {$DEFINE HASORACLECONNECTION}
  {$DEFINE HASPQCONNECTION}
  {$DEFINE HASSQLITE3CONNECTION}
{$ENDIF}

{$IF FPC_FULLVERSION> 20402}
  {$DEFINE HASMYSQL51CONNECTION}
  {$DEFINE HASSQLPARSER}
{$ENDIF}

{$IF FPC_FULLVERSION>= 20601}
  {$DEFINE HASMYSQL55CONNECTION}
{$ENDIF}

{$IF FPC_FULLVERSION>= 20601}
  {$IF DEFINED(BEOS) OR DEFINED(HAIKU) OR DEFINED(LINUX) OR DEFINED(FREEBSD) OR DEFINED (NETBSD) OR DEFINED(OPENBSD) OR DEFINED(WIN32) OR DEFINED(WIN64)}
    // MS SQL Server and Sybase ASE connectors were introduced in the FPC 2.7 development branch, and
    // backported to 2.6.1
    // Operating systems should match FPC packages\fcl-db\fpmake.pp
    {$DEFINE HASMSSQLCONNECTION}
    {$DEFINE HASSYBASECONNECTION}
  {$ENDIF}
{$ENDIF}

{$IFNDEF Solaris}
  {$DEFINE HASIBCONNECTION}
{$ENDIF}
