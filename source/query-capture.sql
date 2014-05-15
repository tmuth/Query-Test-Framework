----------------------------------------------------------------------------------------
--
-- File name:   query-capture.sql
--
-- Purpose:     Used to run a query contained in a file and capture metrics on that query
--
-- Author:      Tyler D Muth
--
-- Usage:       This script takes in 4 parameters
--
-- Parameters:  1: The file name where the query is located
--
--				2: A name for this query
--
--				3: An alter session file used to change session parameters. You can also pass in 
--				   the name of an empty file such as null.sql
--
--				4: Output subdirectory, where the results are captured
--
--              See https://github.com/tmuth/Query-Test-Framework for additional information.
----------------------------------------------------------------------------------------- 


set SERVEROUTPUT off
set timing off
set wrap off
set define '^'
set concat '.'
set verify on
set pagesize 10000
set linesize 300
column name format a70
set numwidth 16
--alter session force parallel query parallel 128;
--alter session set parallel_degree_policy=AUTO;
alter session set statistics_level=ALL;

@^3

prompt about to run ^2

spool ^4/^2..txt
set timing on

@^1

set timing off

column prev_sql_id new_value PREV_SQLID
select prev_sql_id from gv$session where audsid=userenv('sessionid');

select s.name, m.value
  from v$mystat m, v$statname s
 where s.statistic# = m.statistic#
   and m.value > 0
 order by 1;




select * from table(dbms_xplan.display_cursor(sql_id => '^PREV_SQLID', format=>'ALLSTATS LAST'));

--select * from table(dbms_xplan.display_cursor(sql_id => '^PREV_SQLID', format=>'advanced +parallel +partition +predicate'));



spool off

set serveroutput ON SIZE 1000000 FORMAT WORD_WRAPPED
set wrap on
set verify off
set pagesize 0 echo off timing off linesize 1000 trimspool on trim on long 2000000 longchunksize 2000000 feedback off
spool ^4/^2..html
select dbms_sqltune.report_sql_monitor(type=>'EM', sql_id=>'^PREV_SQLID') monitor_report from dual;
spool off

exit
