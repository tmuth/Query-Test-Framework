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
alter session force parallel query parallel 128;
--alter session set parallel_degree_policy=AUTO;
alter session set statistics_level=ALL;

@^3

prompt about to run ^2

spool ^2..txt
set timing on

@^1

set timing off

column prev_sql_id new_value PREV_SQLID
select prev_sql_id from v$session where audsid=userenv('sessionid');

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
spool sqlmon_^2..html
select dbms_sqltune.report_sql_monitor(type=>'EM', sql_id=>'^PREV_SQLID') monitor_report from dual;
spool off

exit
