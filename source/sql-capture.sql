rem Use the following perl command to append a ; to the end of each file
rem perl -i.orig -0pe 's/$/;/' *.sql
rem This replaces the 1st select to add a monitor hint
rem perl -i.orig -0pe 's/select /select \/\*\+ monitor gather_plan_statistics \+\//i' *.sql



accept sql_id -
       prompt 'Enter value for sql_id: ' 
accept query_name -
	   prompt 'Enter a name for this query:' 
	   
set head off
set linesize 9999
set pagesize 9999
set verify off
set long 500000
spool &&query_name..sql
prompt --sql_id &&sql_id
select sql_fulltext from GV$SQLAREA where sql_id = '&&sql_id';
spool off
set head on





column spool_dest noprint new_value spooldest
SELECT DECODE(COUNT(*),0,'null.out','&&query_name..html')  AS spool_dest
  FROM gv$sql_monitor 
 where sql_id = '&&sql_id';

set serveroutput on
begin
	$IF dbms_db_version.ver_le_11_2 $THEN
		dbms_output.put_line('Version 11.2 or less, not setting event to disable XML compression');
	$ELSE
		dbms_output.put_line('Version is 12.1 or greater, setting event to disable XML compression');
		execute immediate q'[ alter session set events='emx_control compress_xml=none' ]' ;
	$END
end;
/

 

set serveroutput ON SIZE 1000000 FORMAT WORD_WRAPPED
set wrap on
set verify off
set pagesize 0 echo off timing off linesize 1000 trimspool on trim on long 2000000 longchunksize 2000000 feedback off
spool &&spooldest
select dbms_sqltune.report_sql_monitor(type=>'ACTIVE', report_level=>'ALL',sql_id=>'&&sql_id') monitor_report from dual;
spool off
