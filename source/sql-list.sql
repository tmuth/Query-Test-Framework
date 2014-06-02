set linesize 200
set wrap off
set pagesize 300
column node format 99
column sid format 999
column module format a15
column action format a15
column USERNAME format a15
column CLIENT_IDENTIFIER format a17
column sql format a60

select * from (
select inst_id node,
		--sid,SESSION_SERIAL# ser#,
		username,module,
		--action,
		--CLIENT_IDENTIFIER,
		status,sql_id,round((sysdate - sql_exec_start)*1440,1) min_ago,
       '** SQL Mon' SOURCE,
       substr(sql_text,1,50) sql
from  gv$sql_monitor 
where username is not null
  and sql_exec_start > sysdate - 30/1440
union all  
 select ses.inst_id node,
		--ses.sid,ses.SERIAL# ser#,
		username, module,
		--action,
		--CLIENT_IDENTIFIER,
		status,sql.sql_id,round((sysdate - sql_exec_start)*1440,1) min_ago,
       'GV$SQLTEXT' SOURCE, 
       substr(sql_text,1,50) sql
  FROM gV$SESSION SES,   
       GV$SQLTEXT SQL 
 where SES.STATUS = 'ACTIVE'
   and SES.USERNAME is not null
   and SES.SQL_ADDRESS    = SQL.ADDRESS 
   and SES.SQL_HASH_VALUE = SQL.HASH_VALUE 
   and SES.INST_ID = SQL.INST_ID
   and sql.piece = 1
   and Ses.AUDSID <> userenv('SESSIONID'))
   order by source,min_ago asc;