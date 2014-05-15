grant select on v_$statname to sh;
grant select on gv_$session to sh;
grant select on v_$sql to sh;
grant select on gv_$sql to sh;
grant select on V_$SQL_PLAN_STATISTICS_ALL to sh;
grant execute on DBMS_SQLTUNE to sh;
grant select on v_$mystat to sh;
GRANT "SELECT_CATALOG_ROLE" TO "SH" ;