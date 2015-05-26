Query-Test-Framework
====================

A set of tools to run queries on an Oracle Database, capture metrics, and graph the results

## Capture Files
* **run-all.sh** Once configured, looks in a directory for files containing SQL queries, then connects to sql*plus and runs each query, capturing the output
* **query-capture.sql** called by run-all.sh to capture the sqlmonitor reports and stats for each query
* **run-one-px.sh** similar to run-all.sh, though this script is meant to run a single query over and over, changing the degree of parallelism
* **query-capture-variable-px.sql** called by run-one-px.sh. Almost identical to query-capture.sql, except that the 5th parameter is the degree of parallelism
* **sql-list.sql** lists recent SQL from gv$sql_monitor and gv$sqltext, allowing you to use the sql_id to call sql-capture.sql. The use case for this is to extract recent queries, perhaps run by a batch process, that you will later use with run-all.sh
* **sql-capture.sql** asks for a sql_id and a query name, then writes the SQL text to a file using the name. Used with sql-list.sql

## Visualization Files
* **parse-SQL-Monitor-HTML.R** an R script that parses and extracts data from all files active sql monitor reports (HTML, uncompressed format) in the current working directory (of the R session)
* **aggregateSQLMonResults.R** once you have several directories of active sql monitor reports, AND you've run parse-SQL-Monitor-HTML.R on each directory, thus generating a .Rda file for each dir, this R script starts from their parent directory and traverses down, looking for each .Rda file, then aggregating the stats for all test runs
* **parse-sqlmon-dirs.sh** finds all sub-directories, then runs parse-SQL-Monitor-HTML.R on each
