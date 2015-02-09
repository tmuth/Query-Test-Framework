#!/bin/bash
for i in $(find . -type d)
do
    echo "$i"
   ( cd $i && `rscript "M:\Dropbox\MyFiles\GitHub\Query-Test-Framework\source\parse-SQL-Monitor-HTML.R" .` )
done
