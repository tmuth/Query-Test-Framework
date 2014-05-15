#!/bin/bash
FILES=./rtl*.sql
for f in $FILES
do
  echo "Processing $f file..."
  queryname=`echo "$f" | sed 's/\..\{3\}$//' | sed 's/[\/|\.]//g'`
  \sqlplus d56/d56 @query-capture.sql $f $queryname  null.sql
done
