#!/bin/bash
FILENAME='rtl1.sql'

echo "Bash version ${BASH_VERSION}..."
for i in {0..10..2}
  do
     echo "Welcome $i times"
 done



for f in $FILES
do
  echo "Processing $f file..."
  queryname=`echo "$f" | sed 's/\..\{3\}$//' | sed 's/[\/|\.]//g'`
  \sqlplus d56/d56 @driver $f $queryname  null.sql
done
