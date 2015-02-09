#!/bin/bash
DIRECTORY=retail-queries
#OUTPUT_SUB_DIRECTORY="Test1"
OUTPUT_SUB_DIRECTORY=`echo "d28.px.test_"$(date +"%m-%d_%H-%M-%S")`
FILE=rtl22.sql
ALTER_SESSION_FILE=null.sql
CONNECT_STRING=d28/d28
# . ~/./orcl.env


SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $SCRIPT_DIR

if [[ "$OSTYPE" == "cygwin" ]]; then
        QUERY_CAPTURE_FILE=`cygpath -w ${SCRIPT_DIR}/query-capture.sql`
		echo "cygwin"
else
       QUERY_CAPTURE_FILE="${SCRIPT_DIR}/query-capture.sql"
	   echo "not cygwin"
fi
echo "$QUERY_CAPTURE_FILE"

cd $DIRECTORY



if [[ ! -f ${f} ]];
then
	echo "ALTER_SESSION_FILE does not exist so creating a blank file for it"
	touch ${ALTER_SESSION_FILE}
fi

if [[ ! -f ${OUTPUT_SUB_DIRECTORY} ]];
then
	echo "OUTPUT_SUB_DIRECTORY does not exist so creating it now"
	mkdir ${OUTPUT_SUB_DIRECTORY}
fi




dop_list=(32 64 128 256 512 564 660 800 1024)
#dop_list=(32 64 )

for i in ${dop_list[@]}; 
#for f in ${FILES};
do
	#if [ -f ${f} ]
	#then
	#	if [ `basename $ALTER_SESSION_FILE` != `basename $f` ]
	#	then
			echo "Processing $FILE file..."
			queryname1=`basename $FILE .sql | sed 's/\..\{3\}$//' | sed 's/[\/|\.]//g'`
			queryname="$queryname1.$i"
			echo "Query Name: $queryname"
			
			\sqlplus $CONNECT_STRING @"$QUERY_CAPTURE_FILE" $FILE $queryname "$ALTER_SESSION_FILE" $OUTPUT_SUB_DIRECTORY $i
			errorCode=$?  
			echo "Error Code: $errorCode"

			if [ "$errorCode" -gt 0 ]
 			then
			   break  # Skip entire rest of loop.
 			fi
	#	fi
	#fi
done

grep -m 1 Elapsed *.txt > timing.out
