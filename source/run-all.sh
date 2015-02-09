#!/bin/bash
DIRECTORY=retail-queries2
#OUTPUT_SUB_DIRECTORY="Test1"
OUTPUT_SUB_DIRECTORY=`echo "d7.test_"$(date +"%m-%d_%H-%M-%S")`
FILES=rtl*.sql
ALTER_SESSION_FILE=no-in-memory.sql
CONNECT_STRING=d7/d7
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

for f in ${FILES};
do
	if [ -f ${f} ]
	then
		if [ `basename $ALTER_SESSION_FILE` != `basename $f` ]
		then
			echo "Processing $f file..."
			queryname=`basename $f .sql | sed 's/\..\{3\}$//' | sed 's/[\/|\.]//g'`
			echo "Query Name: $queryname"
			
			\sqlplus $CONNECT_STRING @"$QUERY_CAPTURE_FILE" $f $queryname "$ALTER_SESSION_FILE" $OUTPUT_SUB_DIRECTORY
			errorCode=$?
                        echo "Error Code: $errorCode"

                        if [ "$errorCode" -gt 0 ]
                        then
                           break  # Skip entire rest of loop.
                        fi

		fi
	fi
done

grep -m 1 Elapsed *.txt > timing.out
