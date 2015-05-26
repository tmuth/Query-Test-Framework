#!/bin/bash

## Edit variables in the following block ################################################################

DIRECTORY=star-schema1
MAIN_TEST_NAME="olap.500m.capacity"
FILES=\*.sql
CONNECT_STRING=olap_perf/olap_perf
# . ~/./orcl.env


declare -A testName # this is an associative array of testname:alter-session-file
#testName[a]=null.sql
testName[IMC-autovector]=in-memory-auto-vector.sql
testName[IMC-novector]=in-memory-no-vector.sql
testName[IMC-forcevector]=in-memory-vector.sql

testName[NOIMC-autovector]=no-in-memory-auto-vector.sql
testName[NOIMC-novector]=no-in-memory-novector.sql
testName[NOIMC-forcevector]=no-in-memory-vector.sql

## Do Not Edit Below This Line ###########################################################################




ALTER_SESSION_SUBDIR=`echo "alter"`

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

#for t in "${!testName[@]}"; do
#    #printf "%s is in %s\n" "$t" "${testName[$t]}"
#	OUTPUT_SUB_DIRECTORY=`echo "$MAIN_TEST_NAME.$t.test_"$(date +"%m-%d_%H-%M-%S")`
#	#echo $OUTPUT_SUB_DIRECTORY
#	ALTER_SESSION_FILE=${testName[$t]}
#	
#	ALTER_SESSION_FILE=`echo $ALTER_SESSION_SUBDIR/${testName[$t]}`
#	#echo $ALTER_SESSION_FILE
#	#cat $ALTER_SESSION_FILE
#done

tLength=${#testName[@]}
declare -i loopCounter
loopCounter=0


# main test loop
for t in "${!testName[@]}"; do #loop over all of the tests in the test array
	((loopCounter++))
	echo "$(date +"%m-%d_%H:%M:%S") - Begin Test $loopCounter of $tLength \n" >> status.log
    #printf "%s is in %s\n" "$t" "${testName[$t]}"
	OUTPUT_SUB_DIRECTORY=`echo "$MAIN_TEST_NAME.$t.test_"$(date +"%m-%d_%H-%M-%S")`
	echo $OUTPUT_SUB_DIRECTORY
	ALTER_SESSION_FILE=`echo "$ALTER_SESSION_SUBDIR/${testName[$t]}"`
	echo $ALTER_SESSION_FILE
	
	
	if [[ ! -d ${ALTER_SESSION_SUBDIR} ]];
	then
		echo "ALTER_SESSION_SUBDIR does not exist so creating it now"
		mkdir ${ALTER_SESSION_SUBDIR}
	fi
	
	if [[ ! -f ${ALTER_SESSION_FILE} ]];
	then
		echo "ALTER_SESSION_FILE does not exist so creating a blank file for it"
		touch ${ALTER_SESSION_FILE}
	fi

	if [[ ! -d ${OUTPUT_SUB_DIRECTORY} ]];
	then
		echo "OUTPUT_SUB_DIRECTORY does not exist so creating it now"
		mkdir ${OUTPUT_SUB_DIRECTORY}
	fi
		
	for f in ${FILES}; # for each test, loop over all of the query files
	do
		if [ -f ${f} ]
		then
			if [ `basename $ALTER_SESSION_FILE` != `basename $f` ]
			then
				echo "Processing $f file..."
				#queryname=`basename $f .sql | sed 's/\..\{3\}$//' | sed 's/[\/|\.]//g'`
				queryname=`basename $f .sql | sed 's/\..\{3\}$//' | sed 's/[\/]//g'`
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
	
	if [ "$errorCode" -gt 0 ]
	then
	   break  # Skip entire rest of loop.
	fi
	echo "$(date +"%m-%d_%H:%M:%S") - End   Test $loopCounter of $tLength \n" >> status.log
done

echo "\n\n\n" >> status.log
