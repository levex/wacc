#!/bin/bash

if [[ $# -lt 1 ]]
  then echo "Usage: $0 <path_to_wacc_examples_repo> [-q|-g]"
    echo "-g implies printing out the output of each wacc compilation"
    echo "-q implies printing out the output of each wacc compilation"
  exit
fi 

TARGET_PATH=$1
PRINT_ERRS=$2

if [[ ! -d ${TARGET_PATH} ]]
  then echo "Target path does not exist"
  exit
fi 

VALID_WACC=`find ${TARGET_PATH} -name \*.wacc`

TEST_FAILURES=0
TEST_SUCCESS=0
TOTAL_TESTS=0
FAILED_TESTS=

for wacc in ${VALID_WACC}
do
  if [[ $PRINT_ERRS == "-g" ]]
  then
    stack exec wacc-exe ${wacc}
  else
    stack exec wacc-exe ${wacc} >/dev/null 2>&1
  fi
  EXIT_CODE=$?
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
  TEST_NAME=`basename ${wacc/${TARGET_PATH}/}`

  if ((${EXIT_CODE} == 0))
  then
    TEST_SUCCESS=$((TEST_SUCCESS + 1))
    if [[ $PRINT_ERRS != "-q" ]]
    then
        echo "SUCCESS: ${TEST_NAME}"
    fi
  else
    TEST_FAILURES=$((TEST_FAILURES + 1))
    FAILED_TESTS+=" ${TEST_NAME}"
    if [[ $PRINT_ERRS != "-q" ]]
    then
        echo "FAILURE: ${TEST_NAME}"
    fi
  fi
done

echo "-------------------------------------"
echo "|           TEST RESULTS            |"
echo "-------------------------------------"
echo ""
echo "Total tests: ${TOTAL_TESTS}"
echo "Tests successfully executed: ${TEST_SUCCESS}"
echo "Failed tests: ${TEST_FAILURES}"
if [[ ${TEST_FAILURES} -ne 0 ]]
then
  echo "These are: "
  echo "-------------------------------------"
  for i in ${FAILED_TESTS}; do echo $i; done
  echo "-------------------------------------"
fi
