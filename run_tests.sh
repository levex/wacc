#!/bin/sh

if [[ $# -lt 1 ]]
  then echo "Usage: $0 <path_to_wacc_examples_repo> [-g]"
    echo "-g implies printing out the output of each wacc compilation"
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
    echo "FAFAFAF"
    stack exec wacc-exe ${wacc}
  else
    stack exec wacc-exe ${wacc} >/dev/null 2>&1
  fi
  TOTAL_TESTS=$((TOTAL_TESTS + 1))
  TEST_NAME=`basename ${wacc/${TARGET_PATH}/}`

  if (($? > 0))
  then
    TEST_SUCCESS=$((TEST_SUCCESS + 1))
   echo "SUCCESS: ${TEST_NAME}"
  else
    TEST_FAILURES=$((TEST_FAILURES + 1))
    echo "FAILURE: ${TEST_NAME}"
  fi
  FAILED_TESTS+=" ${TEST_NAME}"
done

echo "-------------------------------------"
echo "|           TEST RESULTS            |"
echo "-------------------------------------"
echo ""
echo "Total tests: ${TOTAL_TESTS}"
echo "Tests successfully executed: ${TEST_SUCCESS}"
echo "Failed tests: ${TEST_FAILURES}"
echo "These are: "
echo "-------------------------------------"
for i in ${FAILED_TESTS}
do
  echo ${i}
done
echo "-------------------------------------"
