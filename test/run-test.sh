#!/bin/sh

for test in test/test-*.scm
do
  echo "Running test $test..."
  gosh $test
done
