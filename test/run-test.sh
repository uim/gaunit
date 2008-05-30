#!/bin/sh

basedir=`dirname $0`

for test in $basedir/../test/**/test-*.scm
do
  echo "Running test $test"
  gosh -I$basedir/../ $test -vp
done
