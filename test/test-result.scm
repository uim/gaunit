#!/usr/bin/env gosh

(add-load-path ".")
(use test.unit)
(require "test/utils")

(let ((test
       (make-test-suite "Test result"
         ("test-case1: 1 test result"
          ("1 successes 2 failures"
           (assert-equal 1 1)
           (assert-equal 1 3)
           (assert-equal 1 2)))
         ("test-case2: 2 test result"
          ("1 successes 0 failures 1 errors"
           (assert-equal 1 1)
           (assert-equal 1 (1)))
          ("0 successes 2 failures 1 errors"
           (assert-equal #f #t)
           (assert-equal #f #t)
           (assert-equal (1)))))))
  (run-test-with-no-output test)
  (define-test-case "Test result"
    ("Test test-case1"
     (assert-test-case-result
      (car (with-module test.unit (test-cases-of test)))
      1 1 2 0))
    ("Test test-case2"
     (assert-test-case-result
      (cadr (with-module test.unit (test-cases-of test)))
      2 1 2 2))
    ("Test test-suite"
     (assert-test-suite-result test 3 8 2 4 2))))

(let* ((test
        (make-test-case "Error test"
          ("Error occured"
           (assert-equal (1)))))
       (test-result (run-test-with-no-output test)))
  (define-test-case "Error test"
    ("Test test-case1"
     (assert-have-error-message test-result))))
