#!/usr/bin/env gosh

(use test.unit)

(let ((test
       (make-test-suite
        "Test result"
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
  (call-with-output-string
   (cut with-output-to-port <>
        (lambda () (run test))))
  (define-test-case "Test result"
    ("Test test-case1"
     (with-module test.unit
                  (let ((test-case (car (test-cases-of test))))
                    (assert-equal 1 (length (tests-of test-case)))
                    (assert-equal 1 (success-of test-case))
                    (assert-equal 2 (failure-of test-case))
                    (assert-equal 0 (error-of test-case)))))
    ("Test test-case2"
     (with-module test.unit
                  (let ((test-case (cadr (test-cases-of test))))
                    (assert-equal 2 (length (tests-of test-case)))
                    (assert-equal 1 (success-of test-case))
                    (assert-equal 2 (failure-of test-case))
                    (assert-equal 2 (error-of test-case)))))
    ("Test test-suite"
     (with-module test.unit
                  (assert-equal 3 (test-number-of test))
                  (assert-equal 8 (assertion-number-of test))
                  (assert-equal 2 (success-number-of test))
                  (assert-equal 4 (failure-number-of test))
                  (assert-equal 2 (error-number-of test))))))

(run-all-test)
     
