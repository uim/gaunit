(define-module test.test-result
  (extend test.unit.test-case)
  (use test.unit)
  (use test.gaunit-test-utils))
(select-module test.test-result)

(define %test-cases-of (with-module test.unit.base test-cases-of))

(define test-suite #f)
(define test-case #f)

(define (setup)
  (set! test-suite
        (make-test-suite "Test result"
                         ("test-case1: 1 test result"
                          ("1 passes, 2 failures"
                           (assert-equal 1 1)
                           (assert-equal 1 3)
                           (assert-equal 1 2)))
                         ("test-case2: 2 test result"
                          ("1 passes, 0 failures, 1 errors"
                           (assert-equal 1 1)
                           (assert-equal 1 (1)))
                          ("0 passes, 2 failures, 1 errors"
                           (assert-equal #f #t)
                           (assert-equal #f #t)
                           (assert-equal (1))))
                         ("test-case3: 1 test result"
                          ("1 passes, 1 pendings"
                           (assert-equal 1 1)
                           (pend "not work yet")))))

  (set! test-case
        (make-test-case "Error test"
                        ("Error occurred"
                         (assert-equal (1))))))

(define (test-success-failure-result)
  (assert-run-result
   0 1 1
   1 0 0 1 0
   (car (%test-cases-of test-suite)))
  #f)

(define (test-success-error-result)
  (assert-run-result
   0 1 2
   1 0 0 1 1
   (cadr (%test-cases-of test-suite)))
  #f)

(define (test-suite-result)
  (assert-run-result
   1 3 4
   3 0 1 2 1
   test-suite)
  #f)

(define (test-error-result)
  (assert-output "E" (lambda () (run-test-with-ui test-case)))
  #f)

(provide "test/test-result")
