(define-module test.test-result
  (extend test.unit.test-case)
  (use test.unit)
  (use test.gaunit-test-utils))
(select-module test.test-result)

(define %test-cases-of (with-module test.unit.base test-cases-of))

(define test-suite #f)
(define test-suite-result #f)
(define test-case #f)
(define test-case-result #f)

(define (setup)
  (set! test-suite
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
                           (assert-equal (1))))))
  (set! test-suite-result (run-with-string-output test-suite))

  (set! test-case
        (make-test-case "Error test"
                        ("Error occurred"
                         (assert-equal (1)))))
  (set! test-case-result (run-with-string-output test-case)))

(define (test-success-failure-result)
  (assert-test-case-result
   1 1 2 0
   (car (%test-cases-of test-suite))))

(define (test-success-error-result)
  (assert-test-case-result
   2 1 2 2
   (cadr (%test-cases-of test-suite))))

(define (test-suite-result)
  (assert-test-suite-result 3 8 2 4 2 test-suite))

(define (test-error-result)
  (assert-equal "E" test-case-result))

(provide "test/test-result")
