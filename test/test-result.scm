(define-module test.test-result
  (extend test.unit.test-case)
  (use test.unit)
  (use test.gaunit-test-utils))
(select-module test.test-result)

(define %test-cases-of (with-module test.unit.base test-cases-of))
(define %tests-of (with-module test.unit.base tests-of))

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
                         ("test-case3: 3 test result"
                          ("1 pass, 1 pending"
                           (assert-equal 1 1)
                           (pend "not work yet"))
                          ("1 failure"
                           (pend "not work yet" (lambda () "nothing raised")))
                          ("1 pending"
                           (pend "not work yet"
                                 (lambda () (error "not implemented")))))))

  (set! test-case
        (make-test-case "Error test"
                        ("Error occurred"
                         (assert-equal (1))))))

(define (test-success-failure-test-case-result)
  (assert-run-result
   #f
   0 1 1
   1 0 0 1 0
   `((failure "1 passes, 2 failures"
              ,(string-append "expected: <1>\n"
                              " but was: <3>")))
   (car (%test-cases-of test-suite)))
  #f)

(define (test-success-error-test-case-result)
  (assert-run-result
   #f
   0 1 2
   1 0 0 1 1
   `((failure "0 passes, 2 failures, 1 errors"
              ,(string-append "expected: <#f>\n"
                              " but was: <#t>"))
     (error "1 passes, 0 failures, 1 errors"
            "#<error \"invalid application: (1)\">"))
   (cadr (%test-cases-of test-suite)))
  #f)

(define (test-pending-test-case-result)
  (assert-run-result
   #f
   0 1 3
   1 0 2 1 0
   `((pending "1 pending" "not work yet")
     (failure "1 failure" "Pending thunk should not be passed: not work yet")
     (pending "1 pass, 1 pending" "not work yet"))
   (caddr (%test-cases-of test-suite)))
  #f)

(define (test-pending-test-result)
  (assert-run-result
   #t
   0 0 1
   1 0 1 0 0
   '((pending "1 pass, 1 pending" "not work yet"))
   (car (%tests-of (caddr (%test-cases-of test-suite)))))
  #f)

(define (test-suite-result)
  (assert-run-result
   #f
   1 3 6
   3 0 2 3 1
   `((pending "1 pending" "not work yet")
     (failure "1 failure" "Pending thunk should not be passed: not work yet")
     (pending "1 pass, 1 pending" "not work yet")
     (failure "0 passes, 2 failures, 1 errors"
              ,(string-append "expected: <#f>\n"
                              " but was: <#t>"))
     (error "1 passes, 0 failures, 1 errors"
            "#<error \"invalid application: (1)\">")
     (failure "1 passes, 2 failures"
              ,(string-append "expected: <1>\n"
                              " but was: <3>")))
   test-suite)
  #f)

(define (test-error-result)
  (assert-output "E" (lambda () (run-test-with-ui test-case)))
  #f)

(provide "test/test-result")
