(define-module test.gaunit-test-utils
  (use test.unit)
  (use test.unit.run-context)
  (use test.unit.ui.text)
  (export-all))
(select-module test.gaunit-test-utils)

(define (run-test test)
  (let ((run-context (make <test-run-context>)))
    (test-run test :run-context run-context)
    run-context))

(define (run-test-with-ui test)
  (let ((run-context (make <test-run-context>)))
    (push! (listeners-of run-context) (make <test-ui-text>))
    (test-run test :run-context run-context)))

(define (assert-run-context n-test-suites n-test-cases n-tests
                            n-assertions n-successes n-failures n-errors
                            run-context)
  (assert-equal `((n-test-suites . ,n-test-suites)
                  (n-test-cases . ,n-test-cases)
                  (n-tests . ,n-tests)
                  (n-assertions . ,n-assertions)
                  (n-successes . ,n-successes)
                  (n-failures . ,n-failures)
                  (n-errors . ,n-errors))
                `((n-test-suites . ,(n-test-suites-of run-context))
                  (n-test-cases . ,(n-test-cases-of run-context))
                  (n-tests . ,(n-tests-of run-context))
                  (n-assertions . ,(n-assertions-of run-context))
                  (n-successes . ,(n-successes-of run-context))
                  (n-failures . ,(n-failures-of run-context))
                  (n-errors . ,(n-errors-of run-context)))))

(define (assert-run-result n-test-suites n-test-cases n-tests
                           n-assertions n-successes n-failures n-errors
                           test-case)
  (assert-run-context n-test-suites n-test-cases n-tests
                      n-assertions n-successes n-failures n-errors
                      (run-test test-case)))

(provide "test/gaunit-test-utils")
