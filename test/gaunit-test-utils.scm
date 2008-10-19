(define-module test.gaunit-test-utils
  (use test.unit)
  (use test.unit.run-context)
  (use test.unit.ui.text)
  (export-all))
(select-module test.gaunit-test-utils)

(define (run-test-with-ui test . options)
  (let ((run-context (make <test-run-context>)))
    (push! (listeners-of run-context) (apply make <test-ui-text> options))
    (test-run test :run-context run-context)))

(define (assert-run-context n-test-suites n-test-cases n-tests
                            n-assertions n-successes n-pendings
                            n-failures n-errors messages
                            run-context)
  (assert-equal `((n-test-suites . ,n-test-suites)
                  (n-test-cases . ,n-test-cases)
                  (n-tests . ,n-tests)
                  (n-assertions . ,n-assertions)
                  (n-successes . ,n-successes)
                  (n-pendings . ,n-pendings)
                  (n-failures . ,n-failures)
                  (n-errors . ,n-errors)
                  (messages . ,messages))
                `((n-test-suites . ,(n-test-suites-of run-context))
                  (n-test-cases . ,(n-test-cases-of run-context))
                  (n-tests . ,(n-tests-of run-context))
                  (n-assertions . ,(n-assertions-of run-context))
                  (n-successes . ,(n-successes-of run-context))
                  (n-pendings . ,(n-pendings-of run-context))
                  (n-failures . ,(n-failures-of run-context))
                  (n-errors . ,(n-errors-of run-context))
                  (messages . ,(map (lambda (fault)
                                      (let ((kind (car fault))
                                            (test (cadr fault))
                                            (message (caddr fault)))
                                        (list kind
                                              (name-of test)
                                              (if (string? message)
                                                message
                                                (pretty-print-object message)))))
                                    (faults-of run-context)))))
  run-context)

(define (assert-run-result success
                           n-test-suites n-test-cases n-tests
                           n-assertions n-successes n-pendings
                           n-failures n-errors messages
                           test)
  (let ((run-context (make <test-run-context>)))
    (if success
      (assert-true (test-run test :run-context run-context))
      (assert-false (test-run test :run-context run-context)))
    (assert-run-context n-test-suites n-test-cases n-tests
                        n-assertions n-successes n-pendings n-failures n-errors
                        messages run-context)))

(provide "test/gaunit-test-utils")
