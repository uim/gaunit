(select-module test.unit.assertions)
(define (make-number-of-message-handler expect type)
  (make-message-handler expect :after-expected
                        (format " number of ~a" type)))

(define-assertion (assert-test-case-result test-case n-tests
                                           n-assertions n-failures
                                           n-errors)
  (let ((run-context (make <test-run-context>)))
    (parameterize ((count-assertion #t))
      (test-run test-case :run-context run-context))
    (assert-equal n-tests (n-tests-of run-context)
                  (make-number-of-message-handler n-tests "test"))
    (assert-equal n-assertions (n-assertions-of run-context)
                  (make-number-of-message-handler n-assertions "assertion"))
    (assert-equal n-failures (n-failures-of run-context)
                  (make-number-of-message-handler n-failures "failure"))
    (assert-equal n-errors (n-errors-of run-context)
                  (make-number-of-message-handler n-errors "error"))))
