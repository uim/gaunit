(define-module test.unit.listener
  (extend test.unit.common)
  (export <test-listener>
          test-listener-on-start
          test-listener-on-start-test-suite
          test-listener-on-start-test-case
          test-listener-on-start-test
          test-listener-on-pass-assertion
          test-listener-on-success
          test-listener-on-failure
          test-listener-on-error
          test-listener-on-finish-test
          test-listener-on-finish-test-case
          test-listener-on-finish-test-suite
          test-listener-on-finish))
(select-module test.unit.listener)

(define-class <test-listener> ()
  ())

(define-method test-listener-on-start (listener run-context)
  (error "Not implemented: test-listener-on-start"))

(define-method test-listener-on-start-test-suite (listener
                                                  run-context test-suite)
  (error "Not implemented: test-listener-on-start-test-suite"))

(define-method test-listener-on-start-test-case (listener run-context test-case)
  (error "Not implemented: test-listener-on-start-test-case"))

(define-method test-listener-on-start-test (listener run-context test)
  (error "Not implemented: test-listener-on-start-test"))

(define-method test-listener-on-pass-assertion (listener run-context test)
  (error "Not implemented: test-listener-on-pass-assertion"))

(define-method test-listener-on-success (listener run-context test)
  (error "Not implemented: test-listener-on-success"))

(define-method test-listener-on-failure (listener
                                         run-context test message stack-trace)
  (error "Not implemented: test-listener-on-failure"))

(define-method test-listener-on-error (listener run-context test err)
  (error "Not implemented: test-listener-on-error"))

(define-method test-listener-on-finish-test (listener run-context test)
  (error "Not implemented: test-listener-on-finish-test"))

(define-method test-listener-on-finish-test-case (listener run-context test-case)
  (error "Not implemented: test-listener-on-finish-test-case"))

(define-method test-listener-on-finish-test-suite (listener run-context
                                                            test-suite)
  (error "Not implemented: test-listener-on-finish-test-suite"))

(define-method test-listener-on-finish (listener run-context)
  (error "Not implemented: test-listener-on-finish"))

(provide "test/unit/listener")
