(define-module test.unit.listener
  (extend test.unit.common)
  (export test-listener-on-start
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
  (error "Not implimented"))

(define-method test-listener-on-start-test-suite (listener run-context
                                                           test-suite)
  (error "Not implimented"))

(define-method test-listener-on-start-test-case (listener run-context test-case)
  (error "Not implimented"))

(define-method test-listener-on-start-test (listener run-context test)
  (error "Not implimented"))

(define-method test-listener-on-pass-assertion (listener run-context test)
  (error "Not implimented"))

(define-method test-listener-on-success (listener run-context test)
  (error "Not implimented"))

(define-method test-listener-on-failure (listener run-context test message
                                                  stack-trace)
  (error "Not implimented"))

(define-method test-listener-on-error (listener run-context test error)
  (error "Not implimented"))

(define-method test-listener-on-finish-test (listener run-context test)
  (error "Not implimented"))

(define-method test-listener-on-finish-test-case (listener run-context test-case)
  (error "Not implimented"))

(define-method test-listener-on-finish-test-suite (listener run-context
                                                            test-suite)
  (error "Not implimented"))

(define-method test-listener-on-finish (listener run-context)
  (error "Not implimented"))

(provide "test/unit/listener")
