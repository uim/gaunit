(define-module test.unit.ui
  (export test-successed test-failed test-errored
          test-run test-case-run
          test-case-setup test-case-teardown
          test-start test-case-start test-suite-start
          test-finish test-case-finish test-suite-finish))
(select-module test.unit.ui)

(define-method test-errored (ui test err)
  (error "Not implimented"))

(define-method test-successed (ui test)
  (error "Not implimented"))

(define-method test-failed (ui test message stack-trace)
  (error "Not implimented"))

(define-method test-run (ui test test-thunk)
  (test-thunk))

(define-method test-start (ui test)
  (error "Not implimented"))

(define-method test-finish (ui test)
  (error "Not implimented"))

(define-method test-case-setup (ui test setup-thunk)
  (setup-thunk))

(define-method test-case-teardown (ui test teardown-thunk)
  (teardown-thunk))

(define-method test-case-start (ui test-case)
  (error "Not implimented"))

(define-method test-case-finish (ui test-case)
  (error "Not implimented"))

(define-method test-suite-start (ui test-suite)
  (error "Not implimented"))

(define-method test-suite-finish (ui test-suite)
  (error "Not implimented"))

(provide "test/unit/ui")
