(define-module test.ui
  (export test-run test-case-run test-suite-run
          test-successed test-failed test-errored)
  )
(select-module test.ui)

(define-method test-errored (ui test err)
  (error "Not implimented"))

(define-method test-successed (ui test)
  (error "Not implimented"))

(define-method test-failed (ui test message stack-trace)
  (error "Not implimented"))

(define-method test-run (ui test test-thunk)
  (error "Not implimented"))

(define-method test-case-run (ui test-case test-thunk)
  (error "Not implimented"))

(define-method test-suite-run (ui test-suite test-thunk)
  (error "Not implimented"))
