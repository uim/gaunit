(define-module test.unit.result
  (extend test.unit.common)
  (use test.unit.ui)
  (export <result>
          success-of failure-of error-of
          add-success! add-failure! add-error!))
(select-module test.unit.result)

(define-class <result> ()
  ((success :accessor success-of :init-value 0)
   (failure :accessor failure-of :init-value 0)
   (error :accessor error-of :init-value 0)
   ))

(define-method add-success! ((self <result>) test-ui test)
  (inc! (success-of self))
  (test-successed test-ui test))

(define-method add-failure! ((self <result>) test-ui test message stack-trace)
  (inc! (failure-of self))
  (test-failed test-ui test message stack-trace))

(define-method add-error! ((self <result>) test-ui test err)
  (inc! (error-of self))
  (test-errored test-ui test err))

(provide "test/unit/result")
