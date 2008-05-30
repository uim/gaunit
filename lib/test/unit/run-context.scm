(define-module test.unit.run-context
  (extend test.unit.common)
  (use test.unit.listener)
  (export <test-run-context>
          n-assertions-of n-successes-of n-failures-of n-errors-of

          test-run-context-start
          test-run-context-start-test-suite
          test-run-context-start-test-case
          test-run-context-start-test

          test-run-context-pass-assertion
          test-run-context-success
          test-run-context-failure
          test-run-context-error

          test-run-context-finish-test
          test-run-context-finish-test-case
          test-run-context-finish-test-suite
          test-run-context-finish
          ))
(select-module test.unit.run-context)

(define-class <test-run-context> ()
  ((listeners :accessor listeners-of :init-form '())
   (n-assertions :accessor n-assertions-of :init-value 0)
   (n-successes :accessor n-successes-of :init-value 0)
   (n-failures :accessor n-failures-of :init-value 0)
   (n-errors :accessor n-errors-of :init-value 0)))

(define (notify run-context key . args)
  (let* ((callback-symbol (string->symbol #`"test-listener-on-,key"))
         (callback-method (global-variable-ref (current-module)
                                               callback-symbol)))
    (for-each (lambda (listener)
                (apply callback-method listener run-context args))
              (listeners-of run-context))))

(define (test-run-context-start run-context)
  (notify run-context 'start))

(define (test-run-context-start-test-suite run-context test-suite)
  (notify run-context 'start-test-suite test-suite))

(define (test-run-context-start-test-case run-context test-case)
  (notify run-context 'start-test-case test-case))

(define (test-run-context-start-test run-context test)
  (notify run-context 'start-test test))

(define (test-run-context-pass-assertion run-context test)
  (inc! (n-assertions-of run-context))
  (notify run-context 'pass-assertion test))

(define (test-run-context-success run-context test)
  (inc! (n-successes-of run-context))
  (notify run-context 'success test))

(define (test-run-context-failure run-context test message stack-trace)
  (inc! (n-failures-of run-context))
  (notify run-context 'failure test))

(define (test-run-context-error run-context test error)
  (inc! (n-errors-of run-context))
  (notify run-context 'error test))

(define (test-run-context-finish-test run-context test)
  (notify run-context 'finish-test test))

(define (test-run-context-finish-test-case run-context test-case)
  (notify run-context 'finish-test-case test-case))

(define (test-run-context-finish-test-suite run-context test-suite)
  (notify run-context 'finish-test-suite test-suite))

(define (test-run-context-finish run-context)
  (notify run-context 'finish))

(provide "test/unit/run-context")
