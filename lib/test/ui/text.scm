(select-module test.unit)

(use gauche.vm.debugger)

(define-class <test-ui-text> ()
  ())

(define-method display-result ((self <test-ui-text>) test-suite)
  (print (format "Test suite ~s was run."
                 (name-of test-suite))))

(define-method test-errored ((self <test-ui-text>) test err)
  (display "E\n")
  (report-error err))

(define-method test-successed ((self <test-ui-text>) test)
  (display "."))

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (print "F\n" message #`" in ,(name-of test)")
   (with-error-to-port (standard-output-port)
                       (with-module gauche.vm.debugger
                                    (lambda ()
                                      (debug-print-stack
                                       stack-trace
                                       *stack-show-depth*)))))

(define-method test-start ((self <test-ui-text>) test)
  #f)
;  (print #`"--- Start test ,(name-of test)"))

(define-method test-finish ((self <test-ui-text>) test)
  #f)
;  (newline))

(define-method test-case-start ((self <test-ui-text>) test-case)
  (print #`"-- Start test case ,(name-of test-case)"))

(define-method test-case-finish ((self <test-ui-text>) test-case)
  (newline))

(define-method test-suite-start ((self <test-ui-text>) test-suite)
  (print #`"- Start test suite ,(name-of test-suite)"))

(define-method test-suite-finish ((self <test-ui-text>) test-suite)
  (print (format "~s tests, ~s successes, ~s failures, ~s errors"
                 (test-number-of test-suite)
                 (success-number-of test-suite)
                 (failure-number-of test-suite)
                 (error-number-of test-suite))))

(set-default-test-ui! (make <test-ui-text>))
