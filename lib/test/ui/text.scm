(select-module test.unit)

(use gauche.vm.debugger)
(use gauche.time)

(define-class <test-ui-text> ()
  ())

(define-method display-result ((self <test-ui-text>) test-suite)
  (print (format "Test suite ~s was run."
                 (name-of test-suite))))

(define-method test-errored ((self <test-ui-text>) test err)
  (print "E\n" #`"Error occured in ,(name-of test)")
  (with-error-to-port (current-output-port)
                      (lambda () (report-error err))))

(define-method test-successed ((self <test-ui-text>) test)
  (display "."))

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (print "F\n" message #`" in ,(name-of test)")
   (with-error-to-port (current-output-port)
                       (with-module gauche.vm.debugger
                                    (lambda ()
                                      (debug-print-stack
                                       stack-trace
                                       *stack-show-depth*)))))

(define-method test-run ((self <test-ui-text>) test test-thunk)
  (test-thunk))

(define-method test-case-run ((self <test-ui-text>) test-case test-thunk)
  (print #`"-- Start test case ,(name-of test-case)")
  (test-thunk)
  (newline))

(define-method test-suite-run ((self <test-ui-text>) test-suite test-thunk)
  (let ((counter (make <real-time-counter>)))
    (print #`"- Start test suite ,(name-of test-suite)")
    (with-time-counter counter (test-thunk))
    (print
     (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
             (test-number-of test-suite)
             (assertion-number-of test-suite)
             (success-number-of test-suite)
             (failure-number-of test-suite)
             (error-number-of test-suite)))
    (print (format "Testing time: ~s" (time-counter-value counter)))))

(set-default-test-ui! (make <test-ui-text>))
