(select-module test.unit)

(use gauche.vm.debugger)
(use gauche.time)
(use srfi-2)

(define-class <test-ui-text> ()
  ((verbose :accessor verbose-of :init-keyword :verbose
            :init-value :normal)))

(define *verbose-level* (make-hash-table 'eq?))

(hash-table-put! *verbose-level* :silent 0)
(hash-table-put! *verbose-level* :normal 1)
(hash-table-put! *verbose-level* :verbose 2)

(define (level>=? l1 l2)
  (>= (hash-table-get *verbose-level* l1)
      (hash-table-get *verbose-level* l2)))

(define-method display-when ((self <test-ui-text>) level message . options)
  (let-optionals* options ((print-proc display))
    (if (level>=? (verbose-of self) level)
        (print-proc message))))

(define (print-error-line stack)
  (and-let* ((code (car stack))
             ((pair? code))
             (info (pair-attribute-get code 'source-info #f))
             ((pair? info))
             ((pair? (cdr info))))
            (print (format "~a:~a: ~s" (car info) (cadr info) code))))
  
(define-method test-errored ((self <test-ui-text>) test err)
  (display-when self :normal "E\n")
  (print-error-line (cadddr (vm-get-stack-trace)))
  (print #`"Error occured in ,(name-of test)")
  (with-error-to-port (current-output-port)
                      (lambda ()
                        (report-error err))))

(define-method test-successed ((self <test-ui-text>) test)
  (display-when self :normal "."))

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (display-when self :normal "F\n")
  (print-error-line (car stack-trace))
  (print message #`" in ,(name-of test)")
  (with-error-to-port (current-output-port)
                      (lambda ()
                        (with-module gauche.vm.debugger
                                     (debug-print-stack
                                      stack-trace
                                      *stack-show-depth*)))))

(define-method test-run ((self <test-ui-text>) test test-thunk)
  (test-thunk))

(define-method test-case-run ((self <test-ui-text>) test-case test-thunk)
  (display-when self :verbose #`"-- Start test case ,(name-of test-case)\n")
  (test-thunk)
  (display-when self :verbose #\newline)
  )

(define-method test-suite-run ((self <test-ui-text>) test-suite test-thunk)
  (let ((counter (make <real-time-counter>)))
    (display-when self :verbose #`"- Start test suite ,(name-of test-suite)\n")
    (with-time-counter counter (test-thunk))
    (display-when self :normal "\n")
    (display-when
     self :normal
     (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
             (test-number-of test-suite)
             (assertion-number-of test-suite)
             (success-number-of test-suite)
             (failure-number-of test-suite)
             (error-number-of test-suite))
     print)
    (display-when
     self :normal
     (format "Testing time: ~s" (time-counter-value counter))
     print)))

(set-default-test-ui! (make <test-ui-text>))
