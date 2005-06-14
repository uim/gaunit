(define-module test.unit.ui.text
  (extend test.unit.ui)
  (use test.unit)
  (use gauche.time)
  (use srfi-2)
  (export <test-ui-text>))
(select-module test.unit.ui.text)

(define pair-attribute-get (with-module gauche.internal pair-attribute-get))

(define-class <test-ui-text> ()
  ((successed :accessor successed-of)
   (verbose :accessor verbose-of :init-keyword :verbose
            :init-value :normal)))

(define *verbose-level* (make-hash-table 'eq?))

(hash-table-put! *verbose-level* :silent 0)
(hash-table-put! *verbose-level* :progress 1)
(hash-table-put! *verbose-level* :normal 2)
(hash-table-put! *verbose-level* :verbose 3)

(define (level>=? l1 l2)
  (>= (hash-table-get *verbose-level* l1)
      (hash-table-get *verbose-level* l2)))

(define-method display-when ((self <test-ui-text>) level message . options)
  (let-optionals* options ((print-proc display))
    (if (level>=? (verbose-of self) level)
        (print-proc message))))

(define (print-error-line stack)
  (and-let* ((line (error-line stack)))
    (print line)))
  
(define-method test-errored ((self <test-ui-text>) test err)
  (let ((stack-trace (cdddr (vm-get-stack-trace-lite))))
    (set! (successed-of self) #f)
    (display-when self :progress "E\n")
    (print-error-line (car stack-trace))
    (print #`"Error occurred in ,(name-of test)")
    (print (error-message err stack-trace :max-depth 5))))

(define-method test-successed ((self <test-ui-text>) test)
  #f)

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (set! (successed-of self) #f)
  (display-when self :progress "F\n")
  (print-error-line stack-trace)
  (print message #`" in ,(name-of test)"))
  ;; (print (error-message err (list stack-trace) :max-depth 5)))

(define-method test-start ((self <test-ui-text>) test)
  (set! (successed-of self) #t))

(define-method test-finish ((self <test-ui-text>) test)
  (if (successed-of self)
    (display-when self :progress ".")))

(define-method test-case-start ((self <test-ui-text>) test-case)
  (display-when self :verbose #`"-- (test case) ,(name-of test-case): "))

(define-method test-case-finish ((self <test-ui-text>) test-case)
  (display-when self :verbose #\newline))

(define-method test-suite-start ((self <test-ui-text>) test-suite)
  (display-when self :normal #`"- (test suite) ,(name-of test-suite)\n"))

(define-method test-suite-finish ((self <test-ui-text>) test-suite)
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
   (format "Testing time: ~s" (operating-time-of test-suite)))
  (display-when self :progress "\n"))

(set-default-test-ui! (make <test-ui-text>))

(provide "test/unit/ui/text")
