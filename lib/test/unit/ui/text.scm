(define-module test.unit.ui.text
  (extend test.unit.ui)
  (use test.unit)
  (use test.unit.color)
  (use gauche.time)
  (use srfi-2)
  (export <test-ui-text>))
(select-module test.unit.ui.text)

(define pair-attribute-get (with-module gauche.internal pair-attribute-get))

(define *color-schemes*
  `((default
      (success . ,(make-color "green" :bold #t))
      (failure . ,(make-color "red" :bold #t))
      (pending . ,(make-color "magenta" :bold #t))
      (omission . ,(make-color "blue" :bold #t))
      (notification . ,(make-color "cyan" :bold #t))
      (error . ,(make-color "yellow" :bold #t)))))

(define (guess-color-availability)
  (cond ((not (sys-isatty (current-output-port))) #f)
        ((and-let* ((term (sys-getenv "TERM"))
                    ((or (#/term\z/ term)
                         (equal? term "screen")))))
         #t)
        ((equal? (sys-getenv "EMACS") "t") #t)
        (else #f)))

(define-class <test-ui-text> (<test-ui-base>)
  ((successed :accessor successed-of)
   (verbose :accessor verbose-of :init-keyword :verbose
            :init-value :normal)
   (use-color :accessor use-color-of :init-keyword :use-color
              :init-thunk guess-color-availability)
   (color-scheme :accessor color-scheme-of
                 :init-form (cdr (assoc 'default *color-schemes*)))
   (reset-color :accessor reset-color-of
                :init-form (make-color "reset"))))

(define *verbose-level* (make-hash-table 'eq?))

(hash-table-put! *verbose-level* :silent 0)
(hash-table-put! *verbose-level* :progress 1)
(hash-table-put! *verbose-level* :normal 2)
(hash-table-put! *verbose-level* :verbose 3)

(define (level>=? l1 l2)
  (>= (hash-table-get *verbose-level* l1)
      (hash-table-get *verbose-level* l2)))

(define-method display-when ((self <test-ui-text>) color level message . options)
  (let-optionals* options ((print-proc display))
    (if (level>=? (verbose-of self) level)
      (let ((message (if (and (use-color-of self) color)
                       (string-append (escape-sequence-of color)
                                      message
                                      (escape-sequence-of (reset-color-of self)))
                       message)))
        (print-proc message)))))

(define (print-error-line stack)
  (and-let* ((line (error-line stack)))
    (print line)))

(define (color self key)
  (cdr (assoc key (color-scheme-of self))))

(define-method test-errored ((self <test-ui-text>) test err)
  (let ((stack-trace (retrieve-target-stack-trace
                      (cddddr (vm-get-stack-trace-lite)))))
    (set! (successed-of self) #f)
    (display-when self (color self 'error) :progress "E\n")
    (print-error-line (car stack-trace))
    (print #`"Error occurred in ,(name-of test)")
    (print (error-message err stack-trace :max-depth 5))))

(define-method test-successed ((self <test-ui-text>) test)
  #f)

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (set! (successed-of self) #f)
  (display-when self (color self 'failure) :progress "F\n")
  (print-error-line stack-trace)
  (print message #`" in ,(name-of test)"))
  ;; (print (error-message err (list stack-trace) :max-depth 5)))

(define-method test-start ((self <test-ui-text>) test)
  (set! (successed-of self) #t))

(define-method test-finish ((self <test-ui-text>) test)
  (if (successed-of self)
    (display-when self (color self 'success) :progress ".")))

(define-method test-case-start ((self <test-ui-text>) test-case)
  (display-when self #f :verbose #`"-- (test case) ,(name-of test-case): "))

(define-method test-case-finish ((self <test-ui-text>) test-case)
  (display-when self #f :verbose #\newline))

(define-method test-suite-start ((self <test-ui-text>) test-suite)
  (display-when self #f :normal #`"- (test suite) ,(name-of test-suite)\n"))

(define-method test-suite-finish ((self <test-ui-text>) test-suite)
  (display-when self #f :normal "\n")
  (display-when self #f :normal
   (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
           (test-number-of test-suite)
           (assertion-number-of test-suite)
           (success-number-of test-suite)
           (failure-number-of test-suite)
           (error-number-of test-suite))
   print)
  (display-when self #f :normal
   (format "Testing time: ~s" (operating-time-of test-suite)))
  (display-when self #f :progress "\n"))

(set-default-test-ui! (make <test-ui-text>))

(provide "test/unit/ui/text")
