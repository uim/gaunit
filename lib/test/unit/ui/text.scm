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
  ((succeeded :accessor succeeded-of)
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

(define-method output ((self <test-ui-text>) color level message . options)
  (let-optionals* options ((print-proc display))
    (if (level>=? (verbose-of self) level)
      (let ((message (if (and (use-color-of self) color)
                       (string-append (escape-sequence-of color)
                                      message
                                      (escape-sequence-of (reset-color-of self)))
                       message)))
        (print-proc message)))))

(define (output-error-line self stack)
  (and-let* ((line (error-line stack)))
    (output self #f :progress line print)))

(define (color self key)
  (cdr (assoc key (color-scheme-of self))))

(define-method test-errored ((self <test-ui-text>) test err)
  (let ((stack-trace (retrieve-target-stack-trace
                      (cddddr (vm-get-stack-trace-lite)))))
    (set! (succeeded-of self) #f)
    (output self (color self 'error) :progress "E\n")
    (output-error-line self (car stack-trace))
    (output self #f :progress #`"Error occurred in ,(name-of test)" print)
    (output self #f :progress (error-message err stack-trace :max-depth 5)
            print)))

(define-method test-succeeded ((self <test-ui-text>) test)
  #f)

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (set! (succeeded-of self) #f)
  (output self (color self 'failure) :progress "F\n")
  (output-error-line self stack-trace)
  (output self #f :progress #`",message in ,(name-of test)" print))
  ;; (print (error-message err (list stack-trace) :max-depth 5)))

(define-method test-start ((self <test-ui-text>) test)
  (set! (succeeded-of self) #t))

(define-method test-finish ((self <test-ui-text>) test)
  (if (succeeded-of self)
    (output self (color self 'success) :progress ".")))

(define-method test-case-start ((self <test-ui-text>) test-case)
  (output self #f :verbose #`"-- (test case) ,(name-of test-case): "))

(define-method test-case-finish ((self <test-ui-text>) test-case)
  (output self #f :verbose #\newline))

(define-method test-suite-start ((self <test-ui-text>) test-suite)
  (output self #f :normal #`"- (test suite) ,(name-of test-suite)\n"))

(define-method test-suite-finish ((self <test-ui-text>) test-suite)
  (output self #f :normal "\n")
  (output self #f :normal
   (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
           (test-number-of test-suite)
           (assertion-number-of test-suite)
           (success-number-of test-suite)
           (failure-number-of test-suite)
           (error-number-of test-suite))
   print)
  (output self #f :normal
   (format "Testing time: ~s" (operating-time-of test-suite)))
  (output self #f :progress "\n"))

(set-default-test-ui! (make <test-ui-text>))

(provide "test/unit/ui/text")
