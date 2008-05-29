(define-module test.unit.ui.text
  (extend test.unit.ui)
  (use test.unit)
  (use test.unit.color)
  (use gauche.time)
  (use gauche.sequence)
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
   (faults :accessor faults-of :init-form '())
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

(define-method output ((self <test-ui-text>) message . options)
  (let-optionals* options ((color #f)
                           (level :normal))
    (if (level>=? (verbose-of self) level)
      (let ((message (if (and (use-color-of self) color)
                       (string-append (escape-sequence-of color)
                                      message
                                      (escape-sequence-of (reset-color-of self)))
                       message)))
        (display message)))))

(define (output-error-line self stack)
  (and-let* ((line (error-line stack)))
    (output self #`",|line|\n")))

(define (color self key)
  (cdr (assoc key (color-scheme-of self))))

(define-method test-erred ((self <test-ui-text>) test err)
  (let ((stack-trace (retrieve-target-stack-trace
                      (cddddr (vm-get-stack-trace-lite)))))
    (set! (succeeded-of self) #f)
    (output self "E" (color self 'error) :progress)
    (push! (faults-of self)
           (list 'error "Error" test
                 (error-message err stack-trace :max-depth 5)
                 stack-trace))))

(define-method test-succeeded ((self <test-ui-text>) test)
  #f)

(define-method test-failed ((self <test-ui-text>) test message stack-trace)
  (set! (succeeded-of self) #f)
  (output self "F" (color self 'failure) :progress)
  (push! (faults-of self)
         (list 'failure "Failure" test message (list stack-trace))))

(define-method test-start ((self <test-ui-text>) test)
  (set! (succeeded-of self) #t))

(define-method test-finish ((self <test-ui-text>) test)
  (if (succeeded-of self)
    (output self "." (color self 'success) :progress)))

(define-method test-case-start ((self <test-ui-text>) test-case)
  (output self #`"-- (test case) ,(name-of test-case): " #f :verbose))

(define-method test-case-finish ((self <test-ui-text>) test-case)
  (output self "\n" #f :verbose))

(define-method test-suite-start ((self <test-ui-text>) test-suite)
  (output self #`"- (test suite) ,(name-of test-suite)\n" #f :verbose))

(define-method test-suite-finish ((self <test-ui-text>) test-suite)
  (output self "\n")
  (for-each-with-index
   (lambda (i args)
     (apply (lambda (i type label test message stack-trace)
              (output self "\n")
              (output self (format "~3d) ~a\n" (+ i 1) (name-of test)))
              (output self #`",message\n")
              (output-error-line self stack-trace))
            i
            args))
   (faults-of self))
  (output self (format "\nFinished in ~s seconds\n\n"
                       (operating-time-of test-suite)))
  (output self
          (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
                  (test-number-of test-suite)
                  (assertion-number-of test-suite)
                  (success-number-of test-suite)
                  (failure-number-of test-suite)
                  (error-number-of test-suite)))
  (output self "\n" #f :progress))

(set-default-test-ui! (make <test-ui-text>))

(provide "test/unit/ui/text")
