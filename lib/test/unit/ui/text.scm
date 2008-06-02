(define-module test.unit.ui.text
  (extend test.unit.ui)
  (use test.unit)
  (use test.unit.color)
  (use test.unit.listener)
  (use test.unit.run-context)
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
                    ((or (#/term$/ term)
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

(define-method test-listener-on-start ((self <test-ui-text>) run-context)
  #f)

(define-method test-listener-on-start-test-suite ((self <test-ui-text>)
                                                  run-context
                                                  test-suite)
  (output self #`"- (test suite) ,(name-of test-suite)\n" #f :verbose))

(define-method test-listener-on-start-test-case ((self <test-ui-text>)
                                                 run-context
                                                 test-case)
  (output self #`"-- (test case) ,(name-of test-case): " #f :verbose))

(define-method test-listener-on-start-test ((self <test-ui-text>)
                                            run-context
                                            test)
  (set! (succeeded-of self) #t))

(define-method test-listener-on-success ((self <test-ui-text>) run-context test)
  (output self "." (color self 'success) :progress))

(define-method test-listener-on-pass-assertion ((self <test-ui-text>)
                                                run-context test)
  #f)

(define-method test-listener-on-failure ((self <test-ui-text>) run-context test
                                         message stack-trace)
  (set! (succeeded-of self) #f)
  (output self "F" (color self 'failure) :progress)
  (push! (faults-of self)
         (list 'failure "Failure" test
               #`",|message|\n,(error-message #f stack-trace :max-depth 5)"
               stack-trace)))

(define-method test-listener-on-error ((self <test-ui-text>)
                                       run-context test err)
  (let ((stack-trace (retrieve-target-stack-trace
                      (cdddr (vm-get-stack-trace-lite)))))
    (set! (succeeded-of self) #f)
    (output self "E" (color self 'error) :progress)
    (push! (faults-of self)
           (list 'error "Error" test
                 (error-message err stack-trace :max-depth 5)
                 stack-trace))))

(define-method test-listener-on-finish-test ((self <test-ui-text>)
                                             run-context test)
  (if (succeeded-of self)
    (output self "." (color self 'success) :progress)))

(define-method test-listener-on-finish-test-case ((self <test-ui-text>)
                                                  run-context
                                                  test-case)
  #f)

(define-method test-listener-on-finish-test-suite ((self <test-ui-text>)
                                                   run-context
                                                   test-suite)
  (output self "\n" #f :verbose))

(define-method test-listener-on-finish ((self <test-ui-text>) run-context)
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
  (output self (format "\nFinished in ~s seconds\n"
                       (elapsed-of run-context)))
  (output self "\n")
  (output self
          (format "~s tests, ~s assertions, ~s successes, ~s failures, ~s errors"
                  (n-tests-of run-context)
                  (n-assertions-of run-context)
                  (n-successes-of run-context)
                  (n-failures-of run-context)
                  (n-errors-of run-context))
          (color self (test-run-context-status run-context)))
  (output self "\n" #f :progress))

(set-default-test-ui! (make <test-ui-text>))

(provide "test/unit/ui/text")
