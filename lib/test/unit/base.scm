(define-module test.unit.base
  (extend test.unit.common)
  (use srfi-1)
  (use gauche.collection)
  (use gauche.parameter)
  (use gauche.time)
  (use test.unit.run-context)
  (use test.unit.ui)
  (export *gaunit-version*
          gaunit-default-test-suite
          gaunit-all-test-suite

          <test> <test-case> <test-suite>
          make-test make-test-case make-test-suite
          define-test-suite define-test-case
          test-run test-run-all
          reset-test-suites soft-reset-test-suites
          set-default-test-ui!
          name-of elapsed-of |setter of elapsed-of|

          test-handle-exception

          gaunit-add-default-setup-proc!
          gaunit-delete-default-setup-proc!
          gaunit-clear-default-setup-procs!
          gaunit-add-default-teardown-proc!
          gaunit-delete-default-teardown-proc!
          gaunit-clear-default-teardown-procs!
          ))
(select-module test.unit.base)

(autoload test.unit.ui.text <test-ui-text>)

(define *gaunit-version* "0.1.6")

(define *default-test-ui* #f)
(define (set-default-test-ui! ui)
  (set! *default-test-ui* ui))
(define (default-test-ui)
  (unless *default-test-ui* <test-ui-text>)
  *default-test-ui*)

(define-class <test> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (thunk :accessor thunk-of :init-keyword :thunk :init-value (lambda () #f))
   (elapsed :accessor elapsed-of :init-value 0)))

(define-method call-with-iterator ((self <test>) proc . args)
  (apply call-with-iterator (thunk-of self) proc args))

(define-class <test-case> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (tests :accessor tests-of :init-keyword :tests :init-value '())
   (setup-procedures :accessor setup-procedures-of :init-form '())
   (teardown-procedures :accessor teardown-procedures-of :init-form '())))

(define-method call-with-iterator ((coll <test-case>) proc . args)
  (apply call-with-iterator (tests-of coll) proc args))

(define *default-setup-procs* '())
(define *default-teardown-procs* '())

(define (gaunit-add-default-setup-proc! proc)
  (push! *default-setup-procs* proc))
(define (gaunit-delete-default-setup-proc! proc)
  (set! *default-setup-procs*
        (remove (cut eq? proc <>) *default-setup-procs*)))
(define (gaunit-clear-default-setup-procs!)
  (set! *default-setup-procs* '()))

(define (gaunit-add-default-teardown-proc! proc)
  (push! *default-teardown-procs* proc))
(define (gaunit-delete-default-teardown-proc! proc)
  (set! *default-teardown-procs*
        (remove (cut eq? proc <>) *default-teardown-procs*)))
(define (gaunit-clear-default-teardown-procs!)
  (set! *default-teardown-procs* '()))

(define-method initialize ((self <test-case>) args)
  (next-method)
  (set! (setup-procedures-of self)
        `(,@*default-setup-procs* ,(get-keyword :setup args #f)))
  (set! (teardown-procedures-of self)
        (cons (get-keyword :teardown args #f)
              (reverse *default-teardown-procs*))))

(define-class <test-suite> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (test-cases :accessor test-cases-of :init-keyword :test-cases
               :init-value '())
   (setup :accessor setup-of :init-keyword :setup
          :init-value (lambda () #f))
   (teardown :accessor teardown-of :init-keyword :teardown
             :init-value (lambda () #f))
   (ran :accessor ran-of :init-value #f)))

(define-method ran? ((self <test-suite>))
  (ran-of self))

(define-method set-ran! ((self <test-suite>) new-value)
  (slot-set! self 'ran new-value))

(define-method call-with-iterator ((coll <test-suite>) proc . args)
  (apply call-with-iterator (test-cases-of coll) proc args))

(define *default-test-suite* #f)
(define *test-suites* '())
(define (gaunit-default-test-suite) *default-test-suite*)
(define (gaunit-all-test-suite) *test-suites*)
(define (reset-default-test-suite)
  (set! *default-test-suite* (make <test-suite> :name "Default test suite")))
(define (reset-test-suites)
  (reset-default-test-suite)
  (set! *test-suites* (list *default-test-suite*))
  (soft-reset-test-suites))
(define (soft-reset-test-suites . options)
  #f)

(reset-test-suites)

(define (retrieve-procedure symbol module)
  (let ((value (with-error-handler
                   (lambda (e) #f)
                 (lambda ()
                   (eval symbol module)))))
    (if (procedure? value)
      value
      #f)))

(define (test-procedure? symbol module)
  (and-let* (((#/^test-/ (symbol->string symbol)))
             (procedure (retrieve-procedure symbol module)))
    (procedure-arity-includes? procedure 0)))

(define (collect-tests test-case-module)
  (map (lambda (symbol)
         (make <test>
           :name (symbol->string symbol)
           :thunk (lambda () (eval `(,symbol) test-case-module))))
       (filter (lambda (symbol)
                 (test-procedure? symbol test-case-module))
               (hash-table-keys (module-table test-case-module)))))

(define (make-test-case-from-module module)
  (make <test-case>
    :name (symbol->string (module-name module))
    :setup (retrieve-procedure 'setup module)
    :teardown (retrieve-procedure 'teardown module)
    :tests (collect-tests module)))

(define (find-test-case-modules base-test-case-module)
  (map make-test-case-from-module
       (filter (lambda (mod)
                 (member base-test-case-module (module-parents mod)))
               (all-modules))))

(define (make-default-run-context)
  (let ((run-context (make <test-run-context>)))
    (push! (listeners-of run-context) (default-test-ui))
    run-context))

(define (user-module-test-mode?)
  (and-let* ((user-module (find-module 'user))
             (test-unit-module (find-module 'test.unit))
             ((member test-unit-module (module-imports user-module))))
    user-module))

(define (add-user-module-test-case-if-need test-suite)
  (and-let* ((user-module (user-module-test-mode?)))
    (add-test-case! test-suite (make-test-case-from-module user-module))))

(define (test-run-all . options)
  (unless *default-test-suite*
    (eval '(use test.unit.ui.text) (current-module)))
  (for-each (lambda (test-case)
              (add-test-case! *default-test-suite* test-case))
            (find-test-case-modules (find-module 'test.unit.test-case)))
  (add-user-module-test-case-if-need *default-test-suite*)
  (let-keywords* options ((run-context (make-default-run-context))
                          (test-suite-regexp #//)
                          (test-case-regexp #//)
                          (test-regexp #//))
    (test-run-context-start run-context)
    (let ((success (fold (lambda (suite prev-success)
                           (and (if (and (not (null? (test-cases-of suite)))
                                         (not (ran? suite)))
                                  (test-run suite
                                            :run-context run-context
                                            :test-suite-regexp test-suite-regexp
                                            :test-case-regexp test-case-regexp
                                            :test-regexp test-regexp))
                                prev-success))
                         #t
                         (reverse *test-suites*))))
      (test-run-context-finish run-context)
      success)))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ name test-case ...)
     (push!
      *test-suites*
      (make <test-suite>
        :name name
        :test-cases (make-test-cases test-case ...))))))

(define-syntax make-test-suite
  (syntax-rules ()
    ((_ name test-case ...)
     (make <test-suite>
       :name name
       :test-cases (make-test-cases test-case ...)))))

(define-method add-test-case! ((self <test-suite>) test-case)
  (push! (test-cases-of self)
         test-case))

(define-method add-test-cases! ((self <test-suite>) test-cases)
  (for-each (lambda (test-case) (add-test-case! self test-cases))
            test-cases))

(define-syntax define-test-case
  (syntax-rules ()
    ((_ name) #f)
    ((_ name rest ...)
     (add-test-case! *default-test-suite*
                     (make-test-case name rest ...)))))

(define-syntax make-test-cases
  (syntax-rules ()
    ((_ (name test ...))
     (list (make-test-case name test ...)))
    ((_ (name test ...) rest ...)
     (cons (make-test-case name test ...)
           (make-test-cases rest ...)))))

(define-syntax make-test-case
  (syntax-rules (setup teardown)
    ((_ name (setup setup-proc) (teardown teardown-proc) test ...)
     (make <test-case>
       :name name
       :setup setup-proc
       :teardown teardown-proc
       :tests (make-tests test ...)))
    ((_ name (setup proc) test ...)
     (make <test-case>
       :name name
       :setup proc
       :tests (make-tests test ...)))
    ((_ name (teardown proc) test ...)
     (make <test-case>
       :name name
       :teardown proc
       :tests (make-tests test ...)))
    ((_ name test ...)
     (make <test-case>
       :name name
       :tests (make-tests test ...)))))

(define-syntax make-test
  (syntax-rules ()
    ((_ name assertion ...)
     (make <test>
       :name name
       :thunk (lambda () assertion ... #f))))) ; #f is for get stack-trace

(define-syntax make-tests
  (syntax-rules ()
    ((_)
     '())
    ((_ (name assertion ...))
     (list (make-test name assertion ...)))
    ((_ (name assertion ...) rest ...)
     (cons (make-test name assertion ...)
           (make-tests rest ...)))))

(define (apply-empty-argument-procedure procedure)
  (if (and (procedure? procedure)
           (procedure-arity-includes? procedure 0))
    (procedure)))

(define-method setup ((self <test-case>))
  (for-each apply-empty-argument-procedure (setup-procedures-of self)))

(define-method teardown ((self <test-case>))
  (for-each apply-empty-argument-procedure (teardown-procedures-of self)))

(use gauche.interactive)

(define (run-test-case test-suite run-context test-case
                       test-case-regexp test-regexp)
  (test-run test-case
            :run-context run-context
            :test-case-regexp test-case-regexp
            :test-regexp test-regexp))

(define-method test-run ((self <test-suite>) . options)
  (let-keywords* options ((run-context (make <test-run-context>))
                          (test-suite-regexp #//)
                          (test-case-regexp #//)
                          (test-regexp #//))
    (when (rxmatch test-suite-regexp (name-of self))
      (test-run-context-start-test-suite run-context self)
      (let* ((counter (make <real-time-counter>))
             (success (with-time-counter counter
                        (fold (lambda (test-case prev-success)
                                (and (run-test-case self run-context test-case
                                                    test-case-regexp
                                                    test-regexp)
                                     prev-success))
                              #t
                              (test-cases-of self)))))
        (set-ran! self #t)
        (test-run-context-finish-test-suite run-context self)
        success))))

(define-method test-handle-exception ((self <test-case>) (test <test>)
                                      run-context e)
  (test-run-context-error run-context test e (retrieve-target-stack-trace))
  #f)

(define (run-test test-case run-context test test-regexp
                  setup-proc teardown-proc)
  (let ((success (guard (e (else
                            (test-handle-exception test-case test
                                                   run-context e)))
                        (setup-proc)
                        (test-run test
                                  :run-context run-context
                                  :test-regexp test-regexp))))
    (guard (e (else
               (test-handle-exception test-case test run-context e)
               #f))
           (teardown-proc)
           success)))

(define-method test-run ((self <test-case>) . options)
  (let-keywords* options ((run-context (make <test-run-context>))
                          (test-case-regexp #//)
                          (test-regexp #//))
    (when (rxmatch test-case-regexp (name-of self))
      (test-run-context-start-test-case run-context self)
      (let* ((setup-proc (lambda () (setup self)))
             (teardown-proc (lambda () (teardown self)))
             (success (fold (lambda (test prev-success)
                              (and (parameterize ((test-run-context run-context))
                                     (run-test self run-context test test-regexp
                                               setup-proc teardown-proc))
                                   prev-success))
                            #t
                            (tests-of self))))
        (test-run-context-finish-test-case run-context self)
        success))))

(define-method test-handle-exception ((self <test>) run-context e)
  (test-run-context-error run-context self e (retrieve-target-stack-trace)))

(define-method test-run ((self <test>) . options)
  (let-keywords* options ((run-context (make <test-run-context>))
                          (test-regexp #//))
    (when (rxmatch test-regexp (name-of self))
      (let ((counter (make <real-time-counter>)))
        (test-run-context-start-test run-context self)
        (let ((success (guard (e (else
                                  (test-handle-exception self run-context e)))
                              (parameterize ((test-run-context run-context)
                                             (current-test self))
                                (with-time-counter counter ((thunk-of self))))
                              (test-run-context-success run-context self)
                              #t)))
          (set! (elapsed-of self) (time-counter-value counter))
          (test-run-context-finish-test run-context self)
          success)))))


(define-method elapsed-of ((self <test-suite>))
  (fold + 0
        (map elapsed-of (test-cases-of self))))

(define-method elapsed-of ((self <test-case>))
  (fold + 0
        (map elapsed-of (tests-of self))))


(provide "test/unit/base")
