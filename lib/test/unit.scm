(define-module test.unit
  (use srfi-1)
  (use gauche.collection)
  (use gauche.parameter)
  (use test.ui)
  (require "test/assertions")
  (require "test/autorunner")
  (export *gaunit-version*
          make-test make-test-case make-test-suite
          define-test-suite define-test-case
          run run-all-test
          reset-test-suites soft-reset-test-suites
          set-default-test-ui!
          success-of failure-of error-of
          name-of test-number-of assertion-number-of
          success-number-of failure-number-of error-number-of
          ))
(select-module test.unit)

(autoload test.ui.text <test-ui-text>)

(define *gaunit-version* "0.0.4")

(define test-result (make-parameter #f))
(define test-ui (make-parameter #f))
(define current-test (make-parameter #f))
(define count-assertion (make-parameter #t))

(define *default-test-ui* #f)
(define (set-default-test-ui! ui)
  (set! *default-test-ui* ui))
(define (default-test-ui)
  (unless *default-test-ui* <test-ui-text>)
  *default-test-ui*)

(define-class <result> ()
  ((success :accessor success-of :init-value 0)
   (failure :accessor failure-of :init-value 0)
   (error :accessor error-of :init-value 0)
   ))

(define-class <test> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (result :accessor result-of :init-thunk (lambda () (make <result>)))
   (asserts :accessor asserts-of :init-keyword :asserts :init-value '())
   ))

(define-method call-with-iterator ((coll <test>) proc . args)
  (apply call-with-iterator (asserts-of coll) proc args))

(define-class <test-case> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (tests :accessor tests-of :init-keyword :tests :init-value '())
   (setup :accessor setup-of :init-keyword :setup
          :init-value (lambda () #f))
   (teardown :accessor teardown-of :init-keyword :teardown
             :init-value (lambda () #f))
   ))

(define-method call-with-iterator ((coll <test-case>) proc . args)
  (apply call-with-iterator (tests-of coll) proc args))

(define-class <test-suite> (<collection>)
  ((name :accessor name-of :init-keyword :name)
   (test-cases :accessor test-cases-of :init-keyword :test-cases
               :init-value '())
   (setup :accessor setup-of :init-keyword :setup
          :init-value (lambda () #f))
   (teardown :accessor teardown-of :init-keyword :teardown
             :init-value (lambda () #f))
   (ran :accessor ran-of :init-value #f)
   ))

(define-method ran? ((self <test-suite>))
  (ran-of self))

(define-method set-ran! ((self <test-suite>) new-value)
  (slot-set! self 'ran new-value))

(define-method call-with-iterator ((coll <test-suite>) proc . args)
  (apply call-with-iterator (test-cases-of coll) proc args))

(define *default-test-suite* #f)
(define *test-suites* '())
(define (reset-default-test-suite)
  (set! *default-test-suite* (make <test-suite> :name "Default test suite")))
(define (reset-test-suites)
  (reset-default-test-suite)
  (set! *test-suites* (list *default-test-suite*))
  (soft-reset-test-suites))
(define (soft-reset-test-suites . options)
  (let-optionals* options ((suites *test-suites*))
    (for-each
     (lambda (suite)
       (set-ran! suite #f)
       (for-each
        (lambda (test-case)
          (for-each
           (lambda (test)
             (let ((result (result-of test)))
               (for-each
                (lambda (slot)
                  (slot-set! result slot 0))
                '(success failure error))))
           (tests-of test-case)))
        (test-cases-of suite)))
     suites)))

(reset-test-suites)

(define (run-all-test . options)
  (unless *default-test-suite* (eval '(use test.ui.text) (current-module)))
  (let-keywords* options ((ui (default-test-ui)))
    (for-each (lambda (suite)
                (if (and (not (null? (test-cases-of suite)))
                         (not (ran? suite)))
                    (run suite :ui ui)))
              (reverse *test-suites*))))

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
                     (make-test-case name rest ...)))
    ))

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
    ((_ name assert ...)
     (make <test>
       :name name
       :asserts (lambda () assert ... #f))))) ; #f is for get stack-trace

(define-syntax make-tests
  (syntax-rules ()
    ((_)
     '())
    ((_ (name assert ...))
     (list (make-test name assert ...)))
    ((_ (name assert ...) rest ...)
     (cons (make-test name assert ...)
           (make-tests rest ...)))))

(define-method setup ((self <test-case>))
  ((setup-of self)))

(define-method teardown ((self <test-case>))
  ((teardown-of self)))

(define-method run ((self <test-suite>) . options)
  (let-keywords* options ((ui (default-test-ui)))
    (test-suite-run
     ui
     self
     (lambda ()
       (for-each (lambda (test-case) (run test-case :ui ui))
                 (test-cases-of self))
       (set-ran! self #t)))))

(define-method run ((self <test-case>) . options)
  (let-keywords* options ((ui (default-test-ui)))
    (let ((setup-proc (lambda () (setup self)))
          (teardown-proc (lambda () (teardown self))))
      (test-case-run
       ui
       self
       (lambda ()
         (for-each (lambda (test)
                     (with-error-handler
                      (lambda (err)
                        (add-error! (result-of test) ui test err))
                      (lambda ()
                        (dynamic-wind
                            setup-proc
                            (lambda () (run test :ui ui))
                            teardown-proc))))
                   (tests-of self)))))))

(define-method run ((self <test>) . options)
  (let-keywords* options ((ui (default-test-ui)))
    (parameterize ((test-result (result-of self))
                   (test-ui ui)
                   (current-test self))
      (test-run ui self
                (lambda () ((asserts-of self)))))))

(define-method add-success! ((self <result>) test-ui test)
  (inc! (success-of self))
  (test-successed test-ui test))

(define-method add-failure! ((self <result>) test-ui test message stack-trace)
  (inc! (failure-of self))
  (test-failed test-ui test message stack-trace))

(define-method add-error! ((self <result>) test-ui test err)
  (inc! (error-of self))
  (test-errored test-ui test err))

(define-macro (define-assertion-number-of type)
  `(define-method assertion-number-of ((self ,type))
     (fold (lambda (get-number-proc prev)
             (+ prev (get-number-proc self)))
           0
           (list success-of failure-of error-of))))

(define-method x-of ((self <test>) get-x-proc)
  (get-x-proc (result-of self)))

(define-method success-of ((self <test>))
  (x-of self success-of))

(define-method failure-of ((self <test>))
  (x-of self failure-of))

(define-method error-of ((self <test>))
  (x-of self error-of))

(define-assertion-number-of <test>)

(define-method x-of ((self <test-case>) get-x-proc)
  (fold (lambda (test prev) (+ prev (get-x-proc test)))
        0
        self))

(define-method success-of ((self <test-case>))
  (x-of self success-of))

(define-method failure-of ((self <test-case>))
  (x-of self failure-of))

(define-method error-of ((self <test-case>))
  (x-of self error-of))

(define-assertion-number-of <test-case>)

(define-method x-of ((self <test-suite>) get-x-proc)
  (fold (lambda (test prev) (+ prev (get-x-proc test)))
        0
        self))

(define-method test-number-of ((self <test-suite>))
  (x-of self
        (lambda (test-case)
          (length (tests-of test-case)))))

(define-method success-of ((self <test-suite>))
  (x-of self success-of))
(define success-number-of success-of)

(define-method failure-of ((self <test-suite>))
  (x-of self failure-of))
(define failure-number-of failure-of)

(define-method error-of ((self <test-suite>))
  (x-of self error-of))
(define error-number-of error-of)

(define-assertion-number-of <test-suite>)

(provide "test/unit")
