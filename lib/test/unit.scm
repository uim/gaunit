(define-module test.unit
  (use gauche.parameter)
  (require "test/assertions")
  (require "test/utils")
  (export *gaunit-version*
          make-test make-test-case make-test-suite
          define-test-suite define-test-case
          run run-all-test
          ))
(select-module test.unit)

(define *gaunit-version* "0.0.0")

(define *default-test-ui* #f)
(define (set-default-test-ui! ui)
  (set! *default-test-ui* ui))

(define test-result (make-parameter #f))
(define test-ui (make-parameter #f))
(define current-test (make-parameter #f))

(define-class <result> ()
  ((success :accessor success-of :init-value 0)
   (fail :accessor fail-of :init-value 0)
   (error :accessor error-of :init-value 0)
   ))

(define-class <test> ()
  ((name :accessor name-of :init-keyword :name)
   (result :accessor result-of)
   (asserts :accessor asserts-of :init-keyword :asserts :init-value '())
   ))

(define-method initialize ((self <test>) . args)
  (next-method)
  (set! (result-of self) (make <result>)))

(define-class <test-case> ()
  ((name :accessor name-of :init-keyword :name)
   (tests :accessor tests-of :init-keyword :tests :init-value '())
   (setup :accessor setup-of :init-keyword :setup
          :init-value (lambda () #f))
   (teardown :accessor teardown-of :init-keyword :teardown
             :init-value (lambda () #f))
   ))

(define-class <test-suite> ()
  ((name :accessor name-of :init-keyword :name)
   (test-cases :accessor test-cases-of :init-keyword :test-cases
               :init-value '())
   (setup :accessor setup-of :init-keyword :setup
          :init-value (lambda () #f))
   (teardown :accessor teardown-of :init-keyword :teardown
             :init-value (lambda () #f))
   ))

(define *default-test-suite* (make <test-suite> :name "Default test suite"))
(define *test-suites* (list *default-test-suite*))

(define (run-all-test . options)
  (unless *default-test-suite* (require "test/ui/text"))
  (let-keywords* options ((test-ui *default-test-ui*))
    (for-each (lambda (suite)
                (if (not (null? (test-cases-of suite)))
                    (run suite :test-ui test-ui)))
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
  (let-keywords* options ((test-ui *default-test-ui*))
    (test-suite-start test-ui self)
    (for-each (lambda (test-case) (run test-case :test-ui test-ui))
              (test-cases-of self))
    (test-suite-finish test-ui self)))

(define-method run ((self <test-case>) . options)
  (let-keywords* options ((test-ui *default-test-ui*))
    (let ((setup-proc (lambda () (setup self)))
          (teardown-proc (lambda () (teardown self))))
      (test-case-start test-ui self)
      (for-each (lambda (test)
                  (with-error-handler
                   (lambda (err)
                     (add-error! (result-of test) test-ui test err))
                   (lambda ()
                     (dynamic-wind
                         setup-proc
                         (lambda () (run test :test-ui test-ui))
                         teardown-proc))))
                (tests-of self))
      (test-case-finish test-ui self))))

(define-method run ((self <test>) . options)
  (let-keywords* options ((tu *default-test-ui*))
    (parameterize ((test-result (result-of self))
                   (test-ui tu)
                   (current-test self))
      (test-start tu self)
      ((asserts-of self))
      (test-finish tu self))))

(define-method add-success! ((self <result>) test-ui test)
  (inc! (success-of self))
  (test-successed test-ui test))

(define-method add-fail! ((self <result>) test-ui test message stack-trace)
  (inc! (fail-of self))
  (test-failed test-ui test message stack-trace))

(define-method add-error! ((self <result>) test-ui test err)
  (inc! (error-of self))
  (test-errored test-ui test err))

(define (xmap proc x)
  (map proc (x->list x)))

(define-method x->list ((self <test-case>))
  (tests-of self))

(define-method x->list ((self <test-suite>))
  (test-cases-of self))

(define-method map-result ((self <test-case>) proc)
  (xmap (lambda (test) (proc (result-of test)))
        self))

(define-method success-of ((self <test-case>))
  (apply + (map-result self
                       (lambda (result) (success-of result)))))
  
(define-method fail-of ((self <test-case>))
  (apply + (map-result self
                       (lambda (result) (fail-of result)))))

(define-method error-of ((self <test-case>))
  (apply + (map-result self
                       (lambda (result) (error-of result)))))
  
(define-method test-number-of ((self <test-suite>))
  (apply + (xmap (lambda (test-case)
                   (length (tests-of test-case)))
                 self)))
  
(define-method success-number-of ((self <test-suite>))
  (apply + (xmap (lambda (test-case) (success-of test-case))
                 self)))
  
(define-method failure-number-of ((self <test-suite>))
  (apply + (xmap (lambda (test-case) (fail-of test-case))
                 self)))
  
(define-method error-number-of ((self <test-suite>))
  (apply + (xmap (lambda (test-case) (error-of test-case))
                 self)))
  
(provide "test/unit")