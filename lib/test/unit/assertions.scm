(define-module test.unit.assertions
  (extend test.unit.common)
  (use srfi-1)
  (use test.unit.base)
  (use test.unit.result)
  (use gauche.parameter)
  (export define-assertion))
(select-module test.unit.assertions)

(define-class <assertion-failure> ()
  ((failure-message :accessor failure-message-of
                    :init-keyword :failure-message
                    :init-value "assertion failed")
   (actual :accessor actual-of
           :init-keyword :actual
           :init-value #f)))

(define (assertion-failure? obj)
  (is-a? obj <assertion-failure>))

(define (assertion-failure message . actual)
  (raise (apply make-assertion-failure message actual)))

(define (make-assertion-failure message . actual)
  (let ((failure (make <assertion-failure>
                   :failure-message
                   (apply handle-failure-message message actual))))
    (when (get-optional actual #f)
      (slot-set! failure 'actual (get-optional actual #f)))
    failure))

(define (handle-failure-message message . options)
  (let-optionals* options ((actual #f))
    (if (procedure? message)
        (message actual)
        message)))

(define (make-message-handler expected . keywords)
  (let-keywords* keywords ((after-expected "")
                           (after-actual ""))
    (lambda (actual)
      (format #f
              " expected:<~s>~a\n  but was:<~s>~a"
              expected after-expected
              actual after-actual))))

(define (get-stack-trace . options)
  (let-optionals* options ((stack-trace (cddr (vm-get-stack-trace))))
    (do ((s stack-trace (cdr s)))
        ((or (null? s)
             (rxmatch #/test\.unit::with-exception-handler/
                      (x->string (caar s))))
         (list (if (null? s)
                   (car stack-trace)
                   (cadr s)))))))

(define (eval-body body-thunk)
  (call/cc
   (lambda (cont)
     (with-exception-handler
      cont
      (lambda ()
        (parameterize ((count-assertion #f))
          (body-thunk)))))))
  
(define-macro (define-assertion name&args . body)
  `(with-module test.unit.assertions
     (export ,(car name&args))
     (define ,name&args
       (if (test-result)
         (let ((result (eval-body (lambda () ,@body))))
           (if (count-assertion)
             (cond ((assertion-failure? result)
                    (add-failure!
                     (test-result) (test-ui) (current-test)
                     (failure-message-of result)
                     (get-stack-trace)))
                   ((is-a? result <error>)
                    (add-error! (test-result) (test-ui)
                                (current-test) result))
                   (else
                    (add-success! (test-result) (test-ui)
                                  (current-test))))
             (if (or (assertion-failure? result)
                     (is-a? result <error>))
               (raise result))))))))

(define-assertion (fail . message)
  (raise (make <assertion-failure>
           :failure-message (get-optional message " Failure!"))))

(define-assertion (assert pred expected actual . message)
  (if (pred expected actual)
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler expected))
       actual)))

(define-assertion (assert-equal expected actual . message)
  (apply assert equal? expected actual message))

(define-assertion (assert-null actual . message)
  (if (null? actual)
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler '()))
       actual)))

(define-assertion (assert-true actual . message)
  (apply assert eq? #t actual message))

(define-assertion (assert-false actual . message)
  (apply assert eq? #f actual message))

(define-assertion (assert-instance-of expected-class object . message)
  (if (is-a? object expected-class)
      #t
      (assertion-failure
       (get-optional
        message
        (format #f
                " expaceted:<~s> is an instance of <~s>\n  but was:<~s>"
                object expected-class (class-of object)))
       object)))

(define-assertion (assert-raise expected-class thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (assert is-a? expected-class <class>
          " Should expect a class of exception")
  (call/cc
   (lambda (cont)
     (with-exception-handler
      (lambda (exn)
        (cont
         (if (is-a? exn expected-class)
             #t
             (make-assertion-failure
              (get-optional
               message
               (format #f
                       " expected:<~s> class exception\n  but was:<~s>"
                       expected-class (class-of exn)))
              exn))))
      (lambda ()
        (thunk)
        (make-assertion-failure
         (get-optional
          message
          (format #f
                  " expected:<~s> class exception\n  but none was thrown"
                  expected-class))))))))

(define-assertion (assert-error thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (with-error-handler
   (lambda (err) #t)
   (lambda ()
     (thunk)
     (assertion-failure
      (get-optional message " None expection was thrown")))))

(define-assertion (assert-not-raise thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (call/cc
   (lambda (cont)
     (with-exception-handler
      (lambda (exn)
        (cont (make-assertion-failure
               (get-optional
                message
                (format #f
                        (string-append " expected no exception was thrown\n"
                                       "  but <~s> class exception was thrown")
                        (class-of exn)))
               exn)))
      (lambda ()
        (thunk) #t)))))

(define-assertion (assert-each assert-proc lst . keywords)
  (let-keywords* keywords ((apply-if-can #t)
                           (run-assert (lambda (assert-proc prepared-item)
                                         (if (and (list? prepared-item)
                                                  apply-if-can)
                                             (apply assert-proc prepared-item)
                                             (assert-proc prepared-item))))
                           (prepare (lambda (x) x)))
    (for-each (lambda (item)
                (call-with-values (lambda () (prepare item))
                  (lambda args
                    (apply run-assert assert-proc args))))
              lst)))

(define-assertion (assert-macro1 expanded form . message)
  (apply assert-equal expanded (macroexpand-1 form) message))

(define-assertion (assert-macro expanded form . message)
  (apply assert-equal expanded (macroexpand form) message))

(define-assertion (assert-lset-equal expected actual . message)
  (let ((result (lset= equal? expected actual)))
    (if result
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler
                      expected
                      :after-actual
                      (format #f
                              (string-append
                               "\n diff for expected<->actual:<~s>"
                               "\n diff for actual<->expected:<~s>")
                              (lset-difference equal? expected actual)
                              (lset-difference equal? actual expected))))
       actual))))

(define-assertion (assert-values-equal expected productor . message)
  (receive actual (productor)
    (apply assert equal? expected actual message)))

(provide "test/unit/assertions")
