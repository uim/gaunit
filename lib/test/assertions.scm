(select-module test.unit)
(export define-assertion)

(define-class <assertion-failure> ()
  ((failure-message :accessor failure-message-of
                    :init-keyword :failure-message
                    :init-value "assertion failed")
   (actual :accessor actual-of
           :init-keyword :actual
           :init-value #f)
   ))

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
      (format " expected:<~s>~a\n  but was:<~s>~a"
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
  (let ((prev-handler (current-exception-handler)))
    (call/cc
     (lambda (cont)
       (with-exception-handler
        (lambda (exn)
          (if (assertion-failure? exn)
              (cont exn)
              (call-with-values (prev-handler exn) cont)))
        (lambda ()
          (parameterize ((count-assertion #f))
            (body-thunk))))))))
  
(define-macro (define-assertion name&args . body)
  `(with-module test.unit
     (export ,(car name&args))
     (define ,name&args
       (if (test-result)
           (let ((result (eval-body (lambda () ,@body))))
             (if (assertion-failure? result)
                 (if (count-assertion)
                     (add-failure!
                      (test-result) (test-ui) (current-test)
                      (failure-message-of result)
                      (get-stack-trace))
                     (begin
                       (raise result)))
                 (when (count-assertion)
                   (add-success!
                    (test-result) (test-ui) (current-test)))))))))

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
        (format " expaceted:<~s> is an instance of <~s>\n  but was:<~s>"
                object expected-class (class-of object)))
       object)))

(define-assertion (assert-raise expected-class thunk . message)
  (assert is-a? expected-class <class>
          " Should expect a class of exception")
  (call/cc
   (lambda (cont)
     (let* ((prev-handler (current-exception-handler)))
       (with-exception-handler
        (lambda (exn)
          (cont
           (if (is-a? exn expected-class)
               #t
               (make-assertion-failure
                (get-optional
                 message
                 (format " expected:<~s> class exception\n  but was:<~s>"
                         expected-class (class-of exn)))
                exn))))
        (lambda ()
          (thunk)
          (make-assertion-failure
           (get-optional
            message
            (format " expected:<~s> class exception\n  but none was thrown"
                    expected-class)))))))))

(define-assertion (assert-error thunk . message)
  (assert-true (procedure? thunk) " Must be procedure")
  (with-error-handler
   (lambda (err) #t)
   (lambda ()
     (thunk)
     (assertion-failure
      (get-optional message " None expection was thrown")))))

(define-assertion (assert-each assert-proc lst . keywords)
  (let-keywords* keywords ((run-assert (lambda (assert-proc prepared-item)
                                         (apply assert-proc prepared-item)))
                           (prepare (lambda (x)
                                      (if (list? x) x (list x)))))
    (for-each (lambda (args)
                (call-with-values (lambda () (prepare args))
                  (lambda args
                    (apply run-assert assert-proc args))))
              lst)))

(define-assertion (assert-macro1 expanded form . message)
  (apply assert-equal expanded (macroexpand-1 form) message))

(define-assertion (assert-macro expanded form . message)
  (apply assert-equal expanded (macroexpand form) message))
