(define-module test.assertions
  (export-all)
  )

(define-syntax define-assertion
  (syntax-rules ()
    ((_ (name arg ...) message-handler body ...)
     (begin
       (export name)
       (define (name arg ...)
         (if (test-result)
             (let ((result (begin body ...)))
               (apply (if (eq? #t result)
                          add-success!
                          add-failure!)
                      `(,(test-result) ,(test-ui) ,(current-test)
                        ,@(if (eq? #t result)
                              '()
                              (list (message-handler result)
                                    (list (cadr (vm-get-stack-trace))))))))))))
    ))

(define-assertion (assert actual)
  (lambda (result)
    (format " expected:<not #f> but was:<~s>" result))
  actual)

(define-assertion (assert-equal expected actual)
  (lambda (result)
    (format " expected:<~s> but was:<~s>" expected actual))
  (equal? expected actual))

(define-assertion (assert-null actual)
  (lambda (result)
    (format " expected:<null> but was:<~s>" actual))
  (null? actual))

(define-assertion (assert-true actual)
  (lambda (result)
    (format " expected:<#t> but was:<~s>" actual))
  (eq? #t actual))

(define-assertion (assert-false actual)
  (lambda (result)
    (format " expected:<#f> but was:<~s>" actual))
  (not actual))

(define-assertion (assert-instance-of expected-class object)
  (lambda (result)
    (format " expaceted:<~s> is an instance of <~s> but was:<~s>"
            object expected-class (class-of object)))
  (is-a? object expected-class))

(define-assertion (assert-raise expected-class thunk)
  (if (is-a? expected-class <class>)
      (lambda (result)
        (format " expected:<~s> class exception but ~a"
                expected-class result))
      (lambda (result)
        (format " Should expect a class of exception")))
  (if (is-a? expected-class <class>)
      (with-error-handler
       (lambda (err)
         (if (is-a? err expected-class)
             #t
             (format "was:<~s>" (class-of err))))
       (lambda () (thunk) "none was thrown"))
      #f))

(define-assertion (assert-error thunk)
  (lambda (result)
    (format " None expection was thrown"))
  (if (procedure? thunk)
      (with-error-handler
       (lambda (err) #t)
       (lambda () (thunk) #f))
      #f))

(provide "test.assertions")