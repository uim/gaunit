(select-module test.unit)
(export define-assertion)

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
                          add-fail!)
                      `(,(test-result) ,(test-ui) ,(current-test)
                        ,@(if (eq? #t result)
                              '()
                              (list (message-handler result)
                                    (list (cadr (vm-get-stack-trace))))))))))))
    ))

(define-assertion (assert actual)
  (lambda (result)
    (format "<not #f> expected but got <~s>" result))
  actual)

(define-assertion (assert-equal expected actual)
  (lambda (result)
    (format "<~s> expected but got <~s>" expected actual))
  (equal? expected actual))

(define-assertion (assert-true actual)
  (lambda (result)
    (format "<#t> expected but got <~s>" actual))
  (eq? #t actual))

(define-assertion (assert-false actual)
  (lambda (result)
    (format "<#f> expected but got <~s>" actual))
  (not actual))

(define-assertion (assert-instance-of expected-class object)
  (lambda (result)
    (format "<~s> expected to be an instance of <~s> but was <~s>"
            object expected-class (class-of object)))
  (is-a? object expected-class))

(define-assertion (assert-raise expected-class thunk)
  (if (is-a? expected-class <class>)
      (lambda (result)
        (format "<~s> exception expected but ~a"
                expected-class result))
      (lambda (result)
        (format "Should expect a class of exception")))
  (if (is-a? expected-class <class>)
      (with-error-handler
       (lambda (err)
         (if (is-a? err expected-class)
             #t
             (format "was <~s>" (class-of err))))
       (lambda () (thunk) "none was thrown"))
      #f))

(define-assertion (assert-error thunk)
  (lambda (result)
    (format "none expection was thrown"))
  (with-error-handler
   (lambda (err) #t)
   (lambda () (thunk))))
