(select-module test.unit)
(export define-assertion)

(define-syntax define-assertion
  (syntax-rules ()
    ((_ (name arg ...) message body ...)
     (begin
       (export name)
       (define (name arg ...)
         (if (test-result)
             (if (begin body ...)
                 (add-success! (test-result) (test-ui) (current-test))
                 (add-fail! (test-result) (test-ui) (current-test) message
                            (list (cadr (vm-get-stack-trace)))))))))))

(define-assertion (assert actual)
  (format "<not #f> expected but got <~s>" actual)
  actual)

(define-assertion (assert-equal expected actual)
  (format "<~s> expected but got <~s>" expected actual)
  (equal? expected actual))

(define-assertion (assert-true actual)
  (format "<#t> expected but got <~s>" actual)
  (eq? #t actual))

(define-assertion (assert-false actual)
  (format "<#f> expected but got <~s>" actual)
  (not actual))
