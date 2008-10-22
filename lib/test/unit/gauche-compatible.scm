(define-module test.unit.gauche-compatible
  (use test.unit.test-case)
  (export *test-error* test-start test-section test-end
          test* test test-module))
(select-module test.unit.gauche-compatible)

(define *test-error* (make <object>))

(define (test-start name)
  #f)

(define (test-section name)
  #f)

(define (test-end)
  #f)

(define-macro (test* name expect expression . compare)
  `(test ,name ,expect (lambda () ,expression) ,@compare))

(define (test name expect expression . compare)
  (if (eq? *test-error* expect)
    (assert-raise <exception> expression)
    (assert-equal expect (expression)))
  #f)

(define (test-module module)
  (assert-valid-module module)
  #f)

(provide "test/unit/gauche-compatible")
