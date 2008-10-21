(define-module test.unit.gauche-compatible
  (use test.unit.base)
  (use test.unit.test-case)
  (export *test-error* test-start test-section test-end
          test* test test-module))
(select-module test.unit.gauche-compatible)

(define *test-error* (make <object>))
(define *test-case* #f)
(define *tests* '())

(define (test-start name)
  #f)

(define (test-section name)
  #f)

(define (test-end)
  #f)

(define-macro (test* name expect expression . compare)
  `(test ,name ,expect (lambda () ,expression) ,@compare))

(define tests-of (global-variable-ref 'test.unit.base 'tests-of))
(define (test name expect expression . compare)
  (if (eq? *test-error* expect)
    (assert-raise <exception> expression)
    (assert-equal expect (expression)))
  #f)

(define (test-module module)
  #f)

(provide "test/unit/gauche-compatible")
