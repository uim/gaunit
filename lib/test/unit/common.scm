(define-module test.unit.common
  (use gauche.parameter))
(select-module test.unit.common)

(define *gaunit-version* "0.1.1")

(define test-result (make-parameter #f))
(define test-ui (make-parameter #f))
(define current-test (make-parameter #f))
(define count-assertion (make-parameter #t))

(provide "test/unit/common")
