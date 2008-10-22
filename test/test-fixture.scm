(define-module test.test-fixture
  (use test.unit.test-case)
  (use test.unit.base))
(select-module test.test-fixture)

(define setup-list '())
(define teardown-list '())

(define (setup)
  (set! setup-list '()))

(define (teardown)
  (set! teardown-list '()))

(define (test-touch-only-setup-list)
  (push! setup-list #t)
  (assert-equal '(#t) setup-list)
  (assert-null teardown-list))

(define (test-touch-both-lists)
  (push! setup-list #t)
  (push! teardown-list #t)
  (push! teardown-list #t)
  (assert-equal '(#t) setup-list)
  (assert-equal '(#t #t) teardown-list))

(define (test-only-teardown-list)
  (push! teardown-list #t)
  (push! teardown-list #t)
  (assert-null setup-list)
  (assert-equal '(#t #t) teardown-list))

(provide "test/test-fixture")
