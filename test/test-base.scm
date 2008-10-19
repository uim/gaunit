(define-module test.test-base
  (extend test.unit.test-case)
  (use test.unit.base))
(select-module test.test-base)

(define (test-module)
  (assert-valid-module 'test.unit.base)
  (assert-valid-module 'test.unit.test-case)
  (assert-valid-module 'test.unit.common)
  (assert-valid-module 'test.unit.pending)
  (assert-valid-module 'test.unit.run-context)
  (assert-valid-module 'test.unit.listener))

(define %test.unit.base (find-module 'test.unit.base))
(define %test-procedure? (eval 'test-procedure? %test.unit.base))

(define (test-have-many-arity x y z)
  #f)

(define (test-test-procedure?)
  (assert-true (%test-procedure? 'test-test-procedure? (current-module)))
  (assert-false (%test-procedure? 'test-have-many-arity (current-module)))
  (assert-false (%test-procedure? '%test.have.base (current-module)))
  (assert-false (%test-procedure? '%test-procedure? (current-module))))

(provide "test/test-base")
