(define-module test.unit.test-case
  (extend test.unit.assertions test.unit.pending))
(select-module test.unit.test-case)


(provide "test/unit/test-case")

(if (find-module 'user)
  (with-module user
    (autoload test.unit.auto-runner main)))
