(define-module test.unit
  (extend test.unit.base test.unit.assertions test.unit.pending
          test.unit.auto-runner))
(provide "test/unit")
