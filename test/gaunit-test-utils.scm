(define-module test.gaunit-test-utils
  (use test.unit)
  (use test.unit.ui.text)
  (export-all))
(select-module test.gaunit-test-utils)

(define (run-with-string-output test)
  (call-with-output-string
    (cut with-output-to-port <>
         (lambda ()
           (run test :ui (make <test-ui-text>))))))

(define (assert-test-case-run-result n-tests n-successes n-failures n-errors
                                     test-case)
  (run-with-string-output test-case)
  (assert-equal (list n-tests n-successes n-failures n-errors)
                (list (length (tests-of test-case))
                      (success-of test-case)
                      (failure-of test-case)
                      (error-of test-case))))

(provide "test/gaunit-test-utils")
