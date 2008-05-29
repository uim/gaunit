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

(define (assert-test-case-result n-tests n-successes n-failures n-errors
                                 test-case)
  (assert-equal (list n-tests n-successes n-failures n-errors)
                (list (length (tests-of test-case))
                      (success-of test-case)
                      (failure-of test-case)
                      (error-of test-case))))

(define (assert-test-case-run-result n-tests n-successes n-failures n-errors
                                     test-case)
  (run-with-string-output test-case)
  (assert-test-case-result n-tests n-successes n-failures n-errors test-case))

(define (assert-test-suite-result n-tests n-assertions n-successes
                                  n-failures n-errors
                                  test-suite)
  (assert-equal (list n-tests n-assertions n-successes n-failures n-errors)
                (list (test-number-of test-suite)
                      (assertion-number-of test-suite)
                      (success-number-of test-suite)
                      (failure-number-of test-suite)
                      (error-number-of test-suite))))

(provide "test/gaunit-test-utils")
