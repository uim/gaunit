(define-module test.test-base
  (extend test.unit.test-case)
  (use test.unit.ui.text)
  (use test.gaunit-test-utils))
(select-module test.test-base)

(define empty-test-suite #f)
(define empty-test-case #f)

(define (setup)
  (set! empty-test-suite (make <test-suite> :name "empty test suite"))
  (set! empty-test-case (make <test-case> :name "empty test case")))

(define (test-empty-test-suite)
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui empty-test-suite)))
  (assert-output "- (test suite) empty test suite:\n"
                 (lambda ()
                   (run-test-with-ui empty-test-suite :verbose 'verbose)))
  #f)

(define (test-empty-test-case)
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui empty-test-case)))
  (assert-output "-- (test case) empty test case:\n"
                 (lambda ()
                   (run-test-with-ui empty-test-case :verbose 'verbose)))
  #f)

(provide "test/test-base")
