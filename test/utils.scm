(define (run-test-with-no-output test)
  (call-with-output-string
   (cut with-output-to-port <>
        (lambda () (run test)))))

(define (test-test-case-result test-case test-num success-num failure-num error-num)
  (with-module test.unit
               (assert-equal test-num (length (tests-of test-case)))
               (assert-equal success-num (success-of test-case))
               (assert-equal failure-num (failure-of test-case))
               (assert-equal error-num (error-of test-case))))

(define (test-test-suite-result test-suite test-num assertion-num success-num failure-num error-num)
     (with-module test.unit
                  (assert-equal test-num (test-number-of test-suite))
                  (assert-equal assertion-num (assertion-number-of test-suite))
                  (assert-equal success-num (success-number-of test-suite))
                  (assert-equal failure-num (failure-number-of test-suite))
                  (assert-equal error-num (error-number-of test-suite))))
  