(use test.ui.text)

(define (run-test-with-no-output test)
  (call-with-output-string
   (cut with-output-to-port <>
        (lambda ()
          (run test :ui (make <test-ui-text> :verbose :silent))))))

(select-module test.unit)

(define (make-number-of-message-handler expect type)
  (make-message-handler expect :after-expected
                        (format " number of ~a" type)))

(define-assertion (assert-test-case-result test-case test-num
                                           success-num failure-num
                                           error-num)
  (assert-equal test-num (length (tests-of test-case))
                (make-number-of-message-handler test-num "test"))
  (assert-equal success-num (success-of test-case)
                (make-number-of-message-handler success-num "success"))
  (assert-equal failure-num (failure-of test-case)
                (make-number-of-message-handler failure-num "failure"))
  (assert-equal error-num (error-of test-case)
                (make-number-of-message-handler error-num "error")))

(define-assertion (assert-test-suite-result test-suite test-num
                                            assertion-num success-num
                                            failure-num error-num)
  (assert-equal test-num (test-number-of test-suite)
                (make-number-of-message-handler test-num "test"))
  (assert-equal assertion-num (assertion-number-of test-suite)
                (make-number-of-message-handler assertion-num "assertion"))
  (assert-equal success-num (success-number-of test-suite)
                (make-number-of-message-handler success-num "success"))
  (assert-equal failure-num (failure-number-of test-suite)
                (make-number-of-message-handler failure-num "failure"))
  (assert-equal error-num (error-number-of test-suite)
                (make-number-of-message-handler error-num "error")))

(define-assertion (assert-have-error-message str)
  (if (rxmatch #/\*\*\* ERROR:/ str)
      #t
      (make <assertion-failure>
        :failure-message
        (format " It seems to <~s>\n has no error message"
                str))))
