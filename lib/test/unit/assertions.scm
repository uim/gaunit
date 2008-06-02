(define-module test.unit.assertions
  (extend test.unit.common)
  (use srfi-1)
  (use text.diff)
  (use slib)
  (use test.unit.base)
  (use test.unit.run-context)
  (use gauche.parameter)
  (export define-assertion))
(select-module test.unit.assertions)

(require 'pretty-print)

(define-class <assertion-failure> ()
  ((failure-message :accessor failure-message-of
                    :init-keyword :failure-message
                    :init-value "assertion failed")
   (actual :accessor actual-of
           :init-keyword :actual
           :init-value #f)
   (stack-trace :accessor stack-trace-of
                :init-keyword :stack-trace
                :init-form (retrieve-target-stack-trace))))

(define (assertion-failure? obj)
  (is-a? obj <assertion-failure>))

(define (assertion-failure message . actual)
  (raise (apply make-assertion-failure message actual)))

(define (make-assertion-failure message . actual)
  (let ((failure (make <assertion-failure>
                   :failure-message
                   (apply handle-failure-message message actual))))
    (when (get-optional actual #f)
      (slot-set! failure 'actual (get-optional actual #f)))
    failure))

(define (handle-failure-message message . options)
  (let-optionals* options ((actual #f))
    (if (procedure? message)
        (message actual)
        message)))

(define (format-diff from to)
  (let* ((from (with-output-to-string (lambda () (pretty-print from))))
         (to (with-output-to-string (lambda () (pretty-print to)))))
    (with-output-to-string
      (lambda ()
        (diff-report from to)))))

(define (make-message-handler expected . keywords)
  (let-keywords* keywords ((after-expected "")
                           (after-actual ""))
    (lambda (actual)
      (format #f
              (string-append
               "expected: <~s>~a\n"
               " but was: <~s>~a\n"
               "\n"
               "diff:\n"
               "~a")
              expected after-expected
              actual after-actual
              (regexp-replace #/\s*$/ (format-diff expected actual) "")))))

(define-method test-handle-exception ((test <test>)
                                      run-context (e <assertion-failure>))
  (test-run-context-failure run-context
                            test
                            (failure-message-of e)
                            (stack-trace-of e)))

(define-macro (define-assertion name&args . body)
  `(begin
     (if (eq? 'test.unit.assertions (module-name (current-module)))
       (export ,(car name&args)))
     (define ,name&args
       (parameterize ((count-assertion #f))
         ,@body)
       (if (count-assertion)
         (test-run-context-pass-assertion (test-run-context)
                                          (current-test))))))

(define-assertion (fail . message)
  (raise (make <assertion-failure>
           :failure-message (get-optional message " Failure!"))))

(define-assertion (assert pred expected actual . message)
  (if (pred expected actual)
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler expected))
       actual)))

(define-assertion (assert-equal expected actual . message)
  (apply assert equal? expected actual message))

(define-assertion (assert-not-equal expected actual . message)
  (assert (lambda (x y)
            (not (equal? x y)))
          expected
          actual
          (get-optional message
                        (make-message-handler
                         expected
                         :after-expected " to not be equal?"))))

(define-assertion (assert-null actual . message)
  (if (null? actual)
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler '()))
       actual)))

(define-assertion (assert-not-null actual . message)
  (if (not (null? actual))
    #t
    (assertion-failure
     (get-optional message
                   (make-message-handler
                    '()
                    :after-expected " to not be ()"))
     actual)))

(define-assertion (assert-true actual . message)
  (apply assert eq? #t actual message))

(define-assertion (assert-false actual . message)
  (apply assert eq? #f actual message))

(define-assertion (assert-instance-of expected-class object . message)
  (if (is-a? object expected-class)
      #t
      (assertion-failure
       (get-optional
        message
        (format #f
                " expected:<~s> is an instance of <~s>\n  but was:<~s>"
                object expected-class (class-of object)))
       object)))

(define-assertion (assert-raise expected-class thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (assert is-a? expected-class <class>
          " Should expect a class of exception")
  (guard (e (else
             (unless (is-a? e expected-class)
               (assertion-failure
                (get-optional
                 message
                 (format #f
                         " expected:<~s> class exception\n  but was:<~s>"
                         expected-class
                         (class-of e)))
                e))))
         (thunk)
         (assertion-failure
          (get-optional
           message
           (format #f
                   " expected:<~s> class exception\n  but none was thrown"
                   expected-class)))))

(define-assertion (assert-error thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (unless (guard (e (else #t))
                 (thunk)
                 #f)
    (assertion-failure
     (get-optional message " None expection was thrown"))))

(define-assertion (assert-error-message expected thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (with-error-handler
   (lambda (err)
     (let* ((msg (ref err 'message))
            (ok? (if (regexp? expected)
                   (and (rxmatch expected msg) #t)
                   (string=? expected msg))))
       (or ok?
           (make-assertion-failure
            (get-optional message
                          (format #f
                                  " expected:<~s>~a\n  but was:<~s>"
                                  expected
                                  (if (regexp? expected)
                                    " is match"
                                    "")
                                  msg))
            msg))))
   (lambda ()
     (thunk)
     (assertion-failure
      (get-optional message " None expection was thrown")))))

(define-assertion (assert-not-raise thunk . message)
  (assert-true (procedure? thunk)
               (format #f " <~s> must be procedure" thunk))
  (guard (e (else (assertion-failure
                   (get-optional
                    message
                    (format #f
                            (string-append
                             " expected no exception was thrown\n"
                             "  but <~s> class exception was thrown")
                            (class-of e)))
                   e)))
         (thunk)))

(define-assertion (assert-each assert-proc lst . keywords)
  (let-keywords* keywords ((apply-if-can #t)
                           (run-assert (lambda (assert-proc prepared-item)
                                         (if (and (list? prepared-item)
                                                  apply-if-can)
                                             (apply assert-proc prepared-item)
                                             (assert-proc prepared-item))))
                           (prepare (lambda (x) x)))
    (for-each (lambda (item)
                (call-with-values (lambda () (prepare item))
                  (lambda args
                    (apply run-assert assert-proc args))))
              lst)))

(define-assertion (assert-macro expanded form . message)
  (apply assert-equal expanded (macroexpand form) message))

(define-assertion (assert-macro1 expanded form . message)
  (apply assert-equal expanded (macroexpand-1 form) message))

(define-assertion (assert-lset-equal expected actual . message)
  (let ((result (lset= equal? expected actual)))
    (if result
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler
                      expected
                      :after-actual
                      (format #f
                              (string-append
                               "\n diff for expected<->actual:<~s>"
                               "\n diff for actual<->expected:<~s>")
                              (lset-difference equal? expected actual)
                              (lset-difference equal? actual expected))))
       actual))))

(define-assertion (assert-values-equal expected productor . message)
  (receive actual (productor)
    (apply assert equal? expected actual message)))

(define-assertion (assert-in-delta expected delta actual . message)
  (if (<= (- expected delta) actual (+ expected delta))
      #t
      (assertion-failure
       (get-optional
        message
        (make-message-handler expected
                              :after-expected (format #f " +/- <~s>" delta)))
       actual)))

(define-assertion (assert-output expected thunk . message)
  (let ((assert-proc (if (regexp? expected)
                       assert-match
                       assert-equal)))
    (apply assert-proc expected (with-output-to-string thunk) message)))

(define-assertion (assert-match expected actual . message)
  (if (regexp? expected)
    (if (rxmatch expected actual)
      #t
      (assertion-failure
       (get-optional message
                     (make-message-handler expected
                                           :after-expected " is matched"))
       actual))
    (assertion-failure
     (format #f "expected <~s> must be a regexp" expected))))

(provide "test/unit/assertions")
