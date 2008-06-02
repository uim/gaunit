(define-module test.unit.pending
  (extend test.unit.common)
  (use test.unit.base)
  (use test.unit.run-context)
  (export pend))
(select-module test.unit.pending)

(define-class <test-pending> ()
  ((message :accessor message-of
            :init-keyword :message
            :init-value "pending")
   (stack-trace :accessor stack-trace-of
                :init-keyword :stack-trace
                :init-form (retrieve-target-stack-trace))))

(define (pending? obj)
  (is-a? obj <test-pending>))

(define (pend message)
  (raise (make <test-pending> :message message)))

(define-method test-handle-exception ((test <test>)
                                      run-context (e <test-pending>))
  (test-run-context-pending run-context
                            test
                            (message-of e)
                            (stack-trace-of e)))

(provide "test/unit/pending")
