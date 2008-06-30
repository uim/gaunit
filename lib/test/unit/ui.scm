(define-module test.unit.ui
  (extend test.unit.common)
  (use test.unit.listener)
  (use srfi-1)
  (use srfi-2)
  (export test-succeeded test-failed test-erred
          test-run test-case-run
          test-case-setup test-case-teardown
          test-start test-case-start test-suite-start
          test-finish test-case-finish test-suite-finish))
(select-module test.unit.ui)

(define-class <test-ui-base> (<test-listener>)
  ((setup-success? :accessor setup-success?)))

(define-method test-erred (ui test err)
  (error "Not implimented"))

(define-method test-succeeded (ui test)
  (error "Not implimented"))

(define-method test-failed (ui test message stack-trace)
  (error "Not implimented"))

(define-method test-run (ui test test-thunk)
  (error "Not implimented"))

(define-method test-run ((ui <test-ui-base>) test test-thunk)
  (if (setup-success? ui)
    (test-thunk)))

(define-method test-start (ui test)
  (error "Not implimented"))

(define-method test-finish (ui test)
  (error "Not implimented"))

(define-method test-case-setup (ui test setup-thunk)
  (error "Not implimented"))

(define-method test-case-setup ((ui <test-ui-base>) test setup-thunk)
  (set! (setup-success? ui) (setup-thunk)))

(define-method test-case-teardown (ui test teardown-thunk)
  (error "Not implimented"))

(define-method test-case-teardown ((ui <test-ui-base>) test teardown-thunk)
  (teardown-thunk))

(define-method test-case-start (ui test-case)
  (error "Not implimented"))

(define-method test-case-finish (ui test-case)
  (error "Not implimented"))

(define-method test-suite-start (ui test-suite)
  (error "Not implimented"))

(define-method test-suite-finish (ui test-suite)
  (error "Not implimented"))

(define (error-line stack-trace)
  (and-let* (((pair? stack-trace))
             (info (pair-attribute-get stack-trace 'source-info #f))
             ((pair? info))
             ((pair? (cdr info))))
    (format #f "~a:~a: ~s" (car info) (cadr info) stack-trace)))

(define *stack-depth-limit* 15)

(define (show-stack-trace stack-trace . options)
  (let-keywords* options ((lines '())
                          (max-depth *stack-depth-limit*)
                          (skip 0)
                          (offset 0))
    (do ((stack stack-trace (cdr stack))
         (skip skip (- skip 1))
         (depth offset (+ depth 1)))
        ((or (null? (cdr stack))
             (>= depth max-depth))
         (string-join (reverse! lines) "\n"))
      (and-let* (((<= skip 0))
                 (line (error-line (car stack))))
        (push! lines (format #f "~a" line))))))

(define (error-message err stack-trace . options)
  (let ((messages '()))
    (when err
      (if (is-a? err <error>)
        (push! messages #`"*** ERROR: ,(ref err 'message)")
        (push! messages #`"*** ERROR: unhandled exception: ,err")))
    (push! messages (apply show-stack-trace stack-trace options))
    (string-join (reverse messages) "\n")))

(provide "test/unit/ui")
