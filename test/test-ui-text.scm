(define-module test.test-base
  (extend test.unit.test-case)
  (use test.unit.ui.text)
  (use test.gaunit-test-utils))
(select-module test.test-base)

(define %guess-color-availability
  (with-module test.unit.ui.text guess-color-availability))

(define empty-test-suite #f)
(define empty-test-case #f)
(define empty-test #f)
(define pending-test #f)

(define original-term-env #f)
(define original-emacs-env #f)

(define (setup)
  (set! empty-test-suite (make <test-suite> :name "empty test suite"))
  (set! empty-test-case (make <test-case> :name "empty test case"))
  (set! empty-test (make <test> :name "empty test"))

  (set! pending-test (make <test>
                       :name "pending test"
                       :thunk (lambda () (pend "not implemented yet"))))

  (set! original-term-env (sys-getenv "TERM"))
  (set! original-emacs-env (sys-getenv "EMACS")))

(define (teardown)
  (sys-putenv "TERM" original-term-env)
  (sys-putenv "EMACS" original-emacs-env))

(define (test-empty-test-suite)
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui empty-test-suite :verbose 'silent)))
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
                   (run-test-with-ui empty-test-case :verbose 'silent)))
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui empty-test-case)))
  (assert-output "-- (test case) empty test case:\n"
                 (lambda ()
                   (run-test-with-ui empty-test-case :verbose 'verbose)))
  #f)

(define (test-empty-test)
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui empty-test :verbose 'silent)))
  (assert-output "."
                 (lambda ()
                   (run-test-with-ui empty-test)))
  (assert-output "--- (test) empty test: .\n"
                 (lambda ()
                   (run-test-with-ui empty-test :verbose 'verbose)))
  #f)

(define (test-pending-test)
  (assert-output ""
                 (lambda ()
                   (run-test-with-ui pending-test :verbose 'silent)))
  (assert-output "P"
                 (lambda ()
                   (run-test-with-ui pending-test)))
  (assert-output "--- (test) pending test: P\n"
                 (lambda ()
                   (run-test-with-ui pending-test :verbose 'verbose)))
  #f)


(define (assert-color-available expected term-env emacs-env)
  (sys-putenv "TERM" term-env)
  (sys-putenv "EMACS" emacs-env)
  (assert-equal expected (%guess-color-availability))
  (with-output-to-string
    (lambda () (assert-false (%guess-color-availability)))))

(define (test-guess-color-availability)
  (unless (sys-isatty (current-output-port))
    (pend "need a tty output port for this test."))
  (assert-color-available #f "" "")
  (assert-color-available #t "" "t")
  (assert-color-available #t "xterm" "")
  (assert-color-available #t "mlterm" "")
  (assert-color-available #t "screen" "")
  (assert-color-available #t "xterm-color" "")
  (assert-color-available #f "unknown" ""))

(provide "test/test-base")
