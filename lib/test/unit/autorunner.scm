(define-module test.unit.autorunner
  (use srfi-13)
  (use srfi-37)
  (use test.unit.base)
  (export main))
(select-module test.unit.autorunner)

(autoload test.unit.ui.text <test-ui-text>)
(autoload test.unit.ui.gtk <test-ui-gtk>)

(define (main args)
  (define default-ui (cons <test-ui-text> "text"))
  (define default-verbose (cons :normal "normal"))
  (define (usage)
    (print #`"\t-u, --ui=UI\t\tUse the given UI. (default ,(cdr default-ui))")
    (print "\t\t\t\t(t[ext], g[tk]).")
    (print #`"\t-v, --verbose=LEVEL\tSet the output LEVEL. (default ,(cdr default-verbose))")
    (print "\t\t\t\t(s[ilent], p[rogress], n[ormal], v[erbose]).")
    (print #`"\t-s, --test-suite=REGEXP\t\tRun only test suites which match REGEXP")
    (print #`"\t-c, --test-case=REGEXP\t\tRun only test cases which match REGEXP")
    (print #`"\t-t, --test=REGEXP\t\tRun only tests which match REGEXP")
    (print "\t-h, --help\t\tDisplay this help.")
    (exit -1))
  (define options
    (list (option '(#\u "ui") #t #f
                  (lambda (option name arg ui verbose suite case test . others)
                    (values (if (string? arg)
                              (cond ((string-index arg #\t) <test-ui-text>)
                                    ((string-index arg #\g) <test-ui-gtk>)
                                    (else (usage)))
                              ui)
                            verbose suite case test)))
          (option '(#\v "verbose") #t #f
                  (lambda (option name arg ui verbose suite case test . others)
                    (values ui
                            (if (string? arg)
                              (cond ((string-index arg #\s) :silent)
                                    ((string-index arg #\p) :progress)
                                    ((string-index arg #\n) :normal)
                                    ((string-index arg #\v) :verbose)
                                    (else (usage)))
                              verbose)
                            suite case test)))
          (option '(#\s "test-suite") #t #f
                  (lambda (option name arg ui verbose suite case test . others)
                    (values ui verbose
                            (if (string? arg)
                              (string->regexp arg :case-fold #t)
                              suite)
                            case test)))
          (option '(#\c "test-case") #t #f
                  (lambda (option name arg ui verbose suite case test . others)
                    (values ui verbose suite
                            (if (string? arg)
                              (string->regexp arg :case-fold #t)
                              case)
                            test)))
          (option '(#\t "test") #t #f
                  (lambda (option name arg ui verbose suite case test . others)
                    (values ui verbose suite case
                            (if (string? arg)
                              (string->regexp arg :case-fold #t)
                              test))))
          (option '(#\h "help") #f #f
                  (lambda (option name arg ui verbose . others)
                    (usage)))))
   (receive (ui verbose suite case test)
     (args-fold (cdr args)
       options
       (lambda (option name arg . seeds)         ; unrecognized
         (print "Unrecognized option:" name)
         (usage))
       (lambda (operand ui verbose suite case test) ; operand
         (values ui verbose suite case test))
       (car default-ui)
       (car default-verbose)
       #//
       #//
       #//)
     (run-all-test :ui (make ui :verbose verbose)
                   :test-suite-regexp suite
                   :test-case-regexp case
                   :test-regexp test))
  0)

(provide "test/unit/autorunner")
