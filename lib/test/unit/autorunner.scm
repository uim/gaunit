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
    (print "\t-h, --help\t\tDisplay this help.")
    (exit -1))
  (define options
    (list (option '(#\u "ui") #f #t
                  (lambda (option name arg ui verbose . others)
                    (values (if (string? arg)
                                (cond ((string-index arg #\t) <test-ui-text>)
                                      ((string-index arg #\g) <test-ui-gtk>)
                                      (else (usage)))
                                ui)
                             verbose)))
           (option '(#\v "verbose") #f #t
                   (lambda (option name arg ui verbose . others)
                     (values ui
                             (if (string? arg)
                                 (cond ((string-index arg #\s) :silent)
                                       ((string-index arg #\p) :progress)
                                       ((string-index arg #\n) :normal)
                                       ((string-index arg #\v) :verbose)
                                       (else (usage)))
                                 verbose))))
           (option '(#\h "help") #f #f
                   (lambda (option name arg ui verbose . others)
                     (usage)))))
   (receive (ui verbose)
     (args-fold (cdr args)
       options
       (lambda (option name arg . seeds)         ; unrecognized
         (print "Unrecognized option:" name)
         (usage))
       (lambda (operand ui verbose) ; operand
         (values ui verbose))
       (car default-ui)
       (car default-verbose)
       )
     (run-all-test :ui (make ui :verbose verbose)))
  0)

(provide "test/unit/autorunner")