(select-module test.unit)

(export main)

(use srfi-13)
(use srfi-37)

(define (main args)
  (define options
    (list (option '(#\u "ui") #f #t
                  (lambda (option name arg ui verbose)
                    (values (if (string? arg)
                                (cond ((string-index arg #\t) <test-ui-text>)
                                      (else <test-ui-text>))
                                <test-ui-text>)
                             verbose)))
           (option '(#\v "verbose") #f #t
                   (lambda (option name arg ui verbose)
                     (values ui
                             (if (string? arg)
                                 (cond ((string-index arg #\s) :silent)
                                       ((string-index arg #\v) :verbose)
                                       (else :normal))
                                 :normal))))))
   (receive (ui verbose)
     (args-fold (cdr args)
       options
       (lambda (option name arg . seeds)         ; unrecognized
         (error "Unrecognized option:" name))
       (lambda (operand ui verbose) ; operand
         (values ui verbose))
       <test-ui-text> ; default value of ui
       :normal      ; default value of verbose
       )
     (run-all-test :ui (make ui :verbose verbose)))
  0)
