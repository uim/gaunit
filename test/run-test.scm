#!/usr/bin/env gosh

(add-load-path ".")
(use file.util)
(use test.unit)

(if (symbol-bound? 'main)
    (define _main main))

(define (main args)
  (let ((dir (sys-dirname (car args))))
    (for-each (lambda (test-script)
                ;; (reset-test-suites)
                (print "loading " (string-join
                                   (list dir test-script)
                                   "/"))
                (eval `(load ,(string-join
                               (list dir test-script)
                               "/"))
                      (current-module)))
              (directory-list dir
                              :filter (lambda (x) (rxmatch #/^test-/ x)))
              )
    (if (symbol-bound? '_main)
        (_main `(,(car args) "-vn" ,@(cdr args)))
        (run-all-test))))
