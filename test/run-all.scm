#!/usr/bin/env gosh

(use file.util)

(define (main args)
  (let ((dir (sys-dirname (car args))))
    (for-each (lambda (test-script)
                (eval `(load ,(string-join
                               (list dir test-script)
                               "/"))
                      (current-module)))
              (directory-list dir
                              :filter (lambda (x) (rxmatch #/^test-/ x)))
              )))
