#!/usr/bin/env gosh

(add-load-path "./lib")

(use gauche.interactive)
(use file.util)
(use test.unit)

(if (and (symbol-bound? 'main)
         (not (symbol-bound? '_main)))
  (define _main main))

(define (main args)
  (let ((dir (sys-dirname (car args))))
    (for-each load (glob #`",|dir|/**/test-*.scm"))
    (if (symbol-bound? '_main)
      (_main `(,(car args) "-vn" ,@(cdr args)))
      (run-all-test))))
