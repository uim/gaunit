#!/usr/bin/env gosh

(add-load-path "./lib")
(add-load-path ".")

(use test.unit)

(define base-dir (sys-dirname *program-name*))
(define gaunit-main main)
(define (main args)
  (let ((args (if (= 1 (length args))
                (append args
                       (if (symbol-bound? 'glob)
                         (glob #`",|base-dir|/**/test-*.scm")
                         (append (sys-glob #`",|base-dir|/test-*.scm")
                                 (sys-glob #`",|base-dir|/*/test-*.scm"))))
                args)))
    (gaunit-main args)))
