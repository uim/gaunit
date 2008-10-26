#!/usr/bin/env gosh

(use file.util)

(define (gaunit-test-build-path . components)
  (let* ((test-dir (sys-dirname *program-name*))
         (top-dir (sys-realpath (build-path test-dir ".."))))
    (apply build-path top-dir components)))

(define-macro (%add-top-path-to-load-path)
  `(add-load-path ,(gaunit-test-build-path)))
(define-macro (%add-lib-path-to-load-path)
  `(add-load-path ,(gaunit-test-build-path "lib")))
(%add-lib-path-to-load-path)
(%add-top-path-to-load-path)

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
