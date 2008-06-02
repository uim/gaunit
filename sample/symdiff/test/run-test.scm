#!/usr/bin/env gosh

(add-load-path "./lib")

(use test.unit)

(define base-dir (sys-dirname *program-name*))
(for-each load (glob #`",|base-dir|/**/test-*.scm"))
