#!/usr/bin/env gosh

(use test.unit)
(require "test/utils")

(define-test-case "Test test.utils"
  ("test inject"
   (assert-equal 15
                 (inject +
                         '(1 2 3 4 5)
                         0))
   (assert-equal 24
                 (inject *
                         '(1 2 3 4)))))

(run-all-test)