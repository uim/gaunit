#!/usr/bin/env gosh

(use test.unit)

(let ((test (make-test-case "Test assertions"
             (setup
              (lambda ()
                (print "setuped")))
             (teardown
              (lambda ()
                (print "teardownd")))
             ("Test assert"
              (assert #t)
              (assert #f))
             ("Test assert-equal"
              (assert-equal 1 (+ 1 2))
              (assert-equal 3 (+ 1 2))
              (assert-equal 1 (+ 1 2 -2))))))
  (run test))

(let ((test
       (make-test-suite
        "Test test-suite"
        ("Test case1"
         (setup
          (lambda ()
            (print "case1 setuped")))
         ("Test assert1"
          (assert #t))
         ("Test assert-equal1"
          (assert-equal 1 (+ 1 2))))
        ("Test case2"
         (teardown
          (lambda ()
            (print "case2 teardowned")))
         ("Test assert1"
          (assert #f))
         ("Test assert-equal2"
          (assert-equal 1 (+ 1 2 -2))))
        )))
  (run test))

