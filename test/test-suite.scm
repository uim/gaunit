#!/usr/bin/env gosh

(use test.unit)

(let ((setuped #f))
  (define-test-suite "Test suite1"
    ("test case1"
;      (setup
;       (lambda ()
;          (print setuped)
;          (print "case1 setuped")
;          (set! setuped #t)
;          (print setuped)
;         ))
;      (teardown
;       (lambda ()
;         (print "case1 teardowned")))
     ("test1"
      (assert #t)
      (assert #f))
     ("test2"
      (assert-equal 1 (- 2 1))))
    ("test case2"
;      (setup
;       (lambda ()
;         (print "case2 setuped")))
;      (teardown
;       (lambda ()
;         (print "case2 teardowned")))
     ("test1"
      (assert #t)
      (assert #f)
      (assert #t))
     ("test2"
      (assert-equal 1 (- 2 1))
      (assert-equal 1 (- 2 3))))))

(run-all-test)