#!/usr/bin/env gosh

(use test.unit)
(require "test/utils")

(let ((test
       (make-test-case 
        "Test assert"
        ("assert"
         (assert #t)
         (assert #f)
         (assert '(1 2 3))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert"
    ("Test assert"
     (test-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case
        "Test assert-equal"
        ("assert-equal"
         (assert-equal 1 (+ 1 2))
         (assert-equal 3 (+ 1 2))
         (assert-equal 1 (+ 1 2 -2))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-equal"
    ("Test assert-equal"
     (test-test-case-result test 1 2 1 0))))
     
(run-all-test)

; (let ((test
;        (make-test-suite
;         "Test test-suite"
;         ("Test case1"
;          ("Test assert1"
;           (assert #t))
;          ("Test assert-equal1"
;           (assert-equal 1 (+ 1 2))))
;         ("Test case2"
;          ("Test assert1"
;           (assert #f))
;          ("Test assert-equal2"
;           (assert-equal 1 (+ 1 2 -2)))
;          ("Test assert-instance-of"
;           (assert-instance-of <string> "a")
;           (assert-instance-of <integer> "a"))
;          ("Test assert-raise"
;           (assert-raise <error> (lambda () 1))
;           (assert-raise <error> (lambda () (1)))
;           (assert-raise 1 (lambda () (1)))
;           )
;          ("Test assert-error"
;           (assert-error (lambda () 1))
;           (assert-error (lambda () (1)))
;          )))))
;   (run test))

