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
         (assert-equal 3 3)
         (assert-equal 5 5)
         (assert-equal 1 -1)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-equal"
    ("Test assert-equal"
     (test-test-case-result test 1 2 1 0))))
     
(let ((test
       (make-test-case
        "Test assert-null"
        ("assert-null"
         (assert-null 1)
         (assert-null '())
         (assert-null '(1 1 2 -2))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-null"
    ("Test assert-null"
     (test-test-case-result test 1 1 2 0))))
     
(let ((test
       (make-test-case
        "Test assert-true"
        ("assert-true"
         (assert-true 1)
         (assert-true #t)
         (assert-true #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-true"
    ("Test assert-true"
     (test-test-case-result test 1 1 2 0))))
     
(let ((test
       (make-test-case
        "Test assert-false"
        ("assert-false"
         (assert-false 1)
         (assert-false #t)
         (assert-false #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-false"
    ("Test assert-falsa"
     (test-test-case-result test 1 1 2 0))))
     
(let ((test
       (make-test-case
        "Test assert-instance-of"
        ("assert-instance-of"
         (assert-instance-of <integer> 1)
         (assert-instance-of <integer> #t)
         (assert-instance-of <list> #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-instance-of"
    ("Test assert-instance-of"
     (test-test-case-result test 1 1 2 0))))
     
(let ((test
       (make-test-case
        "Test assert-raise"
        ("assert-raise"
         (assert-raise <error> (lambda () (1)))
         (assert-raise <integer> (lambda () (1)))
         (assert-raise <error> (lambda () #f))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-raise"
    ("Test assert-raise"
     (test-test-case-result test 1 1 2 0))))
     
(let ((test
       (make-test-case
        "Test assert-error"
        ("assert-error"
         (assert-error (lambda () (1)))
         (assert-error (lambda () 1))
         (assert-error #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-error"
    ("Test assert-error"
     (test-test-case-result test 1 1 2 0))))
     
(run-all-test)
