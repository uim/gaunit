#!/usr/bin/env gosh

(add-load-path ".")

(use test.unit)
(require "test/utils")

(let ((test
       (make-test-case "Test assert"
         ("assert"
          (assert eq? #t #t)
          (assert eq? #t #f)
          (assert (lambda (expected actual)
                    (= expected (apply + actual)))
                  10
                  '(1 2 3 4))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert"
    ("Test assert"
     (assert-test-case-result test 1 2 1 0))))

(let ((test
       (make-test-case "Test assert-equal"
         ("assert-equal"
          (assert-equal 3 3)
          (assert-equal 5 5)
          (assert-equal 1 -1)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-equal"
    ("Test assert-equal"
     (assert-test-case-result test 1 2 1 0))))

(let ((test
       (make-test-case "Test assert-null"
         ("assert-null"
          (assert-null 1)
          (assert-null '())
          (assert-null '(1 1 2 -2))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-null"
    ("Test assert-null"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-true"
         ("assert-true"
          (assert-true 1)
          (assert-true #t)
          (assert-true #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-true"
    ("Test assert-true"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-false"
         ("assert-false"
          (assert-false 1)
          (assert-false #t)
          (assert-false #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-false"
    ("Test assert-falsa"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-instance-of"
         ("assert-instance-of"
          (assert-instance-of <integer> 1)
          (assert-instance-of <integer> #t)
          (assert-instance-of <list> #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-instance-of"
    ("Test assert-instance-of"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-raise"
         ("assert-raise"
          (assert-raise <error> (lambda () (1)))
          (assert-raise <integer> (lambda () (1)))
          (assert-raise <error> (lambda () #f))))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-raise"
    ("Test assert-raise"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-error"
         ("assert-error"
          (assert-error (lambda () (1)))
          (assert-error (lambda () 1))
          (assert-error #f)))))
  (run-test-with-no-output test)
  (define-test-case "Test assert-error"
    ("Test assert-error"
     (assert-test-case-result test 1 1 2 0))))

(let ((test
       (make-test-case "Test assert-each"
         ("assert-each success"
          (assert-each assert-equal
                       '((1 1)
                         ("a" "a")))
          (assert-each assert-equal
                       `((1 ,(lambda () 1))
                         ("a" ,(lambda () "a")))
                       :prepare (lambda (test-case)
                                  (list (car test-case)
                                        ((cadr test-case)))))
          (assert-each assert-equal
                       `((1 ,(lambda () 1))
                         ("a" ,(lambda () "a")))
                       :run-assert (lambda (assert-proc expected actual)
                                     (assert-proc expected actual))
                       :prepare (lambda (test-case)
                                  (values (car test-case)
                                          ((cadr test-case)))))
          )
         ("assert-each fail-1"
          (assert-each assert-true
                       '(#t #f)))
         ("assert-each error"
          (assert-each assert-true
                       #t))
         )))
  (run-test-with-no-output test)
  ;; (run test)
  (define-test-case "Test assert-each"
    ("Test assert-each"
     (assert-test-case-result test 3 3 1 1))))

(define-macro (die message)
  `(error ,(x->string message)))

(let ((test
       (make-test-case "Test assert-macro1"
         ("assert-macro1 success3"
          (assert-macro1 '(error "error string!")
                         '(die "error string!"))
          (assert-macro1 '(error "error-symbol!")
                         '(die error-symbol!))
          (assert-macro1 '(error "1")
                         '(die 1))
          )
         ("assert-macro1 fail-1"
          (assert-macro1 '(error 1)
                         '(die 1)))
         ("assert-marcro1 error"
          (assert-macro1 '(error "syntax error")
                         '(die)))
         )))
  (run-test-with-no-output test)
  ;; (run test)
  (define-test-case "Test assert-macro1"
    ("Test assert-macro1"
     (assert-test-case-result test 3 3 1 1))))

(define-macro (or-die0 body message)
  `(or ,body
       (die ,message)))

(define-macro (or-die body message)
  `(or-die0 ,body ,message))

(let ((test
       (make-test-case "Test assert-macro"
         ("assert-macro success3"
          (assert-macro '(or #t
                             (die "shuld not be here!"))
                        '(or-die #t "shuld not be here!"))
          (assert-macro '(or #f
                             (die "always error!"))
                        '(or-die #f "always error!"))
          (assert-macro '(or (begin
                               #t
                               #f)
                             (die "always error too!"))
                        '(or-die (begin #t #f) "always error too!"))
          )
         ("assert-macro fail-1"
          (assert-macro '(and #t
                              (die "must be fail!"))
                        '(or-die #t "must be fail!")))
         ("assert-marcro error"
          (assert-macro '(or #t
                             (die "syntax error"))
                        '(or-die)))
         )))
  (run-test-with-no-output test)
  ;; (run test)
  (define-test-case "Test assert-macro"
    ("Test assert-macro"
     (assert-test-case-result test 3 3 1 1))))
