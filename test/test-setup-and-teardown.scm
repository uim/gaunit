#!/usr/bin/env gosh

(use test.unit)

(let ((setuped '())
      (teardowned '()))
  (let ((test
         (make-test-suite
          "Test setup and teardown suite"
          ("Test setup and teardown"
           (setup
            (lambda () (push! setuped #t)))
           (teardown
            (lambda () (push! teardowned #t)))
           ("test1"
            (assert-equal '(#t) setuped)
            (assert-null teardowned))
           ("test2"
            (assert-equal '(#t #t) setuped)
            (assert-equal '(#t) teardowned))
           ("test3"
            (assert-equal '(#t #t #t) setuped)
            (assert-equal '(#t #t) teardowned))))))
    (run test)))
