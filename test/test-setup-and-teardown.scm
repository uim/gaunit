#!/usr/bin/env gosh

(use test.unit)

(let ((setuped '())
      (teardowned '()))
  (define-test-case
    "Test setup and teardown"
    (setup
     (lambda () (set! setuped '())))
    (teardown
     (lambda () (set! teardowned '())))
    ("test1"
     (set! setuped (append setuped '(#t)))
     (assert-equal '(#t) setuped)
     (assert-null teardowned))
    ("test2"
     (set! setuped (append setuped '(#t #t)))
     (set! teardowned (append teardowned '(#t)))
     (assert-equal '(#t #t) setuped)
     (assert-equal '(#t) teardowned))
    ("test3"
     (set! setuped (append setuped '(#t #t #t)))
     (set! teardowned (append teardowned '(#t #t)))
     (assert-equal '(#t #t #t) setuped)
     (assert-equal '(#t #t) teardowned))))
