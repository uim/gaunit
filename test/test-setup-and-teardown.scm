#!/usr/bin/env gosh

(use test.unit)

(let ((setuped #f)
      (teardowned #f))
  (define-test-case
    "Test setup and teardown"
    (setup
     (lambda () (set! setuped '())))
    (teardown
     (lambda () (set! teardowned '())))
    ("test1"
     (set! setuped (append setuped '(#t)))
     (assert-equal '(#t) setuped)
     (assert-false teardowned))
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

(let ((setuped #f)
      (teardowned '(c)))
  (gaunit-add-default-setup-proc! (lambda () (set! setuped '())))
  (let ((proc (lambda () (push! setuped 'b))))
    (gaunit-add-default-setup-proc! proc)
    (gaunit-delete-default-setup-proc! proc))

  (gaunit-add-default-teardown-proc! (lambda () (push! teardowned 'c)))
  (let ((proc (lambda () (push! teardowned 'd))))
    (gaunit-add-default-teardown-proc! proc)
    (gaunit-delete-default-teardown-proc! proc))

  (define-test-case
    "Test default setup and teardown"
    (setup
     (lambda () (push! setuped 'a)))
    (teardown
     (lambda () (set! teardowned '())))
    ("test1"
     (set! setuped (append setuped '(#t)))
     (assert-equal '(a #t) setuped)
     (assert-equal '(c) teardowned)
     (set! teardowned #f))
    ("test2"
     (set! setuped (append setuped '(#t #t)))
     (assert-equal '(a #t #t) setuped)
     (assert-equal '(c) teardowned)
     (set! teardowned (append teardowned '(#t))))
    ("test3"
     (set! setuped (append setuped '(#t #t #t)))
     (assert-equal '(a #t #t #t) setuped)
     (assert-equal '(c) teardowned)
     (set! teardowned (append teardowned '(#t #t)))))
  
  (gaunit-clear-default-setup-procs!)
  (gaunit-clear-default-teardown-procs!))

(let ((setuped #f)
      (teardowned '(d f)))
  (gaunit-add-default-setup-proc! (lambda () (push! setuped 'c)))
  (let ((proc (lambda () (push! setuped 'b))))
    (gaunit-add-default-setup-proc! proc)
    (gaunit-delete-default-setup-proc! proc))
  (gaunit-add-default-setup-proc! (lambda () (set! setuped '())))

  (gaunit-add-default-teardown-proc! (lambda () (push! teardowned 'f)))
  (let ((proc (lambda () (push! teardowned 'e))))
    (gaunit-add-default-teardown-proc! proc)
    (gaunit-delete-default-teardown-proc! proc))
  (gaunit-add-default-teardown-proc! (lambda () (push! teardowned 'd)))
  
  (define-test-case
    "more Test default setup and teardown"
    (setup
     (lambda () (push! setuped 'a)))
    (teardown
     (lambda () (set! teardowned '())))
    ("test1"
     (set! setuped (append setuped '(#t)))
     (assert-equal '(a c #t) setuped)
     (assert-equal '(d f) teardowned)
     (set! teardowned #f))
    ("test2"
     (set! setuped (append setuped '(#t #t)))
     (assert-equal '(a c #t #t) setuped)
     (assert-equal '(d f) teardowned)
     (set! teardowned (append teardowned '(#t))))
    ("test3"
     (set! setuped (append setuped '(#t #t #t)))
     (assert-equal '(a c #t #t #t) setuped)
     (assert-equal '(d f) teardowned)
     (set! teardowned (append teardowned '(#t #t)))))
  
  (gaunit-clear-default-setup-procs!)
  (gaunit-clear-default-teardown-procs!))
