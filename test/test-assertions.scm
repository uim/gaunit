(define-module test.test-assertions
  (extend test.unit.test-case)
  (use test.unit.base)
  (use test.unit.run-context)
  (use test.gaunit-test-utils))
(select-module test.test-assertions)

(define (test-assert)
  (assert-run-result
   #f
   0 1 2
   2 1 0 1 0
   `((failure "assert eq?"
              ,(string-append "expected: <#t>\n"
                              " but was: <#f>")))
   (make-test-case "Test assert"
                   ("assert eq?"
                    (assert eq? #t #t)
                    (assert eq? #t #f))
                   ("assert lambda"
                    (assert (lambda (expected actual)
                              (= expected (apply + actual)))
                            10
                            '(1 2 3 4)))))
  #f)

(define (test-assert-equal)
  (assert-run-result
   #f
   0 1 2
   2 1 0 1 0
   `((failure "assert-equal fail"
              ,(string-append
                "expected: <1>\n"
                " but was: <-1>")))
   (make-test-case "Test assert-equal"
                   ("assert-equal success"
                    (assert-equal 3 3)
                    (assert-equal 5 5))
                   ("assert-equal fail"
                    (assert-equal 1 -1))))
  #f)

(define (test-assert-equal-diff)
  (assert-run-result
   #f
   0 1 3
   0 0 0 3 0
   `((failure "2: assert-equal fail with multi lines"
              ,(string-append
                "expected: <\"a\nb\">\n"
                " but was: <\"a\nc\">\n"
                "\n"
                "diff:\n"
                "  \"a\n"
                "- b\"\n"
                "+ c\""))
     (failure "1: assert-equal fail with long line"
              ,(string-append
                "expected: <\"aaaaaaaaaaabaaaaaaaaaa\">\n"
                " but was: <\"aaaaaaaaaaaBaaaaaaaaaa\">"))
     (failure "3: assert-equal fail without diff"
              ,(string-append
                "expected: <\"a\">\n"
                " but was: <\"A\">")))
   (make-test-case "Test assert-equal"
                   ("3: assert-equal fail without diff"
                    (assert-equal "a" "A"))
                   ("1: assert-equal fail with long line"
                    (assert-equal "aaaaaaaaaaabaaaaaaaaaa"
                                  "aaaaaaaaaaaBaaaaaaaaaa"))
                   ("2: assert-equal fail with multi lines"
                    (assert-equal "a\nb" "a\nc"))))
  #f)

(define (test-assert-not-equal)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-not-equal fail2"
              ,(string-append "expected: <#t> to not be equal?\n"
                             " but was: <#t>"))
     (failure "assert-not-equal fail1"
              ,(string-append "expected: <3> to not be equal?\n"
                              " but was: <3>")))
   (make-test-case "Test assert-not-equal"
                   ("assert-not-equal success"
                    (assert-not-equal 1 -1))
                   ("assert-not-equal fail1"
                    (assert-not-equal 3 3))
                   ("assert-not-equal fail2"
                    (assert-not-equal #t #t))))
  #f)

(define (test-assert-null)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-null fail2"
              ,(string-append "expected: <()>\n"
                              " but was: <(1 1 2 -2)>"))
     (failure "assert-null fail1"
              ,(string-append "expected: <()>\n"
                              " but was: <1>")))
   (make-test-case "Test assert-null"
                   ("assert-null success"
                    (assert-null '()))
                   ("assert-null fail1"
                    (assert-null 1))
                   ("assert-null fail2"
                    (assert-null '(1 1 2 -2)))))
  #f)

(define (test-assert-not-null)
  (assert-run-result
   #f
   0 1 2
   2 1 0 1 0
   `((failure "assert-not-null fail"
              ,(string-append "expected: <()> to not be ()\n"
                              " but was: <()>")))
   (make-test-case "Test assert-not-null"
                   ("assert-not-null success"
                    (assert-not-null 1)
                    (assert-not-null '(1 1 2 -2)))
                   ("assert-not-null fail"
                    (assert-not-null '()))))
  #f)

(define (test-assert-true)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-true fail2"
              ,(string-append "expected: <#t>\n"
                              " but was: <#f>"))
     (failure "assert-true fail1"
              ,(string-append "expected: <#t>\n"
                              " but was: <1>")))
   (make-test-case "Test assert-true"
                   ("assert-true success"
                    (assert-true #t))
                   ("assert-true fail1"
                    (assert-true 1))
                   ("assert-true fail2"
                    (assert-true #f))))
  #f)

(define (test-assert-false)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-false fail2"
              ,(string-append "expected: <#f>\n"
                              " but was: <#t>"))
     (failure "assert-false fail1"
              ,(string-append "expected: <#f>\n"
                              " but was: <1>")))
   (make-test-case "Test assert-false"
                   ("assert-false success"
                    (assert-false #f))
                   ("assert-false fail1"
                    (assert-false 1))
                   ("assert-false fail2"
                    (assert-false #t))))
  #f)

(define (test-assert-instance-of)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-instance-of fail2"
              ,(string-append
                " expected:<#f> is an instance of <#<class <list>>>\n"
                "  but was:<#<class <boolean>>>"))
     (failure "assert-instance-of fail1"
              ,(string-append
                " expected:<#t> is an instance of <#<class <integer>>>\n"
                "  but was:<#<class <boolean>>>")))
   (make-test-case "Test assert-instance-of"
                   ("assert-instance-of success"
                    (assert-instance-of <integer> 1))
                   ("assert-instance-of fail1"
                    (assert-instance-of <integer> #t))
                   ("assert-instance-of fail2"
                    (assert-instance-of <list> #f))))
  #f)

(define (test-assert-raise)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-raise fail2"
              ,(string-append " expected:<#<class <error>>> class exception\n"
                              "  but was:<#<class <assertion-failure>>>"))
     (failure "assert-raise fail1"
              ,(string-append " expected:<#<class <integer>>> class exception\n"
                              "  but was:<#<class <error>>>")))
   (make-test-case "Test assert-raise"
                   ("assert-raise success"
                    (assert-raise <error> (lambda () (1))))
                   ("assert-raise fail1"
                    (assert-raise <integer> (lambda () (1))))
                   ("assert-raise fail2"
                    (assert-raise <error> (lambda () #f)))))
  #f)

(define (test-assert-error)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-error fail2"
              " <#f> must be procedure")
     (failure "assert-error fail1"
              " None expection was thrown"))
   (make-test-case "Test assert-error"
                   ("assert-error success"
                    (assert-error (lambda () (1))))
                   ("assert-error fail1"
                    (assert-error (lambda () 1)))
                   ("assert-error fail2"
                    (assert-error #f))))
  #f)

(define (test-assert-not-raise)
  (assert-run-result
   #f
   0 1 3
   1 1 0 2 0
   `((failure "assert-not-raise fail2"
              " <#f> must be procedure")
     (failure "assert-not-raise fail1"
              ,(string-append
                " expected no exception was thrown\n"
                "  but <#<class <error>>> class exception was thrown")))
   (make-test-case "Test assert-not-raise"
                   ("assert-not-raise success"
                    (assert-not-raise (lambda () 1)))
                   ("assert-not-raise fail1"
                    (assert-not-raise (lambda () (1))))
                   ("assert-not-raise fail2"
                    (assert-not-raise #f))))
  #f)

(define (test-assert-each)
  (assert-run-result
   #f
   0 1 3
   5 1 0 1 1
   `((error "assert-each error"
            "#<error \"list required, but got #t\">")
     (failure "assert-each fail-1"
              ,(string-append "expected: <#t>\n"
                              " but was: <#f>")))
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
                                 :run-assert (lambda (assert-proc
                                                      expected actual)
                                               (assert-proc expected actual))
                                 :prepare (lambda (test-case)
                                            (values (car test-case)
                                                    ((cadr test-case)))))
                    (assert-each (lambda args
                                   (if (null? (cdr args))
                                     (apply assert-true args)
                                     (assert-each assert-true args)))
                                 '(#t #t (#t #t) #t))
                    (assert-each (lambda (args)
                                   (if (list? args)
                                     (assert-each assert-true args)
                                     (assert-true args)))
                                 '(#t #t (#t #t) #t)
                                 :apply-if-can #f))
                   ("assert-each fail-1"
                    (assert-each assert-true
                                 '(#t #f)))
                   ("assert-each error"
                    (assert-each assert-true
                                 #t))))
  #f)

(define-macro (die message)
  `(error ,(x->string message)))

(define (test-assert-macro1)
  (assert-run-result
   #f
   0 1 3
   3 1 0 1 1
   `((error "assert-marcro1 error"
            ,(string-append
              "#<error \"wrong number of arguments for #<closure die> "
              "(required 1, got 0)\">"))
     (failure "assert-macro1 fail"
              ,(string-append "expected: <(error 1)>\n"
                              " but was: <(error \"1\")>")))
   (make-test-case "Test assert-macro1"
                   ("assert-macro1 success"
                    (assert-macro1 '(error "error string!")
                                   '(die "error string!"))
                    (assert-macro1 '(error "error-symbol!")
                                   '(die error-symbol!))
                    (assert-macro1 '(error "1")
                                   '(die 1))
                    )
                   ("assert-macro1 fail"
                    (assert-macro1 '(error 1)
                                   '(die 1)))
                   ("assert-marcro1 error"
                    (assert-macro1 '(error "syntax error")
                                   '(die)))))
  #f)

(define-macro (or-die0 body message)
  `(or ,body
       (die ,message)))

(define-macro (or-die body message)
  `(or-die0 ,body ,message))

(define (test-assert-macro)
  (assert-run-result
   #f
   0 1 3
   3 1 0 1 1
   `((error "assert-marcro error"
            ,(string-append
              "#<error \"wrong number of arguments for "
              "#<closure or-die> (required 2, got 0)\">"))
     (failure "assert-macro fail"
              ,(string-append "expected: <(and #t (die \"must be fail!\"))>\n"
                              " but was: <(or #t (die \"must be fail!\"))>")))
   (make-test-case "Test assert-macro"
                   ("assert-macro success"
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
                                  '(or-die (begin #t #f) "always error too!")))
                   ("assert-macro fail"
                    (assert-macro '(and #t
                                        (die "must be fail!"))
                                  '(or-die #t "must be fail!")))
                   ("assert-marcro error"
                    (assert-macro '(or #t
                                       (die "syntax error"))
                                  '(or-die)))))
  #f)

(define (test-assert-lset-equal)
  (assert-run-result
   #f
   0 1 3
   3 1 0 1 1
   `((error "assert-lset-equal error"
            "#<error \"argument must be a list, but got: #(1)\">")
     (failure "assert-lset-equal fail"
              ,(string-append
                "expected: <(a b c)>\n"
                " but was: <(a b c d)>\n"
                " diff for expected<->actual:<()>\n"
                " diff for actual<->expected:<(d)>")))
   (make-test-case "Test assert-lset-equal"
                   ("assert-lset-equal success"
                    (assert-lset-equal '(1 2 3)
                                       '(3 2 1))
                    (assert-lset-equal '(1 (2) 3)
                                       '(3 1 (2)))
                    (assert-lset-equal '((1) (2 3) (1))
                                       '((2 3) (1) (2 3))))
                   ("assert-lset-equal fail"
                    (assert-lset-equal '(a b c)
                                       '(a b c d)))
                   ("assert-lset-equal error"
                    (assert-lset-equal #(1) '(1)))))
  #f)

(define (test-assert-values-equal)
  (assert-run-result
   #f
   0 1 3
   3 1 0 1 1
   `((error "assert-values-equal error"
            "#<error \"invalid application: (1)\">")
     (failure "assert-values-equal fail"
              ,(string-append "expected: <(a b c)>\n"
                              " but was: <((a b c))>")))
   (make-test-case "Test assert-values-equal"
                   ("assert-values-equal success"
                    (assert-values-equal '(1 2 3)
                                         (lambda ()
                                           (values 1 2 3)))
                    (assert-values-equal '(1 (2) 3)
                                         (lambda ()
                                           (apply values '(1 (2) 3))))
                    (let ((productor (lambda () (values '(1) '(2 3) '(1)))))
                      (assert-values-equal '((1) (2 3) (1)) productor)))
                   ("assert-values-equal fail"
                    (assert-values-equal '(a b c)
                                         (lambda ()
                                           '(a b c))))
                   ("assert-values-equal error"
                    (assert-values-equal '(1) 1))))
  #f)

(define (test-assert-in-delta)
  (assert-run-result
   #f
   0 1 3
   3 1 0 1 1
   `((error "assert-in-delta error"
            ,(string-append
              "#<error \"wrong number of arguments for "
              "#<closure assert-in-delta> (required 3, got 2)\">"))
     (failure "assert-in-delta fail"
              ,(string-append
                "expected: <1> +/- <0.5>\n"
                " but was: <2>")))
   (make-test-case "Test assert-in-delta"
                   ("assert-in-delta success"
                    (assert-in-delta 0.9 0.1 1)
                    (assert-in-delta 0.9 0.01 0.899999)
                    (assert-in-delta -0.1 0.0001 -0.1000000009))
                   ("assert-in-delta fail"
                    (assert-in-delta 1 0.5 2))
                   ("assert-in-delta error"
                    (assert-in-delta 1 1))))
  #f)

(define (test-assert-output)
  (assert-run-result
   #f
   0 1 3
   4 1 0 1 1
   `((error "assert-output error-1"
            "#<error \"invalid application: (\\\"***\\\")\">")
     (failure "assert-output fail-1"
              ,(string-append "expected: <\"***\">\n"
                              " but was: <\"\">")))
   (make-test-case "Test assert-output"
                   ("assert-output success-4"
                    (assert-output #/\*+/ (lambda () (display "***")))
                    (assert-output "***\n" (lambda () (print "***")))
                    (assert-output "\n" newline)
                    (assert-output "" (lambda () #f)))
                   ("assert-output fail-1"
                    (assert-output "***" (lambda () #f)))
                   ("assert-output error-1"
                    (assert-output "***" "***"))))
  #f)

(define (test-assert-match)
  (assert-run-result
   #f
   0 1 4
   2 1 0 2 1
   `((error "assert-match error"
            "#<error \"invalid application: (\\\"***\\\")\">")
     (failure "assert-match fail2"
              "expected <\"***\"> must be a regexp")
     (failure "assert-match fail1"
              ,(string-append "expected: <#/\\*+/> is matched\n"
                              " but was: <\"\">")))
   (make-test-case "Test assert-match"
                   ("assert-match 2 passes"
                    (assert-match #/\*+/ "*****")
                    (assert-match #/.*/ ""))
                   ("assert-match fail1"
                    (assert-match #/\*+/ ""))
                   ("assert-match fail2"
                    (assert-match "***" "***"))
                   ("assert-match error"
                    (assert-match "***" ("***")))))
  #f)

(define (test-assert-not-match)
  (assert-run-result
   #f
   0 1 4
   2 1 0 2 1
   `((error "assert-not-match error"
            "#<error \"invalid application: (\\\"???\\\")\">")
     (failure "assert-not-match fail2"
              "expected <\"***\"> must be a regexp")
     (failure "assert-not-match fail1"
              ,(string-append
                "expected: <#/\\*+/> is not matched\n"
                " but was: <\"*** ERROR:\">")))
   (make-test-case "Test assert-not-match"
                   ("assert-not-match 2 passes"
                    (assert-not-match #/\*+/ "ERROR")
                    (assert-not-match #/^FIXME/ "<FIXME>"))
                   ("assert-not-match fail1"
                    (assert-not-match #/\*+/ "*** ERROR:"))
                   ("assert-not-match fail2"
                    (assert-not-match "***" "???"))
                   ("assert-not-match error"
                    (assert-not-match "***" ("???")))))
  #f)

(define (test-assert-valid-module)
  (assert-run-result
   #f
   0 1 4
   4 2 0 1 1
   `((error "assert-valid-module error - string"
            "#<error \"symbol required, but got \\\"nonexistent-module\\\"\">")
     (failure "assert-valid-module failure - nonexistent module"
              "expected: <nonexistent-module> is existent module"))
   (make-test-case "Test assert-valid-module"
                   ("assert-valid-module 2 passes - empty module"
                    (let ((anonymous-empty-module (make-module #f)))
                      (assert-valid-module anonymous-empty-module))
                    (make-module 'empty-module)
                    (assert-valid-module 'empty-module))
                   ("assert-valid-module failure - nonexistent module"
                    (assert-valid-module 'nonexistent-module))
                   ("assert-valid-module error - string"
                    (assert-valid-module "nonexistent-module"))
                   ("assert-valid-module 2 passes - valid symbols"
                    (let ((anonymous-valid-symbols-module (make-module #f)))
                      (eval '(begin
                               (export xxx)
                               (define xxx "xxx"))
                            anonymous-valid-symbols-module)
                      (assert-valid-module anonymous-valid-symbols-module))
                    (let ((valid-symbols-module
                           (make-module 'valid-symbols-module)))
                      (eval '(begin
                               (export xxx)
                               (define xxx "xxx"))
                            valid-symbols-module))
                    (assert-valid-module 'valid-symbols-module))))
  #f)

(provide "test/test-assertions")
