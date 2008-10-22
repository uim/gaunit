(define-module test.unit.gauche
  (use srfi-1)
  (use srfi-37)
  (use test.unit.base)
  (export gaunit-gauche-file? gaunit-gauche-load))
(select-module test.unit.gauche)

(define (top-level-form? sexp)
  (and (list? sexp)
       (memq (car sexp)
             '(define use define-class define-module define-recode-type
                define-condition-type define-reader-ctor define-values
                define-macro define-syntax define-constant))))

(define (file->sexp-list file)
  (call-with-input-file file
    (lambda (input)
      (port->sexp-list (open-coding-aware-port input)))))

(define (gaunit-gauche-file? file)
  (let ((sexp-list (file->sexp-list file)))
    (find (lambda (sexp)
            (equal? sexp '(use gauche.test)))
          sexp-list)))

(define (gaunit-gauche-load file)
  (let* ((sexp-list (file->sexp-list file))
         (test-module (make-module #f))
         (test-case-name (cadar
                          (filter (lambda (sexp)
                                    (and (list? sexp)
                                         (equal? (car sexp) 'test-start)))
                                  sexp-list)))
         (test-case (make <test-case> :name test-case-name)))
    (eval '(extend test.unit.test-case)
          test-module)
    (eval '(use test.unit.gauche-compatible)
          test-module)
    (eval `(begin
             ,@(filter (lambda (sexp)
                         (and (top-level-form? sexp)
                              (not (equal? sexp '(use gauche.test)))))
                       sexp-list))
          test-module)
    (push! (tests-of test-case)
           (make <test>
             :name test-case-name
             :thunk (eval `(lambda ()
                             ,@(remove (lambda (sexp)
                                         (or (equal? sexp '(use gauche.test))
                                             (top-level-form? sexp)))
                                       sexp-list))
                          test-module)))
    (test-suite-add-test-case! (gaunit-default-test-suite)
                               test-case)))

(provide "test/unit/gauche")
