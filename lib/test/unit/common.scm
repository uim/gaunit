(define-module test.unit.common
  (use gauche.parameter)
  (use srfi-1))
(select-module test.unit.common)

(define test-result (make-parameter #f))
(define test-run-context (make-parameter #f))
(define current-test (make-parameter #f))
(define count-assertion (make-parameter #t))

(define pair-attribute-get
  (with-module gauche.internal pair-attribute-get))
(define (stack->source-info stack . fallback)
  (pair-attribute-get stack 'source-info (get-optional fallback #f)))
(define (stack-trace->source-info stack-trace . fallback)
  (let ((fallback (get-optional fallback #f)))
    (map (lambda (stack)
           (if (pair? stack)
             (list (stack->source-info stack fallback) stack)
             (list fallback stack)))
         stack-trace)))

(define (retrieve-target-stack-trace . options)
  (define (in-library? stack)
    (or (not (pair? stack))
        (rxmatch #/\/test\/unit\//
                 (car (stack->source-info stack '("") 0))))
    #f)
  (let-optionals* options ((stack-trace (cddr (vm-get-stack-trace-lite))))
    (remove in-library? (unwrap-syntax stack-trace))))

(provide "test/unit/common")
