(define-module test.unit.common
  (use gauche.parameter))
(select-module test.unit.common)

(define test-result (make-parameter #f))
(define test-ui (make-parameter #f))
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
                 (car (stack->source-info stack '("") 0)))))
  (let-optionals* options ((stack-trace (cddddr (vm-get-stack-trace-lite))))
    (car (fold (lambda (stack stack-info)
                 (receive (stacks in? out?)
                     (apply values stack-info)
                   (cond (out? (list stacks in? out?))
                         (in?
                          (let* ((in? (in-library? stack))
                                 (next-stacks (if in?
                                                stacks
                                                (list stack stacks))))
                            (list next-stacks in? out?)))
                         (else
                          (let* ((out? (in-library? stack))
                                 (next-stacks (if out?
                                                stacks
                                                (list stack stacks))))
                            (list next-stacks in? out?))))))
               '(() #t #f)
               stack-trace))))

(provide "test/unit/common")
