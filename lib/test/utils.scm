(define (inject proc lst . args)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst)
              (proc (car lst) result))))
  (let-optionals* args ((init (if (null? lst)
                                  #f
                                  (car lst))))
    (iter lst init)))
