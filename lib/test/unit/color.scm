(define-module test.unit.color
  (use srfi-1)
  (export make-color name-of sequence-of escape-sequence-of))
(select-module test.unit.color)

(define names '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"))

(define (make-color name . options)
  (apply make <color> :name name options))

(define-class <color> ()
  ((name :accessor name-of :init-value #f :init-keyword :name)
   (foreground :accessor foreground-of :init-value #t :init-keyword :foreground)
   (intensity :accessor intensity-of :init-value #f :init-keyword :intensity)
   (bold :accessor bold-of :init-value #f :init-keyword :bold)
   (italic :accessor italic-of :init-value #f :init-keyword :italic)
   (underline :accessor underline-of :init-value #f :init-keyword :underline)))

(define-method sequence-of ((self <color>))
  (define (foreground-parameter)
    (number->string (+ (if (foreground-of self)
                         3
                         4)
                       (if (intensity-of self)
                         6
                         0))))
  (define (name-index name)
    (list-index (lambda (color-name)
                  (equal? name color-name))
                names))
  (define (color-parameter)
    (let ((name (name-of self)))
      (cond ((equal? name "none")
             '())
            ((equal? name "reset")
             "0")
            (else
             #`",(foreground-parameter),(name-index name)"))))
  (remove null? `(,(color-parameter)
                  ,(if (bold-of self) "1" '())
                  ,(if (italic-of self) "3" '())
                  ,(if (underline-of self) "4" '()))))

(define-method escape-sequence-of ((self <color>))
  (let ((joined-sequence (string-join (sequence-of self) ";")))
    (if (equal? joined-sequence "")
      ""
      #`"\x1b[,|joined-sequence|m")))

(provide "test/unit/color")
