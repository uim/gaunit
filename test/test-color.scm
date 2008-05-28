#!/usr/bin/env gosh

(add-load-path ".")

(use test.unit)
(use test.unit.color)
(require "test/utils")

(define (assert-escape-sequence expected color)
  (assert-equal expected (sequence-of color))
  (let ((escape-sequence (escape-sequence-of color)))
    (assert-match #/\x1b\[(?:\d+\;)*\d+m/ escape-sequence)
    (assert-equal expected
                  (string-split (substring escape-sequence
                                           2
                                           (- (string-length escape-sequence) 1))
                                ";"))))

(define-test-case "Test color"
  ("color"
   (assert-equal '() (sequence-of (make-color "none")))
   (assert-equal "" (escape-sequence-of (make-color "none")))
   (assert-escape-sequence '("31") (make-color "red"))
   (assert-escape-sequence '("32" "1") (make-color "green" :bold #t))
   (assert-escape-sequence '("0") (make-color "reset"))
   (assert-escape-sequence '("45") (make-color "magenta" :foreground #f))))
