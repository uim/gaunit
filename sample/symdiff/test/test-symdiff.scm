#!/usr/bin/env gosh

(use test.unit)
(require "symdiff")

(define-test-case "Symbolic Differentiation"
  ("base test"
   (assert-equal 0 (deriv 1 'x))
   (assert-equal 1 (deriv 'x 'x))
   (assert-equal (make-sum (deriv 1 'x)
                           (deriv 'x 'x))
                 (deriv (make-sum 1 'x) 'x))
   (assert-equal (make-sum (make-product 1 (deriv 'x 'x))
                           (make-product (deriv 1 'x) 'x))
                 (deriv (make-product 1 'x) 'x))
   (assert-equal 0 (deriv 'y 'x)))
  ("sum test"
   (assert-equal '(+ 1 x) (make-sum 1 'x))
   (assert-true (sum? (make-sum 1 'x)))
   (assert-equal 1 (addend (make-sum 1 'x)))
   (assert-equal 'x (augend (make-sum 1 'x))))
  ("product test"
   (assert-equal '(* 2 x) (make-product 2 'x))
   (assert-false (product? (make-product 1 'x)))
   (assert-true (product? (make-product 2 'x)))
   (assert-equal 2 (multiplier (make-product 2 'x)))
   (assert-equal 'x (multiplicand (make-product 2 'x))))
  ("variable test"
   (assert-true (same-variable? 'x 'x))
   (assert-false (same-variable? 'x 'y))
   (assert-false (same-variable? 'x 1))
   (assert-true (variable? 'x))
   (assert-false (variable? 1)))
  ("more test"
   (assert-equal (make-sum (make-product 4 'x) 1)
                 (deriv (make-sum (make-product 2 (make-product 'x 'x))
                                  (make-sum 'x 3))
                        'x)))
  ("sum reduce"
   (assert-equal 'x (make-sum 'x 0))
   (assert-true (=number? 1 1))
   (assert-false (=number? 1 'x))
   (assert-equal 3 (make-sum 1 2))
   (assert-equal '(* 5 x) (make-sum (make-product 3 'x)
                                    (make-product 2 'x)))
   (assert-equal '(* 2 x) (make-sum 'x 'x)))
  ("product reduce"
   (assert-equal 'x (make-product 'x 1))
   (assert-equal 0 (make-product 'x 0))
   (assert-equal 6 (make-product 2 3))
   (assert-equal '(* 9 x) (make-product 3 (make-product 3 'x)))))
