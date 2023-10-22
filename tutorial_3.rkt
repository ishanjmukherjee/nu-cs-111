;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tutorial_4_ishan_mukherjee) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; For a given n, returns n!
; type signauture: number -> number

(define factorial
  (λ (n)
    (if (<= n 0)
        1
        (* n (factorial (- n 1))))))

; For a given list of numbers, returns the number of odd numbers in it
; type signature: (listof number) -> number

(define count-odd
  (λ (inp-list)
    (if (empty? inp-list)
        0
        (if (odd? (first inp-list))
            (+ 1 (count-odd (rest inp-list)))
            (count-odd (rest inp-list))))))

; For a given predicate and a list of numbers, returns the number of elements that satisfy that predicate.
; type signature: (T -> boolean) (listof T) -> number

(define count
  (λ (p inp-list)
    (if (empty? inp-list)
        0
        (if (p (first inp-list))
            (+ 1 (count p (rest inp-list)))
            (count p (rest inp-list))))))

; For a given n, returns a tree picture of n layers
; type signature: number -> picture

(define tree
  (λ (n)
    (if (<= n -1)
        empty-image
        (local [(define subtree (tree (- n 1)))]
          (above (beside (rotate 45 (scale 0.5 subtree))
                         (rotate (- 45) (scale 0.5 subtree)))
                 (rectangle 15 100 "solid" "brown"))))))