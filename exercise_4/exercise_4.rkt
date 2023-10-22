;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require "./iterated-images.rkt")

; not needed to implement your functions
; but very helpful for testing
; (require "./iterated-images.rkt")

; Part 1: Ordinary Recursion

; product-of : (listof number) -> number
; compute the product of a list of numbers
; (product-of '()) => 1

(define product-of
  (λ (inp-list)
    (if (empty? inp-list)
        1
        (* (first inp-list) (product-of (rest inp-list))))))

(check-expect (product-of (list 1 2 3 4))
              (* 1 2 3 4))

(check-expect (product-of (list))
              1)

(check-expect (product-of (list -1 -7 6 102.3))
              (* -1 -7 6 102.3))

; my-iterated-overlay : (number -> image) number -> image
; implement iterated-overlay using recursion
; (my-iterated-overlay generator 0) => empty-image

(define my-iterated-overlay
  (λ (generator n)
    (if (= n 0)
        empty-image
        (overlay (my-iterated-overlay generator (- n 1))
                 (generator (- n 1))))))

(check-expect (my-iterated-overlay (λ (n) (square (* n 50)
                                                  "solid"
                                                  (color (* n 50)
                                                         0
                                                         0)))
                                   5)
              (iterated-overlay (λ (n) (square (* n 50)
                                               "solid"
                                               (color (* n 50)
                                                      0
                                                      0)))
                                5))

(check-expect (my-iterated-overlay (λ (n) (square (* (+ n 1) 20)
                                                  "outline"
                                                  "black"))
                                   0)
              empty-image)

(check-expect (my-iterated-overlay (λ (n) (square (* (+ n 1) 20)
                                                  "outline"
                                                  "black"))
                                   10)
              (iterated-overlay (λ (n) (square (* (+ n 1) 20)
                                               "outline"
                                               "black"))
                                10))

; iterated-any : (image image -> image)  (number -> image) number -> image
; take an arbitrary combiner, a generator and a number of iterations, and outputs the result
; (iterated-any combiner generator 0) => empty-image

(define iterated-any
  (λ (combiner generator n)
    (if (= n 0)
        empty-image
        (combiner (iterated-any combiner generator (- n 1))
                  (generator (- n 1))))))

(check-expect (iterated-any overlay
                            (λ (n) (square (* n 10)
                                           "outline"
                                           "black"))
                            0)
              empty-image)

(check-expect (iterated-any above
                            (λ (n) (circle (* (+ n 1) 10)
                                           "solid"
                                           "yellow"))
                            0)
              empty-image)

(check-expect (iterated-any overlay
                            (λ (n) (square (* n 10)
                                           "outline"
                                           "black"))
                            3)
              (iterated-overlay (λ (n) (square (* n 10)
                                               "outline"
                                               "black"))
                                3))

(check-expect (iterated-any beside
                            (λ (n) (circle 10 "solid" "yellow"))
                            7)
              (iterated-beside (λ (n) (circle 10 "solid" "yellow"))
                               7))

(check-expect (iterated-any above
                            (λ (n) (circle (* (+ n 1) 10)
                                           "solid"
                                           "yellow"))
                            7)
              (iterated-above (λ (n) (circle (* (+ n 1) 10)
                                             "solid"
                                             "yellow"))
                              7))

; Part 2: Iterative Recursion

; product-of/iter : (listof number) -> number
; return the product of all elements in a list of numbers
; (product-of/iter '()) => partial-result

(define product-of/iter
  (λ (inp-list)
    (local [(define product-helper (λ (inp-list accumulator)
                                     (if (empty? inp-list)
                                         accumulator
                                         (* (first inp-list)
                                            (product-helper (rest inp-list)
                                                            accumulator)))))]
      (product-helper inp-list 1))))

(check-expect (product-of/iter (list 1 2 3 4 5))
              (* 1 2 3 4 5))

(check-expect (product-of/iter '())
              1)

(check-expect (product-of/iter (list 76 89 -100 0.8))
              (* 76 89 -100 0.8))

(check-expect (product-of/iter (list 1991 1976 1492 0))
              (* 1991 1976 1492 0))

; my-iterated-overlay/iter : (number -> image) number -> image
; iteratively recursive implementtion of iterated-overlay
; (my-iterated-overlay/iter generator 0) => partial-image

(define my-iterated-overlay/iter
  (λ (generator n)
    (local [(define overlay-helper (λ (generator n partial-image)
                                     (if (= n 0)
                                         partial-image
                                         (overlay (overlay-helper generator
                                                                  (- n 1)
                                                                  partial-image)
                                                  (generator (- n 1))))))]
      (overlay-helper generator n empty-image))))

(check-expect (my-iterated-overlay/iter (λ (n) (square (* n 50)
                                                       "solid"
                                                       (color (* n 50)
                                                              0
                                                              0)))
                                        5)
              (iterated-overlay (λ (n) (square (* n 50)
                                               "solid"
                                               (color (* n 50)
                                                      0
                                                      0)))
                                5))

(check-expect (my-iterated-overlay/iter (λ (n) (square (* (+ n 1) 20)
                                                       "outline"
                                                       "black"))
                                        0)
              empty-image)

(check-expect (my-iterated-overlay/iter (λ (n) (square (* (+ n 1) 20)
                                                       "outline"
                                                       "black"))
                                        10)
              (iterated-overlay (λ (n) (square (* (+ n 1) 20)
                                               "outline"
                                               "black"))
                                10)) 

; iterated-any : (image image -> image) (number -> image) number -> image
; take an arbitrary combiner, a generator and a number of iterations, and outputs the result using iterative recursion
; (iterated-any combiner generator 0) => partial-image

(define iterated-any/iter
  (λ (combiner generator n)
    (local [(define any-helper (λ (combiner generator n partial-image)
                                 (if (= n 0)
                                     partial-image
                                     (combiner (any-helper combiner
                                                           generator
                                                           (- n 1)
                                                           partial-image)
                                               (generator (- n 1))))))]
      (any-helper combiner generator n empty-image))))

(check-expect (iterated-any/iter overlay
                                 (λ (n) (square (* n 10)
                                                "outline"
                                                "black"))
                                 0)
              empty-image)

(check-expect (iterated-any/iter above
                                 (λ (n) (circle (* (+ n 1) 10)
                                                "solid"
                                                "yellow"))
                                 0)
              empty-image)

(check-expect (iterated-any/iter overlay
                                 (λ (n) (square (* n 10)
                                                "outline"
                                                "black"))
                                 3)
              (iterated-overlay (λ (n) (square (* n 10)
                                               "outline"
                                               "black"))
                                3))

(check-expect (iterated-any/iter beside
                                 (λ (n) (circle 10 "solid" "yellow"))
                                 7)
              (iterated-beside (λ (n) (circle 10 "solid" "yellow"))
                               7))

(check-expect (iterated-any/iter above
                                 (λ (n) (circle (* (+ n 1) 10)
                                                "solid"
                                                "yellow"))
                                 7)
              (iterated-above (λ (n) (circle (* (+ n 1) 10)
                                             "solid"
                                             "yellow"))
                              7))
