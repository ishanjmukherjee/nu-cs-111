;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tutorial_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define lst-of-lsts
  (list (list 1 2 3) 
        (list 4 5 6) 
        (list 1 1 1 1)))

(define word-lst (list "more" "orange" "run" "tree" "yeah" "igloo" "leaf" "isotope" "fly" "earth"))

; Activity 1
; type: (listof number)

(define num-list (list 2 3 4 5 6))

(check-expect (list? num-list)
              true)

; Activity 2
; type signature: (listof number) -> (listof number)

(define list-times-5
  (λ (inp-list)
    (map (λ (num) (* num 5))
         inp-list)))

(check-expect (list-times-5 (list 0 1 2 3))
              (list 0 5 10 15))

; Activity 3
; type signature: (listof number) -> (listof number)

(define activity-3
  (λ (inp-list)
    (filter (λ (num) (or (= num 2) (> num 5)))
            inp-list)))

(check-expect (activity-3 (list 2 2 0 5 6 7))
              (list 2 2 6 7))

; Activity 4
; type signature: (listof number) -> number

(define my-sum
  (λ (inp-list)
    (foldl + 0 inp-list)))

(check-expect (my-sum (list 1 2 3 4))
              10)

; Activity 5
; type signature: (listof number) -> boolean

(define all-less-than-three?
  (λ (inp-list)
    (andmap (λ (num) (< num 3))
            inp-list)))

(check-expect (all-less-than-three? (list 0 1 2))
              true)

(check-expect (all-less-than-three? (list 0 1 2 3 4 5))
              false)

; Activity 6
; type signature: (listof number) -> boolean

(define any-less-than-three?
  (λ (inp-list)
    (ormap (λ (num) (< num 3))
           inp-list)))

(check-expect (any-less-than-three? (list 0 4 5 6 3))
              true)

(check-expect (any-less-than-three? (list 5 5 6 10))
              false)

(check-expect (any-less-than-three? (list 3))
              false)

; Activity 7
; type signature: (listof (listof number)) -> number

(define lists-product
  (λ (inp-list)
    (foldl * 1 (map
                (λ (elem-list)
                  (foldl * 1 elem-list))
                inp-list))))

(check-expect (lists-product (list (list 1 2 3) 
                                   (list 4 5 6) 
                                   (list 1 1 1 1)))
              720)

(check-expect (lists-product (list (list 1 1 1)
                                   (list 1 0 1)
                                   (list 1 1 1)))
              0)

(check-expect (lists-product (list (list 1 1)
                                   (list 1 1)
                                   (list 1 1)
                                   (list 1 1)))
              1)

; Activity 8
; (listof string) -> string

(define first-letter-word-maker
  (λ (inp-list)
    (apply string-append
           (map
            (λ (string)
              (substring string 0 1))
            inp-list))))

(check-expect (first-letter-word-maker (list "hi"
                                             "orange"
                                             "under"
                                             "sunshine"
                                             "emperor"))
              "house")

; Activity 9

(define-struct cat (name breed hair-color meow-volume))

; Activity 10

(define my-cat (make-cat "Donald" "Manx" "white" 10))

; Activity 11

(define got-the-name (cat-name my-cat))
(define got-the-breed (cat-breed my-cat))

(check-expect (cat-name (make-cat "Ullman"
                                  "British Shorthair"
                                  "white"
                                  7))
              "Ullman")

(check-expect (cat-breed (make-cat "Ullman"
                                  "British Shorthair"
                                  "white"
                                  7))
              "British Shorthair")