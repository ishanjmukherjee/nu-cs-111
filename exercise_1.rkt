;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Please write your code here!

;; These are tests to make sure all of your images are named correctly for grading.
;; These don't guarantee all of your tasks are correct...but it does give you an indication that you're on the
;; right track. When you're ready to double-check an exercise, uncomment the appropriate lines by deleting
;; the semi-colon (;)

(require 2htdp/image)
;; Activity 1
(check-expect (image? a-red-square) #t)
(define a-red-square (square 100 "solid" "red"))

;; Activity 2
(check-expect (image? a-blue-circle) #t)
(define a-blue-circle (circle 50 "solid" "blue"))

;; Activity 3
(check-expect (image? a-barbie-triangle) #t)
(define a-barbie-triangle (triangle 100 "solid" (make-color 218 24 132)))

;; Activity 4
(check-expect (image? outlined-square) #t)
(check-expect (image? outlined-circle) #t)
(check-expect (image? outlined-triangle) #t)
(define outlined-square (square 100 "outline" "red"))
(define outlined-circle (circle 50 "outline" "blue"))
(define outlined-triangle (triangle 100 "outline" (make-color 218 24 132)))

;; Activity 5
(check-expect (image? row-of-squares) #t)
(check-expect (image? column-of-squares) #t)
(check-expect (image? nested-squares) #t)
(define row-of-squares (beside (square 25 "solid" "red")
                               (square 25 "solid" "blue")
                               (square 25 "solid" "green")))
(define column-of-squares (above (square 25 "solid" "red")
                                 (square 25 "solid" "blue")
                                 (square 25 "solid" "green")))
(define nested-squares (overlay (square 25 "solid" "black")
                                (square 50 "solid" "green")
                                (square 75 "solid" "blue")
                                (square 100 "solid" "red")))

;; Activity 6
(check-expect (image? barbie-bowtie) #t)
(define barbie-bowtie (beside (rotate 270 (triangle 50 "solid" (make-color 218 24 132)))
                              (rotate 90 (triangle 50 "solid" (make-color 218 24 132)))))

;; Activity 7
(check-expect (image? flag-of-chicago) #t)
(define flag-of-chicago (underlay/offset (underlay/offset
                                          (underlay/offset
                                           (underlay/offset
                                            (underlay/offset
                                             (underlay/offset (rectangle 450 300 "outline" "black") 0 -75
                                                              (rectangle 450 50 "solid" "dodger blue")) 0 75
                                              (rectangle 450 50 "solid" "dodger blue")) -135 0
                                             (radial-star 6 12 30 "solid" "red")) -45 0
                                            (radial-star 6 12 30 "solid" "red")) 45 0
                                           (radial-star 6 12 30 "solid" "red")) 135 0
                                          (radial-star 6 12 30 "solid" "red")))