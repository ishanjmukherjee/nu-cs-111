(require "define_super_struct.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Part 1 - Fun at the Zoo

;;; Activity 1
;an animal is:
; - (make-animal string number number)

(define-struct animal (name age weight))


;;; Activity 2
;a cat is:
; - (make-cat string number number string) 

(define-struct (cat animal) (sleeping-spot))

;;; Activity 3
;a dog is:
; - (make-dog string number number string)

(define-struct (dog animal) (best-friend))

;;; Activity 4
;a mouse is:
; - (make-mouse string number number string) 

(define-struct (mouse animal) (hiding-spot))

;;; Some test animals for you!
(define figaro (make-cat "figaro" 12 15 "horseshoe"))
(define molly  (make-dog "molly" 12 2   "Emily"))
(define stella (make-dog "stella" 47 4 "Catherine"))
(define stitch (make-dog "stitch" 15 10 "Bob"))
(define mazie (make-dog  "mazie" 10 15 "Bob"))
(define ralph (make-mouse "ralph" 0.5 1 "under the fridge"))
(define zoo (list figaro molly stella stitch mazie ralph))

;;; Activity 5
; feed-animal!: animal -> void
; feeds an animal a 2 lb meal
; Effect: the animal is 2lbs heavier
(define (feed-animal! anim)
  (set-animal-weight! anim (+ (animal-weight anim) 2)))

; Make sure to write some tests!

(check-expect (local [(define dario (make-cat "dario" 2 10 "stable"))]
                (begin (feed-animal! dario)
                       dario))
              (make-cat "dario" 2 12 "stable"))

(check-expect (local [(define sam (make-animal "sam" 10 -7))]
                (begin (feed-animal! sam)
                       sam))
              (make-animal "sam" 10 -5))

;;; Activity 6
; feed-animals-with-favoritism!: list-of-animals -> void
; Feeds all animals in a list...but doubles up
; feeding for any animals with best-friend equal to "Bob"
; Effect: Each animal in the zoo is 2 pound heavier, but
; dogs who list Bob as their best friend get fed twice. 
(define (feed-animals-with-favoritism! loa)
  (for-each (λ (anim)
              (begin (set-animal-weight! anim
                                         (+ (animal-weight anim) 2))
                     (when (dog? anim)
                       (when (string=? (dog-best-friend anim) "Bob")
                         (set-animal-weight! anim
                                             (+ (animal-weight anim) 2))))))
            loa))

; These tests are a little more complicated...so make sure
; you understand how they work!
(define figaro-weight-before-test (animal-weight figaro))
(define stitch-weight-before-test (animal-weight stitch))
(define mazie-weight-before-test (animal-weight mazie))
(define stella-weight-before-test (animal-weight stella))

(check-expect (begin (feed-animals-with-favoritism! zoo)
                     (animal-weight figaro))
              (+ 2 figaro-weight-before-test))
(check-expect (animal-weight stitch)
              (+ 4 stitch-weight-before-test))
(check-expect (animal-weight mazie)
              (+ 4 mazie-weight-before-test))
(check-expect (animal-weight stella)
              (+ 2 stella-weight-before-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Part 2 - Course Roster


;; Activities 7 and 8 will be completed somewhere down here... it's up to
;; you to determine where they need to go!

; a student is a
; (make-student string number string
(define-struct student (name grad-year major))

; a roster is a
; (make-roster string string (listof student))
(define-struct roster (course-name instructor students)
  #:methods
  ; display: roster -> void
  ; to display the values in the attributes of the roster
  ; NOTE: MORE ABOUT PRINTF and HOW IT WORKS AT THE BOTTOM
  (define (display r)
    (begin (printf "~a~n" (roster-course-name r))
           (printf "instructor: ~a~n" (roster-instructor r))
           (local [(define student-num 1)]
             (for-each (λ (s)       
                         (begin (printf "~a. student: ~a~n"
                                        student-num
                                        (student-name s))
                                (set! student-num (+ 1 student-num))))
                       (roster-students r)))))
  (define (search-by-major r maj)
    (filter (λ (stud)
              (string=? (student-major stud) maj))
            (roster-students r)))
  (define (names-by-grad-year r y)
    (map (λ (stud) (student-name stud))
         (filter (λ (stud)
                   (= (student-grad-year stud) y))
                 (roster-students r)))))

; A sample roster for our tests to use
(define cs111 (make-roster "Fundamentals of Computer Programming"
                           "Connor Bain"
                           (list (make-student "yanning" 2024 "Computer Science")
                                 (make-student "ava" 2025 "Political Science")
                                 (make-student "paula" 2024 "Computer Science")
                                 (make-student "david" 2023 "Journalism")
                                 (make-student "liza" 2023 "Math")
                                 (make-student "jose" 2023 "Computer Science"))))

; Try out the display method of roster
(display cs111)

; Test 1 to see if search-by-major works
(check-expect (search-by-major cs111 "Computer Science")
              (list (make-student "yanning" 2024 "Computer Science")
                    (make-student "paula" 2024 "Computer Science")
                    (make-student "jose" 2023 "Computer Science")))

; Test 2 to see if search-by-major works
(check-expect (search-by-major cs111 "Data Science")
              (list))

; Test 3 to see if search-by-major works
(check-expect (search-by-major cs111 "Math")
              (list (make-student "liza" 2023 "Math")))

; Test 1 to see if names-by-grad-year works
(check-expect (names-by-grad-year cs111 2023)
              (list "david" "liza" "jose"))

; Test 2 to see if names-by-grad-year works
(check-expect (names-by-grad-year cs111 2026)
              (list))


; ; printf basics
; -------------
; 
; printf (which stands for print formatted)
; can be used to print (or produce) a string in the REPL (the bottom window),
; with arguments that are formatted as strings.
; 
; For example:
; 
; > (printf "number ~a" 1)
; number 1
; 
; Here, the ~a tells printf, whatever comes after the string, substitute that for ~a, which
; is the number 1 in this case
; 
; You can use multiple ~a's, as such:
; 
; > (printf "numbers ~a, ~a, and ~a!" 1 2 3)
; numbers 1, 2, and 3!
; 
; ~n tells printf to create a new line (i.e. a line break):
; 
; > (printf "~n numbers")
; 
;  numbers
