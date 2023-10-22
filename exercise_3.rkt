;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./remove_duplicates.rkt")

; This defines the basic album datatype.
(define-struct album (title artist genre))
; define-struct automatically creates the following functions for you:
;
; `make-<struct-name>` (in this case `make-album`)
;   a function to create an instance of the struct
;   this function takes arguments for each of the fields listed, so for example
;   (make-struct 'Sway' 'Tove Styrke' 'Pop') will create an album struct
;    with title 'Sway', artist 'Tove Styrke' & genre 'Pop
;
; `<struct-name>-<field-name>` (for each field)
;    functions for accessing values of each field in the struct
;    for album this would mean we'd have the following functions:
;    `album-title`, `album-artist`, `album-genre`
;    the following examples creates an album and then accesses its fields
;    ```
;    (define sway (make-album 'Sway' 'Tove Styrke' 'Pop')
;    (album-title sway) ; returns 'Sway'
;    (album-artist sway) ; returns 'Tove Styrke'
;    (album-genre sway) ; returns 'Pop'
;    ```
;
; `<struct-name>?` (in this case `album?`)
;   a predicate (function which returns a boolean) that checks a value and
;   returns true if it's an instance of the struct, false otherwise
;   using the `sway` album defined in the previous example
;   ```
;   (album? sway) ; returns true
;   (album? 1) ; returns false
;   (album? 'hi') ; returns false
;   ```

;;; Enter a list of albums below
;;; They need not be the actual albums you own.
;;; But you should include enough variety to adequately
;;; test your code.
;;;
;;; Here's what we mean. One of the questions involves
;;; writing a function that finds all the albums of a
;;; given genre.  If all the albums in the library are
;;; in the rock genre, then there's only one genre and
;;; when you ask for all the rock albums and it gives
;;; back all the albums, you don't know whether that's
;;; because the code really works, or because it's
;;; not even paying attention to the genre.  So you want
;;; to make sure there are multiple artists and genres,
;;; some artists with only one album or genre, others
;;; with multiple artists or genres, etc.

(define testing-library-1
  ;; Fill in the info below
  (list (make-album "Discovery" "Daft Punk" "Disco")
        (make-album "Human After All" "Daft Punk" "Rock")
        (make-album "You Want It Darker" "Leonard Cohen" "Rock")
        (make-album "Bad Girls" "Donna Summer" "Disco")
        (make-album "Scorpion" "Drake" "Pop")
        (make-album "Scorpion" "Eve" "Hip Hop")
        (make-album "Various Positions" "Leonard Cohen" "Rock")
        (make-album "John Wesley Harding" "Bob Dylan" "Country")
        (make-album "Tempest" "Bob Dylan" "Folk")
        (make-album "Bringing It All Back Home" "Bob Dylan" "Rock")
        (make-album "Blonde on Blonde" "Bob Dylan" "Rock")
        ; Now delete the example above
        ; and include your own albums below here
        
        ))

;;; Add the functions you write (e.g. all-genres, versatile-artists)
;;; below.  Be sure to test your functions to make sure they work.
;;; We only provide very few test cases this time, so you need
;;; to write your own test cases to make sure the code works.
;;; We will use our own test cases when grading and assign you
;;; a grade based on the number of test cases that passed.


;; all-titles : (listof album) -> (listof string)
(define all-titles (lambda (lib)
                     (map album-title lib)))

(check-expect (all-titles testing-library-1)
              (list
               "Discovery"
               "Human After All"
               "You Want It Darker"
               "Bad Girls"
               "Scorpion"
               "Scorpion"
               "Various Positions"
               "John Wesley Harding"
               "Tempest"
               "Bringing It All Back Home"
               "Blonde on Blonde"))


;; all-artists: (listof album) -> (listof string)
(define all-artists (lambda (lib)
                      (remove-duplicates (map album-artist lib))))

(check-expect (all-artists testing-library-1)
              (list "Daft Punk" "Leonard Cohen" "Donna Summer" "Drake" "Eve" "Bob Dylan"))

;; all-genres: (listof album) -> (listof string)
(define all-genres (lambda (lib)
                     (remove-duplicates (map album-genre lib))))

(check-expect (all-genres testing-library-1)
              (list "Disco" "Rock" "Pop" "Hip Hop" "Country" "Folk"))

;; artist-albums : string, (listof album) -> (listof album)
(define artist-albums
  (lambda (desired-artist lib)
    (filter (lambda (album) (string=?
                             (album-artist album)
                             desired-artist))
            lib)))

(check-expect (artist-albums "Leonard Cohen" testing-library-1)
              (list
               (make-album "You Want It Darker" "Leonard Cohen" "Rock")
               (make-album "Various Positions" "Leonard Cohen" "Rock")))

(check-expect (artist-albums "Cohen, Leonard" testing-library-1)
              '())

;; artist-genres: string, (listof album) -> (listof string)
(define artist-genres
  (lambda (desired-artist lib)
    (remove-duplicates (map album-genre
         (artist-albums desired-artist lib)))))

(check-expect (artist-genres "Leonard Cohen" testing-library-1)
              (list "Rock"))

(check-expect (artist-genres "Bob Dylan" testing-library-1)
              (list "Country" "Folk" "Rock"))

;; artist-is-versatile?: string, (listof album) -> boolean
(define artist-is-versatile?
  (lambda (desired-artist lib)
    (if (> (length (artist-genres desired-artist lib)) 1) 
        true
        false)))

(check-expect (artist-is-versatile? "Bob Dylan" testing-library-1)
              true)

(check-expect (artist-is-versatile? "Santa Claus" testing-library-1)
              false)

(check-expect (artist-is-versatile? "Drake" testing-library-1)
              false)

;; versatile-artists: (listof album) -> (listof string)
(define versatile-artists
  (lambda (lib)
    (filter (lambda (artist) (artist-is-versatile? artist lib))
            (all-artists lib))))

(check-expect (versatile-artists testing-library-1)
              (list "Daft Punk" "Bob Dylan"))

;; artist-album-counts: (listof album) -> (listof (list string number))
(define artist-album-counts
  (lambda (lib)
    (map
     (lambda (artist) (list artist (length (artist-a
                                            lbums artist lib))))
     (all-artists lib))))

(check-expect (artist-album-counts testing-library-1)
              (list
               (list "Daft Punk" 2)
               (list "Leonard Cohen" 2)
               (list "Donna Summer" 1)
               (list "Drake" 1)
               (list "Eve" 1)
               (list "Bob Dylan" 4)))


;; genre-album-counts: (listof album) -> (listof (list string number))
(define genre-albums
  (lambda (desired-genre lib)
    (filter (lambda (album) (string=?
                             (album-genre album)
                             desired-genre))
            lib)))

(check-expect (genre-albums "Rock" testing-library-1)
              (list
               (make-album "Human After All" "Daft Punk" "Rock")
               (make-album "You Want It Darker" "Leonard Cohen" "Rock")
               (make-album "Various Positions" "Leonard Cohen" "Rock")
               (make-album "Bringing It All Back Home" "Bob Dylan" "Rock")
               (make-album "Blonde on Blonde" "Bob Dylan" "Rock")))

(define genre-album-counts
  (lambda (lib)
    (map
     (lambda (genre) (list genre (length (genre-albums genre lib))))
     (all-genres lib))))

(check-expect (genre-album-counts testing-library-1)
              (list
               (list "Disco" 2)
               (list "Rock" 5)
               (list "Pop" 1)
               (list "Hip Hop" 1)
               (list "Country" 1)
               (list "Folk" 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Below are two example album libraries and a few example tests
;;; for SOME functions.
;;;
;;; ALL functions in this assignment should work for ANY given
;;; library, and the test cases below illustrate how libraries
;;; are passed to album functions.
;;;
;;; These test cases are far from complete. Remember to write new
;;; album library and tests to see if your solution works as expected.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define example-testing-library-1
  (list (make-album "You're Stronger Than You Know" "James Morrison" "Pop")
        (make-album "Bootleg"                       "Kenshi Yonezu"  "J-Pop")))

(define example-testing-library-2
  (list (make-album "Arthur Rubinstein Collection" "Arthur Rubinstein" "Classic")
        (make-album "Scott Joplin Piano Rags"      "Joshua Rifkin"     "Rags")
        (make-album "The Violin Sonatas (4 CDs)"   "Lev Oborin"        "Classic")))

(define prof-bains-music-library
  (list (make-album "Midnights"   "Taylor Swift" "Synth-pop")
        (make-album "1989"        "Taylor Swift" "Synth-pop")
        (make-album "Red"         "Taylor Swift" "Country")
        (make-album "Speak Now"   "Taylor Swift" "Country pop")))

(check-expect (all-titles example-testing-library-1)
              (list "You're Stronger Than You Know"
                    "Bootleg"))

(check-expect (all-titles example-testing-library-2)
              (list "Arthur Rubinstein Collection"
                    "Scott Joplin Piano Rags"
                    "The Violin Sonatas (4 CDs)"))

(check-expect (all-genres example-testing-library-2)
              (list "Classic" "Rags"))

(check-expect (artist-albums "Scott Joplin" example-testing-library-2)
              (list))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-2)
              (list (make-album "Scott Joplin Piano Rags" "Joshua Rifkin" "Rags")))

(check-expect (artist-albums "Joshua Rifkin" example-testing-library-1)
              (list))

(check-expect (artist-albums "Kenshi Yonezu" example-testing-library-1)
              (list (make-album "Bootleg" "Kenshi Yonezu" "J-Pop")))

(check-expect (artist-is-versatile? "James Morrison" example-testing-library-1)
              #false)

(check-expect (artist-album-counts example-testing-library-1)
              (list (list "James Morrison" 1)
                    (list "Kenshi Yonezu" 1)))


;; NOTE: Remember that "function" and "procedure" are the same thing! Racket's predicate
;; for seeing if something is a function is called procedure?
(check-expect (procedure? all-titles) #true)
(check-expect (procedure? all-artists) #true)
(check-expect (procedure? all-genres) #true)
(check-expect (procedure? artist-albums) #true)
(check-expect (procedure? artist-genres) #true)
(check-expect (procedure? artist-is-versatile?) #true)
(check-expect (procedure? versatile-artists) #true)
(check-expect (procedure? artist-album-counts) #true)
(check-expect (procedure? genre-album-counts) #true)
