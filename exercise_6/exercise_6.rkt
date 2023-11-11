(require "./snake_lib.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-food-to-game: game posn -> game
; Given a game and posn, returns a new game (so you want to call make-game here)
; where food has been added at that posn. 
(define (add-food-to-game g p)
 (make-game (game-snake g)
            (cons p (game-food g))
            (game-obstacles g)
            (game-ticks g)))

(check-expect
 (add-food-to-game (make-game (make-snake 'up (list (make-posn 1 2)))
                      (list (make-posn 3 4))
                      (list (make-posn 10 10)
                            (make-posn 20 20))
                      5)
           (make-posn 6 7))
 (make-game (make-snake 'up (list (make-posn 1 2)))
            (list (make-posn 6 7) (make-posn 3 4))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            5))

; change-snake-direction: game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-snake-direction g d)
  (make-game (make-snake d (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

(check-expect
 (change-snake-direction
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'left)
 (make-game (make-snake 'left (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))

(check-expect
 (change-snake-direction
  (make-game (make-snake 'up (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5)
  'up)
 (make-game (make-snake 'up (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (- (* (length (snake-segments (game-snake g))) 100) (game-ticks g)))

; my test

(check-expect
 (game-score
  (make-game (make-snake 'down (list (make-posn 1 2)))
             (list (make-posn 3 4))
             empty
             5))
 (- (* 1 100) 5))

; no tests are provided for game-score because it is open-ended
; feel free to implement it however you would like to

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!
(define (game-over? g)
  (if (or (swallow? g)
          (wall-collision? g)
          (obstacle-collision? g))
      true
      false))

(define (swallow? g)
  (if (and (> (length (snake-segments (game-snake g))) 2)
           (member (first (snake-segments (game-snake g)))
                   (rest (snake-segments (game-snake g)))))
      true
      false))

(define (wall-collision? g)
  (if (or (< (apply min (map (λ (segment) (posn-x segment))
                             (snake-segments (game-snake g))))
             1)
          (< (apply min (map (λ (segment) (posn-y segment))
                             (snake-segments (game-snake g))))
             1)
          (> (apply max (map (λ (segment) (posn-x segment))
                             (snake-segments (game-snake g))))
             50)
          (> (apply max (map (λ (segment) (posn-y segment))
                             (snake-segments (game-snake g))))
             50))
      true
      false))

(define (obstacle-collision? g)
  (if (empty? (snake-segments (game-snake g)))
      false
      (if (member (first (snake-segments (game-snake g)))
                  (game-obstacles g))
          true
          (obstacle-collision?
           (make-game (make-snake (snake-heading (game-snake g))
                                  (rest (snake-segments (game-snake g))))
                      (game-food g)
                      (game-obstacles g)
                      (game-ticks g))))))
      
; wall collision
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn -1 1))) empty empty 5))
 true)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 0 1))) empty empty 5))
 true)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 0))) empty empty 5))
 true)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 0 0))) empty empty 5))
 true)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 50 50))) empty empty 5))
 false)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 51 25))) empty empty 5))
 true)
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 50 1))) empty empty 5))
 false)

; swallow
(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1)
                                              (make-posn 1 1)))
                        empty
                        empty
                        5))
 false)

(check-expect 
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1)
                                              (make-posn 1 1)
                                              (make-posn 2 1)))
                        empty
                        empty
                        5))
 true)

; obstacle collision
(check-expect
 (game-over? (make-game (make-snake 'up (list (make-posn 1 1)
                                              (make-posn 2 1)
                                              (make-posn 3 1)))
                        empty
                        (list (make-posn 2 1))
                        5))
 true)

; game-advance: game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (game-advance g)
  (if (member (new-head (game-snake g)) (game-food g))
      (make-game (make-snake (snake-heading (game-snake g))
                             (cons (new-head (game-snake g))
                                   (snake-segments (game-snake g))))
                 (remove-all (new-head (game-snake g))
                             (game-food g))
                 (game-obstacles g)
                 (+ (game-ticks g) 1))
      (make-game (make-snake (snake-heading (game-snake g))
                             (cons (new-head (game-snake g))
                                   (reverse (rest
                                             (reverse
                                              (snake-segments
                                               (game-snake g)))))))
                 (game-food g)
                 (game-obstacles g)
                 (+ (game-ticks g) 1))))

; snake -> posn
(define (new-head s)
  (cond [(symbol=? (snake-heading s) 'up)
         (make-posn (posn-x (first (snake-segments s)))
                    (+ (posn-y (first (snake-segments s))) 1))]
        [(symbol=? (snake-heading s) 'down)
         (make-posn (posn-x (first (snake-segments s)))
                    (- (posn-y (first (snake-segments s))) 1))]
        [(symbol=? (snake-heading s) 'right)
         (make-posn (+ (posn-x (first (snake-segments s))) 1)
                    (posn-y (first (snake-segments s))))]
        [(symbol=? (snake-heading s) 'left)
         (make-posn (- (posn-x (first (snake-segments s))) 1)
                    (posn-y (first (snake-segments s))))]))

; new-head tests
(check-expect (new-head (make-snake 'up (list (make-posn 1 1)
                                              (make-posn 1 2)
                                              (make-posn 1 3))))
              (make-posn 1 2))

(check-expect (new-head (make-snake 'right (list (make-posn -1 5)
                                                 (make-posn 0 8)
                                                 (make-posn 54 103))))
              (make-posn 0 5))

(check-expect (new-head (make-snake 'left (list (make-posn -1 5)
                                                (make-posn 0 8)
                                                (make-posn 54 103))))
              (make-posn -2 5))

; game-advance tests
(check-expect
 (game-advance
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             empty
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)))
            empty
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))
(check-expect
 (game-advance
  (make-game (make-snake 'down (list (make-posn 2 2)
                                     (make-posn 2 3)
                                     (make-posn 3 3)))
             (list (make-posn 2 1) (make-posn 8 9))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             5))
 (make-game (make-snake 'down (list (make-posn 2 1)
                                    (make-posn 2 2)
                                    (make-posn 2 3)
                                    (make-posn 3 3)))
            (list (make-posn 8 9))
            (list (make-posn 10 10)
                  (make-posn 20 20))
            6))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             (list (make-posn 10 10)
                   (make-posn 20 20))
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game game-advance add-food-to-game change-snake-direction game-score game-over?))

;to start a game
(play game-start)
