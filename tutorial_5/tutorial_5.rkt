;; drop: (listof T) number -> (listof T)
;; removes the first k elements of the list

(define drop
  (λ (inp-list k)
    (if (= k 0)
        inp-list
        (drop (rest inp-list) (- k 1)))))

(check-expect (drop '() 0)
              '())

(check-expect (drop '("a" "b" "c") 0)
              '("a" "b" "c"))

(check-expect (drop '("a" "b" "c") 1)
              '("b" "c"))

(check-expect (drop '("a" "b" "c") 2)
              '("c"))

(check-expect (drop '("a" "b" "c") 3)
              '())

;; take: (listof T) number -> (listof T)
;; Return the first k elements of the list

(define take
  (λ (inp-list k)
    (if (= k 0)
        '()
        (cons (first inp-list)
              (take (rest inp-list) (- k 1))))))

(check-expect (take '() 0)
              '())
(check-expect (take '("a" "b" "c") 0)
              '())
(check-expect (take '("a" "b" "c") 1)
              '("a"))
(check-expect (take '("a" "b" "c") 2)
              '("a" "b"))
(check-expect (take '("a" "b" "c") 3)
              '("a" "b" "c"))

;;;
;;; TREE STUFF
;;;

;; A binary-tree is a number of (make-branch number binary-tree binary-tree
;; Note: binary trees get used for lots of things and not always for storing numbers.
;; So we really *ought* to call this something like binary-number-tree, but that's
;; too cumbersome to type.  So we'll just leave it here as binary-tree.
(define-struct branch (number left right))
(define test-tree
  (make-branch 1
               (make-branch 2
                            (make-branch 4 6 7)
                            5)
               (make-branch 3 8 9)))

;; count-tree: binary-tree -> number
;; Returns the number of numbers in the tree

(define count-tree
  (λ (inp-tree)
    (if (number? inp-tree)
        1
        (+ 1
           (count-tree (branch-left inp-tree))
           (count-tree (branch-right inp-tree))))))
        
(check-expect (count-tree 0)
              1)
(check-expect (count-tree (make-branch 1 0 0))
              3)
(check-expect (count-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              7)

;; sum-tree: binary-tree -> number
;; Returns the sum of all the numbers in the tree

(define sum-tree
  (λ (inp-tree)
    (if (number? inp-tree)
        inp-tree
        (+ (branch-number inp-tree)
           (sum-tree (branch-left inp-tree))
           (sum-tree (branch-right inp-tree))))))

(check-expect (sum-tree 12)
              12)
(check-expect (sum-tree (make-branch 1 2 3))
              6)
(check-expect (sum-tree (make-branch 1
                                       (make-branch 2 0 0)
                                       (make-branch 3 0 4)))
              10)
