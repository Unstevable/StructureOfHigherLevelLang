#lang racket
; Author: Steven Carr
; Last Edited: September 2023
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

  We ask that solutions be distributed only locally -- on paper, on a
  password-protected webpage, etc.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

                    * * * ATTENTION! * * *

  Every solution submitted to our grading server is automatically compared
  against a solution database for plagiarism, which includes every solution
  from every student in past semesters.

  WE FOLLOW A ZERO-TOLERANCE POLICY: any student breaking the Code of Student
  Conduct will get an F in this course and will be reported according to
  Section II Academic Dishonesty Procedures.

|#

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

; Original expression: (6 divded by (4 plus 12)) plus (8 divided by (6 plus 4))
(define ex1
  (+ (/ 6 (+ 4 12))
     (/ 8 (+ 6 4))
   )
)


(define ex2
  (list
     (+ (/ 6 (+ 4 12))
        (/ 8 (+ 6 4))
        ) ; Expression in non-simplified form

     (+ (/ 6 16)
        (/ 8 (+ 6 4))
        ) ; Step 1: Evaluate the right expression within the left-outer expression

     (+ 3/8
        (/ 8 (+ 6 4))
        ) ; Step 2: Evaluate the left-outer expression (left to right, top to bottom)

     (+ 3/8
        (/ 8 10)
        ) ; Step 3: Evaluate the right expression within the right-outer expression

     (+ 3/8
        4/5
        ) ; Step 4: Evaluate the right-outer expression

     47/40 ; Step 5: Solve the final overall expression
  )
)


(define (ex3 x y) ; function parameters x and y
  (> (- (* 3 15) x) (- (- y 14) (+ 10 x))
     )
)

;; Constructs a tree from two trees and a value
(define (tree left value right)
  (list left value right)       ; Make a list of the parameters
)

;; Constructs a tree with a single node
(define (tree-leaf value)
  (list null value null)
)     ; Make a list with only the value

;; Accessors
(define (tree-left self)
  (first self)     ; First thing in the list
)

(define (tree-value self)
  (second self)     ; Second thing in the list
)

(define (tree-right self)
  (third self)     ; Third thing in the list
)

;; Copies the source and updates one of the fields
(define (tree-set-value self value)
  (define old-left (tree-left self)) ; Define old-left as the current left, unchanged
  (define old-right (tree-right self)) ; Same for old-right, unchanged

  (list old-left value old-right) ; Make a new list (tree) keeping the old left/right but changing the value
)
; Repeat that process but change the left
(define (tree-set-left self left)
  (define old-value (tree-value self))
  (define old-right (tree-right self))

  (list left old-value old-right)
)
; Repeat that process but change the rights
(define (tree-set-right self right)
  (define old-left (tree-left self))
  (define old-value (tree-value self))

  (list old-left old-value right)
)

;; Function that inserts a value in a BST
(define (bst-insert self value)
  (cond [(equal? self null) (tree-leaf value)] ; If the node is null, construct a tree with one node
        [(equal? value (tree-value self)) (tree-set-value self value)] ; If the new value is equal to the node value, set the value using the new value 
        [(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))] ; If the new value is less than the node value, set the left tree of the node to the new value (recursive call)
        [else (tree-set-right self (bst-insert (tree-right self) value))] ; Otherwise make the recursive call for the right tree of the node
  )
)

;; lambda
(define (lambda? node)   ; Lambda length is AT LEAST 3 in length (or more)
  (and (list? node)
       (>= (length node) 3)
       (equal? (first node) 'lambda)
       (list? (second node))
       (andmap symbol? (second node))
   )
)

(define (lambda-params node)
  (second node)  ; The parameters of lambda are the second spot in the quoted list
)

(define (lambda-body node)
  (rest (rest node)) ; The body of lambda is the rest of the list after the parameters; rest gives everything after first,
                     ; rest of rest returns everything after second (rest of rest of rest would be everything after third)
)

;; apply
(define (apply? l)
  (and (list? l) ; Check to see if the quoted application is a list
       (>= (length l) 1) ; Needs at least 1 item in the list (the function call)
   )
)

(define (apply-func node)
  (first node) ; Function being called will be the first term
)

(define (apply-args node)
  (rest node) ; The arguments to the function will be the rest of the terms after the function call
)

;; define
(define (define? node)  ; Would be a define if it fits either the define-basic OR the define-func
  (or (define-basic? node)
       (define-func? node)
   )
)

(define (define-basic? node)
  (and (list? node) ; Need to check if its a list first to avoid error
       (equal? (length node) 3)  ; Needs to check if its a non-empty list to avoid error; a basic definition should only be 3 in length
       (equal? (first node) 'define) ; First symbol should be define
       (symbol? (second node)) ; Second item in the list should be a symbol
   )
)

(define (define-func? node) ; Also a list; needs AT LEAST 3 in length (could have more); second should be list of symbols
  (and (list? node)
       (>= (length node) 3)
       (equal? (first node) 'define)
       (list? (second node))
       (>= (length (second node)) 1)
       (andmap symbol? (second node))
   )
)
