#lang racket
; Author: Steven Carr
; Last Edited: October 2023
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1
(define (min-from n l)
  (foldr
   ;step, want to determine which is the minimum between all elements of the list
   (lambda (h result)
     (min h result)
     )
   ;base-case, return n if list is empty
   n
   ;list to loop
   l
   )
  )

;; Exercise 2
(define (count l)
  (foldr
   ;step - add 1 for each element of the list
   (lambda (h result)
     (+ 1 result)
     )
   ;base-case, if there are no elements, the count is 0
   0
   ;list to traverse
   l
    )
  )

;; Exercise 3
(define (sum l)
  (foldr
   ;step - add each element to the previous result
   (lambda (h result)
     (+ h result)
     )
   ;base-case, if there are no elements, the sum is 0
   0
   ;list to traverse
   l
   )
  )

;; Exercise 4
(define (occurrences n l)
  (foldr
   ;step - if n equals the current element in the list, add 1 to result, otherwise add 0
   (lambda (h result)
     (cond [(= n h) (+ 1 result)]
           [else (+ result 0)]
           )
     )
   ;base-case, if n doesn't equal any element in the list, then occurrences is 0
   0
   ;list to traverse
   l
   )
  )

;; Exercise 5
(define (prefix s l)
  ; If the given list is empty, return an empty list
  (cond [(equal? l (list)) (list)]
        ; Otherwise, use map to string-append prefix s onto every element of the list
        [else (map (lambda (x)
                     (string-append s x)
                     )
                   l
                   )
              ]
        )
  )

;; Exercise 6
(define (interleave l1 l2)
  (match l1
    ; If l1 is an empty list, return l2
    [(list)
      l2]
    ; Otherwise, if l1 is a list with multiple elements
    [(list h1 l1 ...)
     ; Make the recursive call but flip the order
     (define result (interleave l2 l1))
     ; Cons the current head with the result
     (cons h1 result)
     ]
    )
)

;; Exercise 7
(define (intersperse l v)
  (match l
    ; If the list is empty, return an empty list
    [(list) (list)]
    ; Otherwise, cons the result of cons-ing 0 to the next element of the list, to the first element of the list
    [(list h l ...)
     (cons h (foldr
      (lambda (h result)
        (cons v (cons h result)))
      ; Base-case
      (list)
      ; List to traverse
      l
      ))]
    )
  )

;; Exercise 8
(define (parse-ast node)
  ; make-define-func
  (define (make-define-func node)
    (r:define (parse-ast (first (second node))) ; Return an r:define where the first space is the first of the second node (first would be the define)
              (r:lambda (map parse-ast (rest (second node))) ; Second space is a r:lambda that maps parse-ast to the rest of the second node and the rest of the node
                        (map parse-ast (rest (rest node))))
              )
    )
  ; make-define-basic
  (define (make-define-basic node)
    (r:define (parse-ast (second node)) (parse-ast (third node))) ; Return a r:Define that does parse-ast on the second and third node
    )
  ;make-lambda:
  (define (make-lambda node)   
    (define func (map parse-ast (second node)))   
    (define args (map parse-ast (rest (rest node))))    
    (r:lambda func args)
    )
  ; make-apply
  (define (make-apply node)
    (define func (parse-ast (first node)))
    (define args (map parse-ast (rest node)))
    (r:apply func args)) ; Return an r:apply that returns parse-ast of the first node (function) and then on the rest of the nodes (arguments)
  
  (define (make-number node) (r:number node)) ; return the r:number of the node
  (define (make-variable node) (r:variable node)) ; return the r:variable of the node

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
