#lang errortrace typed/racket
; Author: Steven Carr
; Last Edited: October 2023
; Implement infinite and finite streams as sets
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require "hw4-util.rkt")
(provide (all-defined-out))

(: stream-skip
  (All [Elem] ; Parameterized on the type of the elements of the stream
    (->
      ; The first parameter is the number of elements we wish to skip
      Real
      ; The input is a stream of elements
      (stream Elem)
      ; The output is a stream of elements
      (stream Elem)
    )
  )
)

; Stream-skip to skip the first n number of elements in a stream
(define (stream-skip n s)
  ; If 0 < n, match the stream s
  (if (< 0 n)
      (match (s)
        ; If it's a proper stream, recursively skip and decrement n until it reaches 0
        ; When it's 0, we return s and we have now skipped n number of elements
        [(stream-add x s)
         (stream-skip (- n 1) s)])
      ; Return the rest of the stream
      s)
  )

(: stream-fold
  ; We have 2 type parameters,
  ; 1. the type of elements of the stream
  ; 2. the type of the result being accumulated
  (All [Elem Accum]
    (->
      ; The type of the step function f
      (-> Elem Accum Accum)
      ; The type of the value being accumulated
      Accum
      ; The input stream of elements
      (stream Elem)
      ; The output stream of folded elements
      (stream Accum)
    )
  )
)

; Stream-fold is an accumulator for an infinite stream
(define (stream-fold f a s)
  (thunk
    (match (s)
      [(stream-add x s)
       (define a1 (f x a))
       (define result (stream-fold f a1 s))
       (stream-add a result)]
      )
    )
)

; Set-void returns an empty set (void with literally nothing in it)
(: set-void set)
(define set-void
  (thunk
   (set-empty)
   )
)

; Set-epislon returns a set with nothing in it besides an empty space
(: set-epsilon set)
(define set-epsilon
  (thunk
   (set-add "" set-empty)
   )
)

; Set-char returns a set with the single given character
(: set-char (-> Char set))
(define (set-char x)
  (thunk
   (set-add (string x) set-empty)
   )
)

; Set-prefix prepends every element of the set with the given prefix
(: set-prefix (-> String set set))
(define (set-prefix s p)
  (thunk
   (match (p)
     ; If the set is empty, return the empty set
     [(set-empty) (set-empty)]
     ; Otherwise, make the recursive call on the rest of the set with the prefix
     [(set-add h l)
      (define result (set-prefix s l))
      ; String-append the prefix to each element
      (set-add (string-append s h) result)]
     )
   )
)

; Set-union works like interleave in hw3, except with sets
(: set-union (-> set set set ))
(define (set-union p1 p2)
  (thunk
   (match (p1)
     ; If the first set is empty, return the second set
     [(set-empty) (p2)]
     ; Otherwise, make the recursive call with the reverse'd order of arguments, with p1 now being the rest of the set of p1
     [(set-add h1 l1)
      (define result (set-union p2 l1))
      ; Interleave each element of both sets by adding the current element and switching to the other set
      (set-add h1 result)]
     )
   )
)

; Set-concat concatenates every pair of string-elements from both sets
; i.e. set1-> "a" "b" "c"|||| set2-> "d" "e" "f"|||| new set->"ad" "be" "cf"
(: set-concat (-> set set set))
(define (set-concat p1 p2)
  (thunk
   (match (p1)
     ; If p1 is an empty set, return an empty set
     [(set-empty) (set-empty)]
     ; Otherwise, define the result as the union of set-prefix of the current element of p1, and the
     ; recursive call for contatenating the rest of p1 to p2
     [(set-add h1 l1)
      (define result (set-union (set-prefix h1 p2) (set-concat l1 p2)))
      ; Return the result
      (result)]
     )
   )
  )

; r:eval-exp evaluates any expression with any number of args
; using apply and map to solve
(: r:eval-exp (-> r:expression Number))
(define (r:eval-exp exp)
  (match exp
    ; If it's a number, return that number
    [(r:number v) v]
    ; If it's a function with multiple arguments, use apply and map to solve
    [(r:apply (r:variable f) args)
      (define func (r:eval-builtin f))
      (apply func (map r:eval-exp args))
    ]
  )
)

; r:-exp-to-string converts the given expression into a string
(: r:exp-to-string (-> r:expression String))
(define (r:exp-to-string exp)
  ; Pattern match the expression
  (match exp
    ; If it's a number, return the string format for the number
    [(r:number v) (format "~a" v)]
    ; Same for if it's a variable
    [(r:variable f) (format "~a" f)]
    ; If it's an apply, I add the function and the arguments together to form one list
    [(r:apply f args)
     (define total (cons f args))
     ; With this one list, I map the recursive call to every element
     (define result (map r:exp-to-string total))
     ; I use string-join to make one string and then string-append parenthesis to it
     (define final-string (string-join result))
     (string-append "(" final-string ")")]
    )
)