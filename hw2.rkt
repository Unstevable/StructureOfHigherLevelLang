#lang racket
; Author: Steven Carr
; Last Edited: September 2023
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################
|#
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1
(struct pair (left right))  ;  Create a struct of a pair with a left hand side and right hand side


;; Exercise 1.a
(define (pair-set-left p l)
  (match p
    [(pair p-left p-right) (pair l p-right)] ; If it matches (is a proper pair), return a pair of the new left and current right
    )
)

;; Exercise 1.b
(define (pair-set-right p r)
  (match p
    [(pair p-left p-right) (pair p-left r)] ; If it matches (is a proper pair), return a pair of the current left and new right
    )
)

;; Exercise 1.c
(define (pair-swap p)
  (match p
    [(pair p-left p-right) (pair p-right p-left)] ; If it matches, swap the positions of the current left and right
    )
)

;; Exercise 1.d
;; You can only use match* one time. You cannot use match.
(define (pair-add p1 p2)
  (match* (p1 p2)
    [((pair p1-left p1-right) (pair p2-left p2-right)) ; If both inputs given are proper pairs
     (pair (+ p1-left p2-left) (+ p1-right p2-right)) ; Return a new pair of the additions of both lefts and both rights respectively
     ]
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.a
(define (name first last)
  (lambda (n)    ; Match a variable n (a name) as having a first and a last name, denoted as 'first and 'last
    (match n
      ['first first]
      ['last last]
      )
    )
  )

;; Exercise 2.b
(define (first-name p)
  (p 'first)  ; Return the 'first name of the input name
  )

;; Exercise 2.c
(define (last-name p)
  (p 'last)  ; Return the 'last name of the input name
  )

;; Exercise 2.d
(define (full-name p)
  (string-append (p 'first) " " (p 'last)) ; Append the first and the last name together with a space inbetween
  )

;; Exercise 2.e
(define (initials p)
  (string-append  ; Append the first letter of both the first and last name to get the initials
   (substring (p 'first) 0 1)  ; Substring from 0 and 1 returns the first letter of the first name
   (substring (p 'last) 0 1)  ; Same for last name
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For Exercise 5:
(define (gen f n l)
  (match l
    [(list) n]
    [(list h l ...)
     (define result (gen f n l))
     (f h result)
     ]
    )
  )
;; Exercise 3
(define (max-from n l)
  (gen max n l)
  )

; Original code:
; (define (max-from n l)
  ;(match l
    ;[(list) n]   ; If list is empty return n
    ;[(list h l ...)
     ; If the list isn't empty make a recursive call on itself
     ;(define result (max-from n l))
     ; Call max to now figure out the max value amongst the list and n
     ;(max h result)
    ; ]
   ; )
  ;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4
(define (min-from n l)
  (gen min n l)
  )

; Original Code
;(define (min-from n l)
  ;(match l
  ;  [(list) n]  ; If list is empty return n
  ;  [(list h l ...)
     ; If the list isn't empty make a recursive call on itself
  ;   (define result (min-from n l))
     ; Call min to calculate the minimum value amongst the list and n
  ;   (min h result)
  ;   ]
  ;  )
  ;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5: revisit Exercise 3 and Exercise 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6
(define (count l)
  (match l
    [(list) 0] ; If there is no list, it has 0 elements
    [(list h l ...)
     ; If there is, define a variable with a value of 1
     ; because there is at least 1 element in the list
     (define cnt 1)
     ; Make a recursive call on itself to iterate all elements
     (define result (count l))
     ; Starting at 1, add 1 for each element
     (+ cnt result)
     ]
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7
(define (sum l)
  (match l
    [(list) 0] ; If there is no list, return 0 (no numbers to add)
    [(list h l ...)
     ; Otherwise make the recursive call
     (define result (sum l))
     ; Add each element to the next
     (+ h result)
     ]
    )    
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8
(define (occurrences x l)
  (match l
    [(list) 0] ; If list is empty x never occurs
    [(list h l ...)
     ; Otherwise make the recursive call on the rest of the list
     (define result (occurrences x l))
     ; If x and the element are equal, add 1 to the total number of occurrences
     (cond [(= x h) (+ 1 result)]
           ; Else add 0 to the occurrences
           [else (+ result 0)]
           )
     ]
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9
(define (norm l)
  ; Define a variable x to hold the sum of the squares of all numbers in the list
  ; Recall the function from earlier in this assignment to sum all numbers in a list together
  (define x (sum
             ; Use map to multiply every number to itself in the list l
             (map (lambda (number1)
                    (* number1 number1)) l)
             )
    )
  ; After every square is added return the square root of x
  (sqrt x)
  )

