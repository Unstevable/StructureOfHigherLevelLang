#lang typed/racket
#|
    Author: Steven Carr
    Last Edited: November 2023
    Learning memory management, working with handles and given memory frames
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
(require "hw7-util.rkt")
(require racket/set)
(provide (all-defined-out))
;; END OF REQUIRES

;;;;;;;;;;;;;;;
;; Exercise 1
;
; Given a frame, return all handles contained therein.
;
; Use frame-values; only closure's env is relevant to this problem
; Read the hints in the PDF.
(: frame-refs (-> frame (Setof handle)))
(define (frame-refs frm)
  (define value-list (frame-values frm)) ; Get the list of values from the frame
  (define closure-list (filter d:closure? value-list)) ; From the values, get the list of closures from the frame
  (define handle-list (map d:closure-env closure-list)) ; From the closures, get the list of handles from the frame
  (define handle-set (list->set handle-list)) ; Put the handles into a set (list->set converts a list of something into a set of something)
  (define parent
    (frame-parent frm)) ; Easy variable to represent the parent frame
  (if parent
      (set-add handle-set parent) ; If the parent is true, then add the parent's handle to the set
      handle-set)) ; If the parent is false (void) just return the set of current handles from the frame

;;;;;;;;;;;;;;;
;; Exercise 2
; Standard graph algorithm: return all handles reacheable via `contained`.
; Hint: Consider solving this exercise last, as conceptually the most difficult
;       exercise.
; Hint: This is a simple breadth-first algorithm. The algorithm should start
;       in env and obtain the set of next elemens by using `contained`.
; Hint: The algorithm must handle cycles.
; Hint: Do not hardcode your solution to frames (you should test it with
;       frames though)
(: mem-mark
  (All [T]
    (->
      (-> T (Setof handle))
      (heap T)
      handle
      (Setof handle)
    )
  )
)
(define (mem-mark contained mem env)

  ; (: mem-mark-iter (-> (Listof handle) (Setof handle) (Setof handle)))
  ; One solution to this problem is to loop while maintaining
  ; a list of environments to visit, and a set of elements visited
  ; (define (mem-mark-iter to-visit visited)
  ;   ; while not empty(to-visit):
  ;   ;    1. pick one env from from to-visit and retrieve its "frame"
  ;   ;    2. let "c" be the set of env's contained in the given "frame"
  ;   ;    3. let the set "new" be every element that is in "c" but not in "visited" (use set-minus)
  ;   ;    4. add every element of "new" to the rest of elements to be visited
  ;   ;    5. update the set of visited elements to include the new elements
  ;   (todo "todo")
  ; )
  ; ; run the loop with 1 element to visit, and 1 element visited
  ; (mem-mark-iter (list env) (set env))


  (error "todo")
)

;;;;;;;;;;;;;;;
;; Exercise 3
;
; Return a new heap that only contains the key-values referenced in to-keep.
;
; Tip 1: We have learned a similar pattern than what is asked here.
; Tip 2: The function you want to use starts with heap-
; Tip 3: The solution is a one-liner
(: mem-sweep
  (All [T]
    (->
      ; heap
      (heap T)
      ; set of handles to keep
      (Setof handle)
      ; the new heap
      (heap T)
    )
  )
)
(define (mem-sweep mem to-keep)
  (heap-filter (lambda (handle _) (set-member? to-keep handle)) mem) ; Call heap-filter to see if the each handle of the memory is apart of the to-keep set
)

;;;;;;;;;;;;;;;
;; Exercise 4

(: eff-map
  (All [State Input Output]
    (->
      (-> Input Output)
      (Listof (eff-op State Input))
      (eff-op State (Listof Output))
    )
  )
)
(define (eff-map f l)
  (match l
    [(list) (eff-pure (list))] ; If the list is empty, return an empty list (use eff-pure for what we return)
    [(list h l ...)
     (eff-bind h
       (lambda ([h-value : Input]) ; Otherwise, bind h with it's value type of Input
         (eff-bind (eff-map f l) ; Bind the recursive call (like doing define result)
           (lambda ([result : (Listof Output)]) :  (eff-op State (Listof Output)) ; Make sure to establish it's type
             (eff-pure (cons (f h-value) result))))))] ; Return the cons of the function applied to the head of the list with the function applied to the rest
    )
)

;;;;;;;;;;;;;;;
;; Exercise 5

(: eff-exists?
  (All [State T]
    (->
      (-> T Boolean)
      (Listof (eff-op State T))
      (eff-op State Boolean)
    )
  )
)
(define (eff-exists? f l)
  (match l
    [(list) (eff-pure #f)] ; If the list is empty, then it doesn't exist; return false
    [(list h l ...)
     (eff-bind h
       (lambda ([h-value : T]) : (eff-op State Boolean) ; Otherwise bind h to it's type of T
         (if (f h-value) (eff-pure #t) ; If it exists, return true
             (eff-exists? f l))))])  ; Otherwise keep looking and make the recursive call
)

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
Soundness is affected by this situation.  If the reference count becomes 0 prematurely, then it begins
to garbage collect earlier than it is supposed to, which could lead to hanging references and potential
crashes.
|#
