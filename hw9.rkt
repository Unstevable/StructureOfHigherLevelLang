#lang typed/racket
#|
    Author: Steven Carr
    Last Edited: December 2023
    Translated a SimpleJS function into LambdaJS
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require "hw9-util.rkt")

(provide (all-defined-out))

(: translate-var (-> s:variable j:variable))
(define (translate-var x)
  (match x
    [(s:variable x) (j:variable x)]
  )
)

;;;;;;;;;;;;;;;
;; Exercise 1
(: translate  (-> s:expression j:expression))
(define (translate exp)
  (match exp
    [(? k:const? k) k]
    [(? s:variable? x) (translate-var x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    [(s:load x y) (j:get (j:deref (translate-var x)) (mk-field y))] ; first line
    [(s:assign x y e) ; -----------------------------------------------------------------second line
       (mk-let (translate e) (lambda (data)                                           ; |
         (mk-let (j:deref (translate-var x)) (lambda (o)                              ; |
           (j:seq (j:assign (translate-var x) (j:set o (mk-field y) data)) data)))))] ;_|
    [(s:invoke x y e) ;-------------------------------------------------------third line
       (mk-let (j:get (j:deref (translate-var x)) (mk-field y)) (lambda (m) ; |
         (mk-let (j:get (j:deref m) (k:string "$code")) (lambda (f)         ; |
           (j:apply f (cons (translate-var x) (map translate e)))))))]      ;_|
    [(s:function x e) ;---------------------------------------------------------------------- fourth line
     (define green (j:lambda (cons (j:variable 'this) (map translate-var x)) (translate e))) ; |
     (mk-object (cons "$code" green) (cons "prototype" (mk-object)))]                        ;_|
    [(s:new ef e) ;--------------------------------------------------------------------------fifth line
     (mk-let (j:deref (translate ef)) (lambda (ctor)                                       ; |
       (mk-let (mk-object (cons "proto" (j:get ctor (k:string "prototype")))) (lambda (obj); |
         (mk-let (j:get ctor (k:string "$code")) (lambda (f)                               ; |
           (j:seq (j:apply f (cons obj (map translate e))) obj)))))))]                     ;_|
  )
)