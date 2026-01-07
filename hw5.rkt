#|
 ; Author: Steven Carr
; Last Edited: October 2023
;  Implemented the substitution operation, evaluation of expressions using substitution and evaluation of
   expressions using environments for a given AST.
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
#lang typed/racket
(provide (all-defined-out))
(require "hw5-util.rkt")
;; END OF REQUIRES
  
;; Exercise 1
(: s:subst (-> s:expression s:variable s:value s:expression))
(define (s:subst exp var val)
  (match exp
    [(s:number n) exp] ; If the expression is a number, we return the number (the expression)
    [(s:variable y)
     (cond [(equal? var exp) val] ; If the expression is a variable y, we check to see if y = x (our "var" in the function definition). If x = y, return the value
           [else exp])] ; If y does not equal x, return the expression (y)
    [(s:lambda y exp)  ; If the expression is a lambda with a variable y and an expression
     (cond [(equal? y var) (s:lambda var exp)] ; If y = x (var), we return the lambda with x.e
           [else (s:lambda y (s:subst exp var val))])] ; Otherwise, if y does not = x, we return the lambda with y.(e[x->v])
    [(s:apply e1 e2) (s:apply (s:subst e1 var val) (s:subst e2 var val))] ; If the expression is an apply, we make a recursive call to subst with the two options as the expression
    )
  )

;; Exercise 2
(: s:eval (-> (-> s:expression s:variable s:value s:expression) s:expression s:value))
(define (s:eval subst exp)
  (match exp
    [(s:apply ef ea) ; If the expression is an s:apply we want to pattern match the result
     (match (s:eval subst ef) 
       [(s:lambda x eb) ; If the subsequent result is a lambda x.eb
        (define va (s:eval subst ea)) ; Define va and vb, then return vb
        (define vb (s:eval subst (subst eb x va)))
        vb])]
    [(? s:value?) exp]) ; If the expression is already a value, return itself (the value)
)

;; Exercise 3
(: e:eval (-> e:environ e:expression e:value))
(define (e:eval env exp)
  (match exp
    [(? e:value?) exp] ; If the expression is a value, return the expression
    [(? e:variable?) (e:env-get env exp)] ; If the expression is a variable, then use env-get to get the value associated with the variable in the hash table?
    [(e:lambda param body) (e:closure env param body)] ; If the expression is a lambda, we want to capture the environment in a closure with the parameters and body
    [(e:apply func arg)  ; If the expression is an apply, it pattern matches the result of the func
     (match (e:eval env func)
       [(e:closure closure-env param closure-body) ; If the func is matched to be an eval of an environment with a func, then eval the argument with it's current environment, use
        (e:eval (e:env-put closure-env param (e:eval env arg)) closure-body)])]  ; env-put to put the closure environment, the parameter and the evaluated argument together in an environment,
    ) ; And evaluate the new given environment and the closure-body (expression)
)

;; Exercise 4 (Manually graded)
#|
I think using an environment over substitution is best when dealing with a function that
can take multiple arguments, so generally when we're dealing with multiple things, whereas
using substitution might be seen as better when dealing with a function that only has one argument.
|#

;; Exercise 5 (Manually graded)
#|
One benefit of using the formal specification to help with implementing a software system could be
that when we use mathematical notation, there is little room for error in interpretation.  You are
almost following a very strict guideline into how something should be implemented, it's like simply
reading what you have to do and simply translating it into code, you don't have to try to interpret anything.
Because of this, I think more time can be spent on optmizing the code rather than figuring out how to
design something and then implement it as you go.  Another possible benefit could be that reading the
mathematical notation of something can be easier, at least once it's understood, than trying to decipher
a piece of code.  Reading multiple different pieces of code will always take time to figure out what
each individual snippet means, but once general mathematical notation is understood, a wider variety
or proper mathematical formats can be read faster than it would compared to reading code.
|#