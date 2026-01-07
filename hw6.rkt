#|
    Author: Steven Carr
    Last Edited: November 2023
    Implemented the evaluation of expressions and terms using a mutable environment
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
(require "hw6-util.rkt")
(provide (all-defined-out))
;; END OF REQUIRES

;; Exercise 1
(: eval-exp (-> memory handle d:expression (eff memory d:value)))
(define (eval-exp mem env exp)
  ; mem is M
  ; env is E
  (match exp
    [(? d:value?)
      ; Return: v ▶ M
      (eff mem exp)
      ]
    [(? d:variable?)
      (eff mem (environ-get mem env exp))
      ; Return: E(x) ▶ M
    ]
    [(d:lambda x t)
      ; Return: {E, λx.t} ▶ M
      (eff mem (d:closure env x t))
    ]
    [(d:apply ef ea)
      (match (eval-exp mem env ef)
        ;; ef ⇓E {Ef, λx.tb} ▶ M1
        [(eff M1 (d:closure Ef x tb))
          ;; ea ⇓E va ▶ M2
          (match (eval-exp M1 env ea)
             [(eff M2 va)
          ;; Eb ← Ef + [x := va] ▶ M3
              (match (environ-push M2 Ef x va)
                [(eff M3 Eb)
          ;; tb ⇓Eb vb ▶ M4
                 (match (eval-term M3 Eb tb)
                   [(eff M4 vb)
          ;; Return: vb ▶ M4
                    (eff M4 vb)])])])
        ]
      )
    ]
  )
)
 


;; Exercise 2
(: eval-term (-> memory handle d:term (eff memory d:value)))
(define (eval-term mem env term)
  (match term
    [(d:define x e)
      ;; e ⇓E v ▶ M1
      (match (eval-term mem env e)
        ;; M1 is now the new memory state and v is the result of e
        [(eff M1 v)
          ;; E ← [x := v] ▶ M2
          (define new-env (environ-put M1 env x v))
          ;; return: void ▶ M2
          (eff new-env (d:void))])]
    [(d:seq t1 t2)
      ;; t1 ⇓E v1 ▶ M1
      (match (eval-term mem env t1)
        ;; M1 is now the new memory state and v1 is the result of t1
        [(eff M1 v1)
          ;; t2 ⇓E v2 ▶ M2
          (match (eval-term M1 env t2)
            ;; M2 is now the new memory state and v2 is the result of t2
            [(eff M2 v2)
              ;; return: v2 ▶ M2
              (eff M2 v2)])])]
    [(? d:expression?) (eval-exp mem env term)]))

;; Exercise 3 (Manually graded)
#|
I believe that the main difference would involve in a regular Racket program, a variable
being bounded to a specific value is determined by its given environment.  So, there could
be a variable x defined in one environment and enclosure, and then later on defined in
another context with a different value than previously defined; the value returned or used
with variable x depends on the mos recently defined context of its environment.  Meanwhile,
in langauge lambda.D, a variable can be bound to a given value and that same value can be
referenced or recalled regardless of what stage in the program we are (I believe).  An example
of a progrma could be as simple as printing a given variable x, where in normal racket program
you could define x separately such as (define x 10), but if you create another function such
as:
(define printx
  (define x 5)
  (displayln x)) <-- the result of this would print 5, even if x was earlier defined outside of
this scope as 10.  If we were to use lambda.D, we should be able to use x in such a way to recall
the value of x being 10 regardless of the current scope.
|#
