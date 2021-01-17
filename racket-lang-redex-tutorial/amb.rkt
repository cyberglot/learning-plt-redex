#lang racket
(require redex)

(define-language L
  (e (e e)
     (λ (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ t t) num)
  (x variable-not-otherwise-mentioned))

; match the lambda case
(redex-match
   L
   ((λ (_ _) e) _)
   (term ((λ (x num) (+ x 1))
          17)))

; match the range portion
; apparently the name you call the variables matter
; I first tried `e` in the place of `t` and it returned `#f`
(redex-match
   L
   (→ _ t)
   (term (→ num (→ num num))))

