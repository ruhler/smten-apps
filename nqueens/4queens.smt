
; Sample SMTLIB2 format query for 4-queens problem

(set-option :produce-models true)
(set-logic QF_LIA)

(declare-fun q1 () Int)
(declare-fun q2 () Int)
(declare-fun q3 () Int)
(declare-fun q4 () Int)

; bound columns properly
(assert (and (>= q1 0) (< q1 4)))
(assert (and (>= q2 0) (< q2 4)))
(assert (and (>= q3 0) (< q3 4)))
(assert (and (>= q4 0) (< q4 4)))

; rows are disjoint by construction

; assert columns are disjoint
(assert (and (distinct q1 q2) (distinct q1 q3) (distinct q1 q4)))
(assert (and (distinct q2 q3) (distinct q2 q4)))
(assert (distinct q3 q4))

; assert positive diagonals are disjoint
(define-fun p1 () Int (+ 1 q1))
(define-fun p2 () Int (+ 2 q2))
(define-fun p3 () Int (+ 3 q3))
(define-fun p4 () Int (+ 4 q4))
(assert (and (distinct p1 p2) (distinct p1 p3) (distinct p1 p4)))
(assert (and (distinct p2 p3) (distinct p2 p4)))
(assert (distinct p3 p4))

; assert negative diagonals are disjoint
(define-fun n1 () Int (- 1 q1))
(define-fun n2 () Int (- 2 q2))
(define-fun n3 () Int (- 3 q3))
(define-fun n4 () Int (- 4 q4))
(assert (and (distinct n1 n2) (distinct n1 n3) (distinct n1 n4)))
(assert (and (distinct n2 n3) (distinct n2 n4)))
(assert (distinct n3 n4))

(check-sat)
(get-value (q1))
(get-value (q2))
(get-value (q3))
(get-value (q4))

