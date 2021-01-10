#lang eopl

"""
Name: Austin Kim
Class: CS-135
Date: 3/26/2019
Pledge: I pledge my honor that I have abided by the Stevens Honor System - Austin Kim (akim10)
"""

;;Remember that empty list is valid input unless otherwise stated

;; ---------------------------------------------------Problem 1----------------------------------------------- (DONE)

;;Define composite
;; Composite takes an inner relation and an outer relation and returns a relation
;; For every (a b) tuple in the inner relation if b equals an A in a (A B) tuple in the outer relation
;; then add the duple (a B) to the relation you will be returning
;; order doesn't matter

;; Examples:
;; (composite '((2 4) (3 5) (4 6) (5 7)) '((1 2) (2 3) (3 4) (4 5)) ) -> '((1 4) (2 5) (3 6) (4 7))
;; (composite '((2 4) (5 8) (3 4)) '((2 4) (5 8) (3 4))) -> '()
;; (composite '((1 1) (2 2) (3 3)) '((1 1) (2 2) (3 3))) -> '((1 1) (2 2) (3 3))

;; Type Signature: (composite relation relation) -> relation

(define (composite  relationOuter relationInner)
  (if (null? relationOuter)
      '()
      (append (compositeHelper (car relationOuter) relationInner) (composite (cdr relationOuter) relationInner) )
  )
)

(define (compositeHelper pair relationInner)
  (if (null? relationInner)
      '()
      (if (equal? (car (cdr (car relationInner))) (car pair))
          (cons (list (car (car relationInner)) (car (cdr pair))) (compositeHelper pair (cdr relationInner)))
          (compositeHelper pair (cdr relationInner))
      )
  )
)

;; ---------------------------------------------------Problem 2----------------------------------------------- (DONE)

;; Define power
;; power takes a relation and applies composite to itself k times
;; You will need to use a helper to store the initial relation
;; when k is zero it should return '()
;; order doesn't matter

;; Examples:
;; (power '((1 2) (2 3) (3 4) (4 1)) 0) -> '()
;; (power '((1 2) (2 3) (3 4) (4 1)) 1) -> '((1 2) (2 3) (3 4) (4 1))
;; (power '((1 2) (2 3) (3 4) (4 1)) 2) -> '((1 3) (2 4) (3 1) (4 2))
;; (power '((1 2) (2 3) (3 4) (4 1)) 3) -> '((1 4) (2 1) (3 2) (4 3))

;; Type Signature: (power relation int) -> relation

;; relationOuter stays the same, relationInner turns into result each time.

(define (power relation k)
  (if (equal? k 0)
      '()
      (if (equal? k 1)
          relation
          (composite relation (power relation (- k 1)))
      )
  )
)

;; ---------------------------------------------------Problem 3-----------------------------------------------

;; Define transitive-closure
;; transitive-closure should compute the transitive-closure of a relation
;; Transitive-closure is the union of all possible powers of the relation
;; a helper will be needed to keep track of the current power
;; each (a b) tuple can only be used at most once in a path in power 
;; order doesn't matter

;; Examples:
;; (transitive-closure '((1 2) (2 3) (3 1))) -> '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
;; (transitive-closure '((1 3) (3 5) (2 4) (5 6) (2 3))) -> '((1 3) (3 5) (2 4) (5 6) (2 3) (1 5) (1 6) (3 6) (2 5) (2 6))

;; Type Signature: (transitive-closure relation) -> relation

"""
(define (transitive-closure relation)
  (define k 2)
  (tcHelper relation k)
)

(define (tcHelper relation k)
  (if (null? relation)
      '()
      (if (set-equal? (power relation k) (power relation (+ k 1)))
          (make-set (union (power relation k) (power relation (+ k 1))))
          (tcHelper relation (+ k 1))
      )
   )
)
"""

(define (transitive-closure relation)
  '()
)


;; ---------------------------------------------------Problem 4-----------------------------------------------

;; Define transitive?
;; returns if a given relation is transitive

;; Examples:
;; (transitive? '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))) -> #t
;; (transitive? '((1 2) (2 3) (3 1))) -> #f

;; Type Signature: (transitive? relation) -> boolean

(define (transitive? relation)
  (if (null? relation)
      #t
      (if (null? (related-to (car (cdr (car relation))) relation))
          #f
          (if (subset? '(((car (cdr (car relation))) (car (related-to (car (cdr (car relation))) relation)))) relation)
              (if (> 1(length (related-to (car (cdr (car relation))) relation)))
                  (if (subset? '(((car (cdr (car relation))) (car (related-to (car (cdr (car relation))) relation)))) relation)
                      (if (> 1(length (related-to (car (cdr (car relation))) relation)))
                          #t
                          (transitive? (cdr relation))
                          )
                      #f
                      )
                  (transitive? (cdr relation))
                  )
              #f
              )
          )
      )
  )



;; ---------------------------------------------------Problem 5----------------------------------------------- (DONE)

;; Define EQ-relation?
;; returns if a given relation is an EQ-relation
;; A relation is an EQ-relation if it is symmetric, reflexive, and transitive

;; Examples:
;; (EQ-relation? '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) 3) -> #t
;; (EQ-relation? '((1 1) (1 2) (2 1)) 2) -> #f
(define (EQ-relation? relation n)
  (if (symmetric? relation)
      (if (transitive? relation)
          (if (reflexive? relation n)
              #t
              #f
          )
          #f
      )
      #f
  )
)




;; ---------------------------------------------------END-----------------------------------------------

;; FUNCTIONS BELOW ARE GIVEN (REFERENCE)



;; Given item, and list-of-items, return #t if item is in list-of-items, #f otherwise.
;; Type signature: (element? element list) -> boolean
(define (element? item list-of-items)
  (if (null? list-of-items)                  ;Is our "set" empty?
      #f                                     ;If empty, not an element!
      (if (equal? item (car list-of-items))  ;Is our item first in list?
          #t                                 ;Yes?  Then it's an element!
          (element? item (cdr list-of-items)))));No? Check the rest.



;; make-set takes a list and removes all duplicates
;; Type signature: (make-set list) -> set
(define (make-set list-of-items)
  (if (null? list-of-items) ;An empty list can have no duplicates,
      '()                   ;so just return an empty list.
      (if (element? (car list-of-items) (cdr list-of-items))
          (make-set (cdr list-of-items))
          (cons (car list-of-items) (make-set (cdr list-of-items))))))



;; Given two lists, return a set representing the set which contains all of the elements from each list EXACTLY once.
;; Type signature: (union list list) -> set
(define (union setA setB)
  (make-set (append setA setB))) 




;; Given two lists A and B, return the set representing the set which contains the elements in A and in B.
;; Type signature: (intersection list list) -> set
(define (intersection setA setB)
  (make-set (Intersection (make-set setA) (make-set setB))))

(define (Intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (cons (car setA) (intersection (cdr setA) setB))
          (intersection (cdr setA) setB))))



;; Takes two sets returns whether the first is the subset of the second. 
;; Type signature: (subset? set set) -> boolean
(define (subset? setA setB)
  (if (null? setA)
      #t
      (if (element? (car setA) setB)
          (subset? (cdr setA)  setB)
          #f)))




;; Determines whether two sets are equivalent (i.e. A = B => every element in A is in B and every element in B is in A).
;; Order of inputs does not matter.
;; Type signature: (set-equal? set set) -> boolean
(define (set-equal? setA setB)
   (and (subset? setA setB) (subset? setB setA)))





(define (proper-subset? setA setB)
  (and (subset? setA setB) (not (set-equal? setA setB))))






;; The set difference A - B is the set of the list of all elements of A which are not in list B.
;; Type signature: (set-difference list list) -> set
(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))

(define (Set-Difference setA setB)
  (if (null? setA)
      '()
      (if (element? (car setA) setB)
          (Set-Difference (cdr setA) setB)
          (cons (car setA) (Set-Difference (cdr setA) setB)))))










(define (sym-diff setA setB)
  (union (set-difference setA setB) (set-difference setB setA)))

(define (cardinality set)
  (length (make-set set)))

(define (disjoint? setA setB)
  (null? (intersection setA setB)))

(define (superset? setA setB)
  (subset? setB setA))

(define (insert element set)
  (make-set (cons element set)))

(define (remove element set)
  (set-difference set (list element)))

(define (id n)
    (if (zero? n)
        '()
        (cons (list n n) (id (- n 1)))))

(define (reflexive? relation n)  
    (subset? (id n) relation))

(define (reflexive-closure relation n)
    (union relation (id n)))

(define (flip-pairs relation)
    (if (null? relation)
        '()
        (cons (reverse (car relation)) (flip-pairs (cdr relation)))))

(define (symmetric? relation)
   (set-equal? relation (flip-pairs relation)))

(define (symmetric-closure relation) 
   (union relation (flip-pairs relation)))

(define (related-to element relation)
   (if (null? relation)
      '()
      (if (equal? element (caar relation))
          (cons (cadar relation) (related-to element (cdr relation)))
          (related-to element (cdr relation)))))
