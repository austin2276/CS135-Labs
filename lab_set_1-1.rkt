#lang eopl

;; Name: Austin Kim
;; Date: 3/5/2019
;; Class: CS135-LE
;; Pledge: "I pledge my honor that I have abided by the Stevens Honor System." - Austin Kim (akim10)

;; element?, intersection, set-difference are the only functions that cannot be done in a simple one line function. 

;;list: a grouping of elements (multiples allowed)
;;set: a grouping of unique elements (no multiples)
;;Be careful. Some of these functions take lists and return sets.


;; ----- PROBLEM 1 ----- DONE


;; Define element?
;; Given item, and list-of-items, return #t if item is in list-of-items,
;; #f otherwise

;; Examples:
;; (element? 0 '()) => #f
;; (element? 8 '(7 8 9)) => #t
;; (element? 7 '(1 2 3 4)) => #f
;; (element? 'a '(the man saw a dog)) => #t


;; Type signature: (element? element list) -> boolean

(define (element? item list-of-items)
  (if [member item list-of-items] #t #f) 
)


;; ----- PROBLEM 2 ----- (GIVEN)


;; make-set takes a list and removes all duplicates
;; This will be useful for the rest of our functions

;; Type signature: (make-set list) -> set

(define (make-set list-of-items)
  (cond
    [(null? list-of-items)
     '()] ; Empty lists never have duplicates
    [(element? (car list-of-items) (cdr list-of-items))
     (make-set (cdr list-of-items))]
    [else
     (cons (car list-of-items) (make-set (cdr list-of-items)))]))


;; ----- PROBLEM 3 ----- DONE


;; Define union
;; Given two lists, return a set representing the set which
;; contains all of the elements from each list EXACTLY once.

;; NOTE: Order does not matter when checking your work for union

;; Examples:
;; (union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6) 
;; (union '(1 2 3) '(1 2 3)) => '(1 2 3)
;; (union '(1 1 1) '()) => '(1)

;; Type signature: (union list list) -> set

(define (union listA listB)
 (make-set (append listA listB))
)


;; ----- PROBLEM 4 ----- DONE


;; Define intersection
;; Given two lists A and B, return the set representing the set which
;; contains the elements in A and in B.

;; NOTE: Order will not matter when checking your work for intersection

;; Examples:
;; (intersection '(1 2 3 4) '(2 4 5)) => '(2 4)
;; (intersection '(s a m u e l) '(k r a u s)) => '(a u s)
;; (intersection '(r i p) '(l i e b)) => '(i)
;; (intersection '(a a a) '(a a a)) => '(a)

;; Type signature: (intersection list list) -> set

(define (intersection listA listB)
  (if (null? listA)
      '()
       (if [member (car listA) listB]
           (make-set (cons (car listA) (intersection (cdr listA) listB))) ;;if first element in listA is in list B, that element + function without first element of listA
           (make-set (intersection (cdr listA) listB))
           )
       )
      )


;; ----- PROBLEM 5 -----


;; Define subset?
;; Takes two sets returns whether the first is the subset of the second.
;; (i.e., every element in the first is also in the second set)

;; Examples:
;; (subset? '() '()) => #t
;; (subset? '(1 2 3) '(1 2 3 4 5)) => #t
;; (subset? '(115 284 385) '(115 146 284 135 385 392)) => #t
;; (subset? '(-2 0 2) '(-1 1 3)) => #f
;; (subset? '(-1 1 2) '(-1 1 3 5 7)) => #f

;; Type signature: (subset? set set) -> boolean

(define (subset? setA setB)
  "A <= B")


;; ----- PROBLEM 6 -----


;; Define set-equal?
;; Determines whether two sets are equivalent (i.e. A = B => every element
;; in A is in B and every element in B is in A)
;; NOTE: order does not matter, so you cannot simply use (equal? A B).
;; NOTE: For the simplest solution, this can be a one-liner

;; Examples:
;; (set-equal? '() '()) => #t
;; (set-equal? '(a b c) '(a b c)) => #t
;; (set-equal? '(1 2 3 4) '(4 3 1 2)) => #t
;; (set-equal? '(1 2 3) '(1 2 4)) => #f

;; Type signature: (set-equal? set set) -> boolean

(define (set-equal? setA setB)
  "x <- A <=> x <- B")


;; ----- PROBLEM 7 -----


;; Define set-difference
;; The set difference A - B is the set of the list of all elements of A
;; which are not in list B.

;; Examples:
;; (set-difference '(1 2 3) '(2 3 4)) => '(1)
;; (set-difference '(1 2 3) '(1 2 3)) => '()
;; (set-difference '(1 2 3) '(4 5 6)) => '(1 2 3)
;; (set-difference '() '(1 2 3))      => '()
;; (set-difference '(1 1 2 3 3) '())  => '(1 2 3)

;; Type signature: (set-difference list list) -> set

(define (set-difference listA listB)
  "A - B")


;; ----- PROBLEM 8 -----


;; Define sym-diff
;; The symmetric difference of two lists (A, B) is the union of
;; A - B and B - A. (It is the set equivalent of the logical operator
;; "xor".)

;; Examples:
;; (sym-diff '(1 2 3) '(3 4 5)) => '(1 2 4 5)
;; (sym-diff '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (sym-diff '(1 2 3) '(1 2 3)) => '()
;; (sym-diff '(1 2) '(1 2 3 4)) => '(3 4)
;; (sym-diff '(1 1 1) '()) => '(1)

;; Type signature: (sym-diff list list) -> set

(define (sym-diff listA listB)
  "A xor B")


;; ----- PROBLEM 9 -----


;; Define cardinality
;; the cardinality of a list |A| is the number of unique elements in
;; a collection, or the length of the set.

;; Examples:
;; (cardinality '(1 2 3))    => 3
;; (cardinality '(1 1 2 3 3) => 3
;; (cardinality '(1 1 1 1 1) => 1
;; (cardinality '() )        => 0

;; Type signature: (cardinality list) -> int

(define (cardinality lst)
  "|A|")


;; ----- PROBLEM 10 -----


;; Define disjoint
;; Two sets are disjoint if they don't have any elements in common.
;; I.e., their intersection is empty.

;; Examples:
;; (disjoint? '(1 2 3) '()) => #t
;; (disjoint? '(1 2 3) '(1) => #f
;; (disjoint? '(1 2 3) '(4 5 6) => #t

;; Type signature: (disjoint? set set) -> boolean

(define (disjoint? setA setB)
  "A ^ B = Ø")


;; ----- PROBLEM 11 -----


;; Define superset?
;; A is a superset B if every element in B is an element of A.
;; i.e. (A >= B) <==> (B <= A)

;; Examples:
;; (superset? '() '()) => #t
;; (superset? '(1 2 3 4 5) '(1 2 3)) => #t
;; (superset? '(-1 1 3) '(-2 0 2)) => #f

;; Type signature: (superset? set set) -> boolean

(define (superset? setA setB)
  "A >= B")


;; ----- PROBLEM 12 -----


;; Define insert
;; Takes a list and an element and returns the set with the element
;; added to the set (NOTE: if the element is already present, it just
;; returns the original set)

;; Examples:
;; (insert 0 '(1 2 3)) => '(0 1 2 3)  [or '(1 2 3 0) etc.]
;; (insert 1 '(1 2 3)) => '(1 2 3)
;; (insert 0 '(0 0 0)) => '(0)

;; Type signature: (insert element list) => set

(define (insert element lst)
  "Add element to set")


;; ----- PROBLEM 13 -----


;; Define remove
;; Takes an element and a set and returns the set without that element.
;; NOTE: if the element is not initially in the set, it should return the
;; equivalent of the original set rather than raising an exception.

;; (remove 2 '(1 2 3)) => '(1 3)
;; (remove 3 '(3))     => '()
;; (remove 4 '(1 2 3)) => '(1 2 3)

;; Type signature: (remove element set) => set

(define (remove element set)
  "remove(x, A) = A - {x}")


;; January 2018
;; Samuel Kraus and Edward Minnix
;; Stevens Institute of Technology
;; CS 135  Discrete Structures
