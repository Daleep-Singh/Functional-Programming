#lang racket #| * CSC324H5 Fall 2021: Assignment 1 * |#
#|
Module:        a1
Description:   Assignment 1: Checking for Tail Recursion
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; This specifies which functions this module exports. Don't change this!
(provide is-tail-recursive)

; Import the testing library
(module+ test
  (require rackunit))


#|
(is-tail-recursive def) -> boolean? 
  def: (and/or symbol? number?) 
    A function definition that follows the grammar for <def> in the handout.
    You may assume that the function defined in def is recursive.

  Returns whether the definition is tail recursive
|#


(define (is-tail-recursive def)
  (let* ([fname (first (second def))]
         [body  (third def)])
    (all-tail-position body fname))) ; TODO: replace (void) with your code

;helper 1
(define/match (has-call expr fname)
  [('() fname) #f]  
  [((cons x xs) fname) (if (equal? x fname) #t (has-call xs fname))]  
  )  

;helper 2 
(define/match (all-tail-position expr fname)  
 [((cons 'let* xs) fname) (all-tail-position (rest xs) fname)]  
 [((cons 'if xs) fname) (if (all-tail-position (first (rest xs)) fname) #t (if (all-tail-position (rest (rest xs)) fname) #t #f))]  
 [((cons fname xs) fname) (if (equal? xs '()) #f #t)]  
 [((cons x xs) fname) (if (equal? xs '()) (all-tail-position x fname) (if (equal? x '()) (all-tail-position xs fname) #f))]  
 [(expr fname) #f]  
 [('() fname) #f]  
  )  
  
; You can write helper functions freely
(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Simple test for tail recursion"        ; Test label
               (is-tail-recursive '(def (f x) (f x)))  ; Actual value
               #t)                                     ; Expected value
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) x x)))
               #f)
  (test-equal? "Recursive call in let* definition"
               (is-tail-recursive '(def (f x) (let* ([a (f x)]) (g a))))
               #f)
  (test-equal? "Recursive call in let* body"
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (f a))))
               #t)

  ;Our tests
  
  ;------------if statements--------------
  (test-equal? "Recursive call in if body"
               (is-tail-recursive '(def (f x) (if f (g x) (f a))))
               #t)
  
  (test-equal? "Recursive call in if body"
               (is-tail-recursive '(def (f x) (if f (f x) (f x))))
               #t)

  (test-equal? "Recursive call in literals body"
               (is-tail-recursive '(def (f x) (if f (g x) (+ 7 2))))
               #f)

  (test-equal? "Recursive call in literals body"
               (is-tail-recursive '(def (f x) (if f (f x) (+ 7 2))))
               #t)
  ;------------function calls-------------
  (test-equal? "Recursive call in fn body"
               (is-tail-recursive '(def (f x) (g (f) (g x))))
               #f)
  ;------------Literals and identifiers-------------
  (test-equal? "Recursive call in literals body"
               (is-tail-recursive '(def (f x) 79))
               #f)
  
  (test-equal? "Recursive call in literals body"
               (is-tail-recursive '(def (f x) "george"))
               #f)

  (test-equal? "Recursive call in literals body"
               (is-tail-recursive '(def (f x) 'george))
               #f)

  ;------------Complicated ones------------------

  (test-equal? "Recursive call in complicated body"
               (is-tail-recursive '(def (f x) (let*([t 7] [q 32]) (if (equal? t 6) (g x) (if (equals? q 32) (f x) (g x))))))
               #t)
  
  )
