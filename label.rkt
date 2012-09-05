;;;;;; label.rkt - Secure Racket Labels.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(struct @ (value label)
  #:property prop:custom-write
  (λ (a port write?)
    (fprintf port (if write? "~s @ ~s" "~a @ ~a")
             (@-value a) (@-label a))))

(define-values (⊥ low high ⊤)
  (values (@ '⊥ '⊥) (@ 'low '⊥) (@ 'high '⊥) (@ '⊤ '⊥)))

(define-syntax-rule (@⊥ x) (@ x (@-value ⊥)))

(define (∨ L1 L2 . rest)
  (define (join L1 L2)
    (match (list L1 L2)
      [`(,L ,L) L]
      [`(⊥ ,L) L]
      [`(,L ⊥) L]
      [`(⊤ ,L) '⊤]
      [`(,L ⊤) '⊤]
      [`(low high) 'high]
      [`(high low) 'high]))
  (foldl join L1 (cons L2 rest)))

(define (⊑ L1 L2 . rest)
  (define (leq L1 L2)
    (match (list L1 L2)
      [`(,L ,L) #t]
      [`(⊥ ,L) #t]
      [`(,L ⊥) #f]
      [`(⊤ ,L) #f]
      [`(,L ⊤) #t]
      [`(low high) #t]
      [`(high low) #f]))
  (let loop ([L L1] [ls (cons L2 rest)])
    (or (null? ls)
        (and (leq L (car ls))
             (loop (car ls) (cdr ls))))))
