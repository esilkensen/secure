;;;;;; main.rkt - Secure Racket.    -*- Mode: Racket -*-

#lang racket

(provide
 require
 #%module-begin
 #%top-interaction
 #%top
 #%app
 (rename-out
  [BConst #%datum])
 get-pc
 ⊥ low high ⊤)

(struct @ (value label)
  #:property prop:custom-write
  (λ (a port write?)
    (fprintf port (if write? "~s @ ~s" "~a @ ~a")
             (@-value a) (@-label a))))

(define-values (⊥ low high ⊤)
  (values (@ '⊥ '⊥) (@ 'low '⊥) (@ 'high '⊥) (@ '⊤ '⊥)))

(define-syntax define-labeled
  (syntax-rules ()
    [(_ id expr)
     (define id (@ expr (@-value ⊥)))]
    [(_ (id . args) body ...)
     (define id (@ (λ args body ...) (@-value ⊥)))]))

(define pc (@-value ⊥))

(define-syntax-rule (BConst . datum)
  (@ (#%datum . datum) (@-value ⊥)))

(define-labeled (get-pc) pc)
