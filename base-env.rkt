;;;;;; base-env.rkt - Secure Racket Environment.    -*- Mode: Racket -*-

#lang racket

(require "label.rkt")

(define-syntax (define/secure stx)
  (syntax-case stx ()
    [(_ id kw-formals body . rest)
     (let ([proc-datum `(,#'λ ,#'kw-formals (@⊥ ,#'body . ,#'rest))])
       (with-syntax ([proc (syntax-property
                            (datum->syntax stx proc-datum stx stx)
                            'inferred-name (syntax-e #'id))])
         #`(begin
             (provide (rename-out [secure-id id]))
             (define secure-id (@⊥ proc)))))]))

(define/secure + zs (apply + (map @-value zs)))
(define/secure * zs (apply * (map @-value zs)))

(define/secure exit ([v (@⊥ #t)]) (exit v))
  
