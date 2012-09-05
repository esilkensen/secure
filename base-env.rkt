;;;;;; base-env.rkt - Secure Racket Environment.    -*- Mode: Racket -*-

#lang racket

(require "label.rkt")

(define-syntax (define/secure stx)
  (define (build-name id . parts)
    (let ([str (apply string-append
                      (map (λ (p)
                             (if (syntax? p)
                                 (symbol->string (syntax-e p))
                                 (format "~a" p)))
                           parts))])
      (datum->syntax id (string->symbol str) id)))
  (syntax-case stx ()
    [(_ id kw-formals body . rest)
     (let ([proc-datum `(,#'λ ,#'kw-formals (@⊥ ,#'body . ,#'rest))])
       (with-syntax ([proc (syntax-property
                            (datum->syntax stx proc-datum stx stx)
                            'inferred-name (syntax-e #'id))]
                     [secure-id (build-name #'id "secure-" #'id)])
         #`(begin
             (provide (rename-out [secure-id id]))
             (define secure-id (@⊥ proc)))))]))

(define/secure + zs (apply + (map @-value zs)))
(define/secure * zs (apply * (map @-value zs)))

(define/secure exit ([v (@⊥ #t)]) (exit v))
  
