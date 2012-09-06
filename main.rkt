;;;;;; main.rkt - Secure Racket.    -*- Mode: Racket -*-

#lang racket

(require "label.rkt" "nav.rkt" "base-env.rkt")

(provide
 require
 #%module-begin
 #%top-interaction
 #%top
 (all-from-out "base-env.rkt")
 ⊥ low high ⊤)

(define pc (@-value ⊥))

(define (raise-pc! L . rest)
  (set-pc! (apply ∨ (cons pc (cons L rest)))))

(define (set-pc! L) (set! pc L))

(provide (rename-out [secure-datum #%datum]))
(define-syntax-rule (secure-datum . datum)
  (@⊥ (#%datum . datum)))

(provide (rename-out [secure-define define]))
(define-syntax secure-define
  (syntax-rules ()
    [(_ (id . args) body . rest)
     (define id (secure-lambda args body . rest))]
    [(_ id expr)
     (define id expr)]))

(provide (rename-out [secure-lambda lambda] [secure-lambda λ]))
(define-syntax (secure-lambda stx)
  (syntax-case stx ()
    [(_ kw-formals body . rest)
     (let ([proc-datum `(,#'λ ,#'kw-formals ,#'body . ,#'rest)])
       (with-syntax ([proc (syntax-property
                            (datum->syntax stx proc-datum stx stx)
                            'inferred-name (syntax-local-name))])
         #'(@⊥ proc)))]))

(provide label-of)
(secure-define (label-of x) (@⊥ (@-label x)))

(provide get-pc)
(secure-define (get-pc) (@⊥ pc))

(provide (rename-out [secure-app #%app]))
(define-syntax-rule (secure-app proc-expr . args)
  (let ([proc (values proc-expr)])
    (raise-pc! (@-label proc))
    (if (eq? 'TFun (@-value (#%app (@-value tag-of) proc)))
        (#%app (@-value proc) . args)
        (@⊥ (prEx (@-value proc))))))

(provide tag-of)
(secure-define (tag-of x)
  (let ([b (@-value x)])
    (cond [(label? b) (@⊥ 'TLab)]
          [(procedure? b) (@⊥ 'TFun)]
          [(number? b) (@⊥ 'TNum)]
          [(string? b) (@⊥ 'TStr)]
          [(tag? b) (@⊥ 'TTag)]
          [(δ? b) x])))

(define (tag? x)
  (member x '(TLab TFun TNum TStr TTag TUnknown)))
        
(provide bracket)
(define-syntax-rule (bracket label-expr expr)
  (let* ([pc0 pc]
         [x label-expr])
    (raise-pc! (@-label x))
    (if (eq? 'TLab (@-value (secure-app tag-of x)))
        (let ([t (values expr)])
          (cond
           [(⊑ (∨ (@-label t) pc)
               (∨ (@-value x) pc0 (@-label x)))
            (set-pc! (∨ pc0 (@-label x)))
            (@ (@-value t) (@-value x))]
           [else
            (set-pc! (∨ pc0 (@-label x)))
            (@ (δ 'EBracket) (@-value x))]))
        (@⊥ (prEx (@-value x))))))

(define (prEx b)
  (if (δ? b) b (δ 'EType)))
