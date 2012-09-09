;;;;;; main.rkt - Secure Racket.    -*- Mode: Racket -*-

#lang racket

(require "label.rkt" "nav.rkt")

(provide
 require
 #%module-begin
 #%top-interaction
 #%top
 let
 (rename-out
  [secure-datum #%datum]
  [secure-define define]
  [secure-lambda lambda]
  [secure-lambda λ]
  [secure-app #%app])
 label-of
 get-pc
 bracket
 ⊥ low high ⊤)

(define pc (@-value ⊥))

(define (raise-pc! L . rest)
  (set-pc! (apply ∨ (cons pc (cons L rest)))))

(define (set-pc! L) (set! pc L))

(define-syntax-rule (secure-datum . datum)
  (@⊥ (#%datum . datum)))

(define-syntax secure-define
  (syntax-rules ()
    [(_ (id . args) body . rest)
     (define id (secure-lambda args body . rest))]
    [(_ id expr)
     (define id expr)]))

(define-syntax (secure-lambda stx)
  (syntax-case stx ()
    [(_ kw-formals body . rest)
     (let ([proc-datum `(,#'λ ,#'kw-formals ,#'body . ,#'rest)])
       (with-syntax ([proc (syntax-property
                            (datum->syntax stx proc-datum stx stx)
                            'inferred-name (syntax-local-name))])
         #'(@⊥ proc)))]))

(secure-define (label-of x) (@⊥ (@-label x)))

(secure-define (get-pc) (@⊥ pc))

(define-syntax-rule (secure-app proc-expr . args)
  (let ([proc (values proc-expr)])
    (raise-pc! (@-label proc))
    (if (eq? 'TFun (@-value (#%app (@-value tag-of) proc)))
        (#%app (@-value proc) . args)
        (@⊥ (prEx (@-value proc))))))

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

(define-syntax (define/secure stx)
  (syntax-case stx ()
    [(_ id kw-formals formals-list formals-types)
     (let ([proc-datum
            `(,#'λ ,#'kw-formals
               (define formals ,#'formals-list)
               (define L (apply ∨ (map @-label formals)))
               (let loop ([as formals] [ts ,#'formals-types])
                 (if (null? as)
                     (@ (apply ,#'id (map @-value formals)) L)
                     (if ((car ts) (@-value (car as)))
                         (loop (cdr as) (cdr ts))
                         (@ (prEx (car as)) L)))))])
       (with-syntax ([proc (syntax-property
                            (datum->syntax stx proc-datum stx stx)
                            'inferred-name (syntax-e #'id))])
         #`(begin
             (provide (rename-out [secure-id id]))
             (define secure-id (@⊥ proc)))))]))

(define/secure ∨ ls ls
  (map (λ (l) label?) ls))

(define/secure ⊑ (L1 L2 . rest) (cons L1 (cons L2 rest))
  (map (λ (l) label?) (cons L1 (cons L2 rest))))

(define/secure + zs zs
  (map (λ (z) number?) zs))

(define/secure * zs zs
  (map (λ (z) number?) zs))

(define/secure exit ([v (@⊥ #t)]) (list v)
  (list (λ (x) #t)))
