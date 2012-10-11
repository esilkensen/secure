;;;;;; main.rkt - Secure Racket.    -*- Mode: Racket -*-

#lang racket

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
  [secure-app #%app]
  [make-excp ε])
 label-of
 get-pc
 bracket
 throw try
 ⊥ low high ⊤)

;; ----------------------------------------------------------------------------

(struct @ (value label)
  #:property prop:custom-write
  (λ (a port write?)
    (fprintf port (if write? "~s @ ~s" "~a @ ~a")
             (@-value a) (@-label a))))

(struct δ (excp)
  #:property prop:custom-write
  (λ (b port write?)
    (fprintf port (if write? "δ ~s" "δ ~a") (δ-excp b))))

(struct τ (excp)
  #:property prop:custom-write
  (λ (b port write?)
    (fprintf port (if write? "τ ~s" "τ ~a") (τ-excp b))))

(struct ε (excp)
  #:property prop:custom-write
  (λ (b port write?)
    (fprintf port (if write? "(ε ~s)" "(ε ~a)") (ε-excp b))))

(define-syntax-rule (make-excp excp) (@⊥ (ε 'excp)))

(struct Left (value)
  #:property prop:custom-write
  (λ (t port write?)
    (fprintf port (if write? "(Left ~s)" "(Left ~a)")
             (Left-value t))))

(struct Right (value)
  #:property prop:custom-write
  (λ (t port write?)
    (fprintf port (if write? "(Right ~s)" "(Right ~a)")
             (Right-value t))))

;; ----------------------------------------------------------------------------

(define-values (⊥ low high ⊤)
  (values (@ '⊥ '⊥) (@ 'low '⊥) (@ 'high '⊥) (@ '⊤ '⊥)))

(define (label? x)
  (or (eq? x '⊥) (eq? x 'low) (eq? x 'high) (eq? x '⊤)))

(define (@⊥ x) (@ x (@-value ⊥)))

(define (∨ . ls)
  (define (join L1 L2)
    (match (list L1 L2)
      [`(,L ,L) L]
      [`(⊥ ,L) L]
      [`(,L ⊥) L]
      [`(⊤ ,L) '⊤]
      [`(,L ⊤) '⊤]
      [`(low high) 'high]
      [`(high low) 'high]))
  (foldl join '⊥ ls))

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

(define pc (@-value ⊥))

(define (raise-pc! L . rest)
  (set-pc! (apply ∨ (cons pc (cons L rest)))))

(define (set-pc! L) (set! pc L))

;; ----------------------------------------------------------------------------

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
  (let ([proc proc-expr])
    (raise-pc! (@-label proc))
    (if (eq? 'TFun (@-value ((@-value tag-of) proc)))
        ((@-value proc) . args)
        (τ 'EType))))

(secure-define (tag-of x)
  (let ([b (@-value x)])
    (cond [(label? b) (@⊥ 'TLab)]
          [(procedure? b) (@⊥ 'TFun)]
          [(number? b) (@⊥ 'TNum)]
          [(string? b) (@⊥ 'TStr)]
          [(tag? b) (@⊥ 'TTag)]
          [else x])))

(define-syntax-rule (bracket label-expr expr)
  (let* ([pc0 pc]
         [x label-expr])
    (raise-pc! (@-label x))
    (if (eq? 'TLab (@-value (secure-app tag-of x)))
        (let ([t (values expr)])
          (cond
           [(and (τ? t)
                 (⊑ pc (∨ (@-value x) pc0 (@-label x))))
            (set-pc! (∨ pc0 (@-label x)))
            (@ (Right (@⊥ (ε (τ-excp t)))) (@-value x))]
           [(τ? t)
            (set-pc! (∨ pc0 (@-label x)))
            (@ (Right (@⊥ (ε 'EBrk))) (@-value x))]
           [(⊑ (∨ (@-label t) pc)
               (∨ (@-value x) pc0 (@-label x)))
            (set-pc! (∨ pc0 (@-label x)))
            (@ (Left (@⊥ (@-value t))) (@-value x))]
           [else
            (set-pc! (∨ pc0 (@-label x)))
            (@ (Right (@⊥ (ε 'EBrk))) (@-value x))]))
        (@⊥ (τ 'EType)))))

(define-syntax-rule (throw expr)
  (let* ([pc0 pc]
         [v expr])
    (set-pc! (∨ pc0 (@-label v)))
    (if (ε? (@-value v))
        (τ (ε-excp (@-value v)))
        (τ 'EType))))

(define-syntax try
  (syntax-rules (catch)
    [(_ try-expr catch x catch-expr)
     (let* ([t try-expr]
            [pc1 pc])
       (if (τ? t)
           (let* ([x (@⊥ (ε (τ-excp t)))]
                  [res catch-expr])
             (set-pc! pc1)
             res)
           t))]))

;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------

(define (tag? x)
  (member x '(TLab TFun TNum TStr TTag TUnknown)))
        
(define (prEx b)
  (if (δ? b) b (τ 'EType)))
