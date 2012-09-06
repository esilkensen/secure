;;;;;; nav.rkt - Secure Racket NaVs.    -*- Mode: Racket -*-

#lang racket

(provide (all-defined-out))

(struct δ (excp)
  #:property prop:custom-write
  (λ (b port write?)
    (fprintf port (if write? "δ ~s" "δ ~a") (δ-excp b))))
