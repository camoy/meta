#lang info

;; General

(define collection "meta")
(define pkg-desc "Associate metadata with values.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/meta.scrbl" ())))
(define license '0BSD)

;; Dependencies

(define deps
  '("base"))

(define build-deps
  '("chk-lib"
    "racket-doc"
    "scribble-lib"))
