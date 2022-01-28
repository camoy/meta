#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [meta? predicate/c]
  [meta-ref (->* (any/c any/c) (failure-result/c) any/c)]
  [meta-has-key? (-> any/c any/c boolean?)]
  [meta-set (-> meta? any/c any/c meta?)]
  [meta-update (->* (meta? any/c (-> any/c any/c)) (failure-result/c) meta?)]
  [meta-remove (-> any/c any/c any/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/async-channel
         racket/list
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define EMPTY (make-immutable-hash))
(define UNDEFINED (gensym))
(define MISSING (gensym))
(define REMOVE (gensym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicates

(define can-chaperone?
  (or/c procedure?
        struct?
        vector?
        box?
        hash?
        (or/c set? set-mutable? set-weak?)
        struct-type?
        evt?
        channel?
        async-channel?
        continuation-prompt-tag?
        continuation-mark-key?))

(define cannot-chaperone?
  (or/c pair?
        string?
        bytes?
        regexp?
        pregexp?
        byte-regexp?
        byte-pregexp?))

(define meta?
  (flat-named-contract
   'meta?
   (or/c can-chaperone?
         cannot-chaperone?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions

(define (meta-ref e k [failure-result UNDEFINED])
  (define meta-hash
    (cond
      [(can-chaperone? e) (meta-ref/chaperone e)]
      [(cannot-chaperone? e) (meta-ref/no-chaperone e)]
      [else #f]))
  (cond
    [(and meta-hash (hash-has-key? meta-hash k))
     (hash-ref meta-hash k)]
    [(eq? failure-result UNDEFINED)
     (error 'meta-ref "no value found for key ~v" k)]
    [(procedure? failure-result) (failure-result)]
    [else failure-result]))

(define (meta-has-key? e k)
  (define v (meta-ref e k MISSING))
  (not (eq? v MISSING)))

(define (meta-set e k v)
  (if (can-chaperone? e)
      (meta-set/chaperone e k v)
      (meta-set/no-chaperone e k v)))

(define (meta-update e k f [failure-result UNDEFINED])
  (define failure-result*
    (if (eq? failure-result UNDEFINED)
        (λ () (error 'meta-update "no value found for key ~v" k))
        failure-result))
  (define v (meta-ref e k failure-result*))
  (meta-set e k (f v)))

(define (meta-remove e k)
  (cond
    [(can-chaperone? e) (meta-set/chaperone e k REMOVE)]
    [(cannot-chaperone? e) (meta-set/no-chaperone e k REMOVE)]
    [else e]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chaperone meta

(define-values (impersonator-prop:meta -meta? -meta-ref)
  (make-impersonator-property 'meta))

(define (meta-ref/chaperone e)
  (-meta-ref e #f))

(define (meta-set/chaperone e k v)
  (define old-meta (-meta-ref e EMPTY))
  (define new-meta (meta-hash-set old-meta k v))
  (cond
    [(procedure? e)
     (chaperone-procedure e #f impersonator-prop:meta new-meta)]
    [(struct? e)
     (define-values (struct-type skipped?) (struct-info e))
     (chaperone-struct e struct-type impersonator-prop:meta new-meta)]
    [(vector? e)
     (chaperone-vector e #f #f impersonator-prop:meta new-meta)]
    [(box? e)
     (chaperone-box e (λ (_ x) x) (λ (_ x) x) impersonator-prop:meta new-meta)]
    [(hash? e)
     (define (ref-proc h k) (values k (λ (h _ v) v)))
     (define (set-proc h k v) (values k v))
     (define (remove-proc h k) k)
     (define (key-proc h k) k)
     (chaperone-hash e
                     ref-proc set-proc remove-proc key-proc
                     impersonator-prop:meta new-meta)]
    [(or (set? e) (set-mutable? e) (set-weak? e))
     (chaperone-hash-set e #f #f #f #f
                         impersonator-prop:meta new-meta)]
    [(struct-type? e)
     (define struct-info-proc values)
     (define make-constructor-proc values)
     (define (guard-proc . args)
       (apply values (drop-right args 1)))
     (chaperone-struct-type e
                            struct-info-proc
                            make-constructor-proc
                            guard-proc
                            impersonator-prop:meta new-meta)]
    [(evt? e)
     (define (proc e) (values e values))
     (chaperone-evt e proc impersonator-prop:meta new-meta)]
    [(channel? e)
     (define (get-proc c) (values c values))
     (define (put-proc c v) v)
     (chaperone-channel e
                        get-proc put-proc
                        impersonator-prop:meta new-meta)]
    [(async-channel? e)
     (chaperone-async-channel e values values
                              impersonator-prop:meta new-meta)]
    [(continuation-prompt-tag? e)
     (chaperone-prompt-tag e
                           values values
                           impersonator-prop:meta new-meta)]
    [(continuation-mark-key? e)
     (chaperone-continuation-mark-key e
                                      values values
                                      impersonator-prop:meta new-meta)]))
(define (meta-hash-set ht k v)
  (cond
    [(eq? v REMOVE) (hash-remove ht k)]
    [else (hash-set ht k v)]))

(define (copy e)
  (cond
    [(pair? e) (cons (car e) (cdr e))]
    [(string? e) (string-copy e)]
    [(bytes? e) (bytes-copy e)]
    [(pregexp? e) (pregexp (object-name e))]
    [(regexp? e) (regexp (object-name e))]
    [(byte-pregexp? e) (byte-pregexp (object-name e))]
    [(byte-regexp? e) (byte-regexp (object-name e))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-chaperone metadata

(define immutable-metas (make-weak-hasheq))

(define (meta-ref/no-chaperone e)
  (hash-ref immutable-metas e #f))

(define (meta-set/no-chaperone e k v)
  (define old-meta (hash-ref immutable-metas e EMPTY))
  (define new-meta (meta-hash-set old-meta k v))
  (define e* (copy e))
  (hash-set! immutable-metas e* new-meta)
  e*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require (submod "..")
           chk
           racket/match)

  (struct my-struct (x y z) #:transparent)

  (define (byte-regex-tester v)
    (equal? (bytes->string/utf-8 (object-name v))
            "hel*o"))

  (define (struct-type-equal? st1 st2)
    (define (extract st)
      (match-define-values (name fld-n auto-n _ _ imm sup skip)
        (struct-type-info st))
      (list name fld-n auto-n imm sup skip))
    (equal? (extract st1) (extract st2)))

  (define things
    (list (cons (λ (x) x)
                (λ (p) (= (p 42) 42)))
          (cons (my-struct 1 2 3)
                (λ (s) (= (my-struct-x s) 1)))
          (cons (vector 1 2 3)
                (λ (v)
                  (vector-set! v 1 42)
                  (and (= (vector-ref v 0) 1)
                       (= (vector-ref v 1) 42))))
          (cons (box 1)
                (λ (b)
                  (and (= (unbox b) 1)
                       (set-box! b 42)
                       (= (unbox b) 42))))
          (cons (make-hash `(["hi" . 2]))
                (λ (h)
                  (hash-ref-key h (string-copy "hi"))
                  (and (= (hash-ref h "hi") 2)
                       (hash-set! h "hi" 42)
                       (= (hash-ref h "hi") 42)
                       (equal? (hash-ref-key h (string-copy "hi")) "hi")
                       (hash-remove! h "hi")
                       (not (hash-has-key? h "hi")))))
          (cons (mutable-set "hi")
                (λ (s)
                  (and (set-member? s "hi")
                       (set-add! s "foo")
                       (set-member? s "foo")
                       (set-remove! s "hi")
                       (not (set-member? s "hi"))
                       (equal? (set-first s) "foo"))))
          (cons struct:my-struct
                (λ (chap-st)
                  (struct-type-equal? struct:my-struct chap-st)))
          (cons always-evt
                (λ (e)
                  (equal? (sync e) e)))
          (cons (make-channel)
                (λ (ch)
                  (define thd (thread (λ () (channel-put ch 42))))
                  (equal? (channel-get ch) 42)))
          (cons (make-async-channel)
                (λ (ch)
                  (async-channel-put ch 42)
                  (equal? (async-channel-get ch) 42)))
          (cons (make-continuation-prompt-tag)
                (λ (pt)
                  (call-with-continuation-prompt
                   (λ () (abort-current-continuation pt 42))
                   pt
                   values)))
          (cons (make-continuation-mark-key)
                (λ (mk)
                  (with-continuation-mark mk "foo"
                    (continuation-mark-set-first
                     (current-continuation-marks)
                     mk))))
          (cons (cons 1 2) #f)
          (cons "hello" #f)
          (cons #"hello" #f)
          (cons (regexp "hel*o") #f)
          (cons (pregexp "hel*o") #f)
          (cons (byte-regexp #"hel*o") byte-regex-tester)
          (cons (byte-pregexp #"hel*o") byte-regex-tester)))

  (for ([p (in-list things)])
    (match-define (cons t tester) p)
    (with-chk (['target t])
      (chk
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       #:! #:t (meta-has-key? t 'foo)

       #:do (define t1 (meta-set t 'foo "bar"))
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       #:t (meta-has-key? t1 'foo)
       (meta-ref t1 'foo)  "bar"

       #:do (define t2 (meta-set t1 'foo "baz"))
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       (meta-ref t1 'foo)  "bar"
       (meta-ref t2 'foo)  "baz"

       #:do (define t3 (meta-remove t2 'foo))
       (meta-ref t2 'foo)  "baz"
       #:x (meta-ref t3 'foo) "meta-ref: no value found for key 'foo"

       #:do (define t4 (meta-update t2 'foo string-upcase))
       (meta-ref t4 'foo)  "BAZ"
       #:x (meta-update t3 'bar values)
       "meta-update: no value found for key 'bar"

       #:do (define t5 (meta-update t2 'fuzz add1 1))
       (meta-ref t5 'fuzz)  2

       #:t (if tester (tester t1) (equal? t t1))
       )))

  (chk
   #:! #:t (meta-ref 42 'key #f)
   #:! #:t (meta-has-key? 42 'key)
   (meta-remove 42 'key)  42

   #:x
   (meta-set 42 'key 'val)
   "expected: meta?"

   #:x
   (meta-update 42 'key add1)
   "expected: meta?"
   ))
