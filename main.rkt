#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [meta? predicate/c]
  [meta-ref (->* (any/c any/c) (failure-result/c) any/c)]
  [meta-has-key? (-> any/c any/c boolean?)]
  [meta-set
   (->i
    ([obj (st) (if (unsupplied-arg? st) meta? any/c)]
     [key any/c]
     [val any/c])
    (#:struct-type [st struct-type?])
    [res any/c])]
  [meta-update
   (->i
    ([obj (st) (if (unsupplied-arg? st) meta? any/c)]
     [key any/c]
     [update (-> any/c any/c)])
    ([fail failure-result/c]
     #:struct-type [st struct-type?])
    [res any/c])]
  [meta-remove
   (->* (any/c any/c) (#:struct-type struct-type?) any/c)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/async-channel
         racket/list
         racket/match
         racket/set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define EMPTY null)
(define UNDEFINED (gensym))
(define MISSING (gensym))

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
        regexp?
        pregexp?
        byte-regexp?
        byte-pregexp?))

(define meta?
  (flat-named-contract
   'meta?
   (or/c can-chaperone? cannot-chaperone?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general functions

(define (meta-ref e k [failure-result UNDEFINED])
  (define meta-map
    (if (cannot-chaperone? e)
        (meta-ref/no-chaperone e)
        (meta-ref/chaperone e)))
  (define kv-pair (and meta-map (assoc k meta-map)))
  (cond
    [kv-pair (cdr kv-pair)]
    [(eq? failure-result UNDEFINED)
     (error 'meta-ref "no value found for key ~v" k)]
    [(procedure? failure-result) (failure-result)]
    [else failure-result]))

(define (meta-has-key? e k)
  (define v (meta-ref e k MISSING))
  (not (eq? v MISSING)))

(define (meta-set e k v #:struct-type [st #f])
  (if (or st (can-chaperone? e))
      (meta-set/chaperone e k v st)
      (meta-set/no-chaperone e k v)))

(define (meta-update e k f
                     [failure-result UNDEFINED]
                     #:struct-type [st #f])
  (define failure-result*
    (if (eq? failure-result UNDEFINED)
        (λ () (error 'meta-update "no value found for key ~v" k))
        failure-result))
  (define v (meta-ref e k failure-result*))
  (meta-set e k (f v) #:struct-type st))

(define (meta-remove e k #:struct-type [st #f])
  (cond
    [(or st (can-chaperone? e)) (meta-remove/chaperone e k #f st)]
    [(cannot-chaperone? e) (meta-remove/no-chaperone e k #f)]
    [else e]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chaperone meta

(define-values (impersonator-prop:meta -meta? -meta-ref)
  (make-impersonator-property 'meta))

(define (meta-ref/chaperone e)
  (-meta-ref e #f))

(define ((make-meta-set/chaperone set/remove) e k v st)
  (define old-map (-meta-ref e EMPTY))
  (define new-map (set/remove old-map k v))
  (cond
    [st (chaperone-struct e st impersonator-prop:meta new-map)]
    [(struct? e)
     (define-values (struct-type skipped?) (struct-info e))
     (chaperone-struct e struct-type impersonator-prop:meta new-map)]
    [(procedure? e)
     (chaperone-procedure e #f impersonator-prop:meta new-map)]
    [(vector? e)
     (chaperone-vector e #f #f impersonator-prop:meta new-map)]
    [(box? e)
     (chaperone-box e (λ (_ x) x) (λ (_ x) x) impersonator-prop:meta new-map)]
    [(hash? e)
     (define (ref-proc h k) (values k (λ (h _ v) v)))
     (define (set-proc h k v) (values k v))
     (define (remove-proc h k) k)
     (define (key-proc h k) k)
     (chaperone-hash e
                     ref-proc set-proc remove-proc key-proc
                     impersonator-prop:meta new-map)]
    [(or (set? e) (set-mutable? e) (set-weak? e))
     (chaperone-hash-set e #f #f #f #f
                         impersonator-prop:meta new-map)]
    [(struct-type? e)
     (define struct-info-proc values)
     (define make-constructor-proc values)
     (define (guard-proc . args)
       (apply values (drop-right args 1)))
     (chaperone-struct-type e
                            struct-info-proc
                            make-constructor-proc
                            guard-proc
                            impersonator-prop:meta new-map)]
    [(evt? e)
     (define (proc e) (values e values))
     (chaperone-evt e proc impersonator-prop:meta new-map)]
    [(channel? e)
     (define (get-proc c) (values c values))
     (define (put-proc c v) v)
     (chaperone-channel e
                        get-proc put-proc
                        impersonator-prop:meta new-map)]
    [(async-channel? e)
     (chaperone-async-channel e values values
                              impersonator-prop:meta new-map)]
    [(continuation-prompt-tag? e)
     (chaperone-prompt-tag e
                           values values
                           impersonator-prop:meta new-map)]
    [(continuation-mark-key? e)
     (chaperone-continuation-mark-key e
                                      values values
                                      impersonator-prop:meta new-map)]))

(define (-meta-set m k v)
  (define kv* (cons k v))
  (let go ([m m])
    (match m
      ['() (list kv*)]
      [(cons (cons (== k) _) rst)
       (cons kv* rst)]
      [(cons kv rst)
       (cons kv (go rst))])))

(define (-meta-remove m k v)
  (let go ([m m])
    (match m
      ['() null]
      [(cons (cons (== k) _) rst)
       rst]
      [(cons kv rst)
       (cons kv (go rst))])))

(define (copy e)
  (cond
    [(pair? e) (cons (car e) (cdr e))]
    [(pregexp? e) (pregexp (object-name e))]
    [(regexp? e) (regexp (object-name e))]
    [(byte-pregexp? e) (byte-pregexp (object-name e))]
    [(byte-regexp? e) (byte-regexp (object-name e))]))

(define meta-set/chaperone (make-meta-set/chaperone -meta-set))
(define meta-remove/chaperone (make-meta-set/chaperone -meta-remove))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-chaperone metadata

(define immutable-metas (make-weak-hasheq))

(define (meta-ref/no-chaperone e)
  (hash-ref immutable-metas e #f))

(define ((make-meta-set/no-chaperone set/remove) e k v)
  (define old-map (hash-ref immutable-metas e EMPTY))
  (define new-map (set/remove old-map k v))
  (define e* (copy e))
  (hash-set! immutable-metas e* new-map)
  e*)

(define meta-set/no-chaperone (make-meta-set/no-chaperone -meta-set))
(define meta-remove/no-chaperone (make-meta-set/no-chaperone -meta-remove))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk
           racket/match)

  (struct my-struct (x y z) #:transparent)
  (struct my-opaque-struct (x y z))

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
    (list (list (λ (x) x)
                (λ (p) (= (p 42) 42)))
          (list (my-struct 1 2 3)
                (λ (s) (= (my-struct-x s) 1)))
          (list (my-opaque-struct 1 2 3)
                (λ (s) (= (my-opaque-struct-x s) 1))
                struct:my-opaque-struct)
          (list (vector 1 2 3)
                (λ (v)
                  (vector-set! v 1 42)
                  (and (= (vector-ref v 0) 1)
                       (= (vector-ref v 1) 42))))
          (list (vector-immutable 1 2 3)
                (λ (v)
                  (and (= (vector-ref v 0) 1)
                       (= (vector-ref v 1) 2))))
          (list (box 1)
                (λ (b)
                  (and (= (unbox b) 1)
                       (set-box! b 42)
                       (= (unbox b) 42))))
          (list (box-immutable 1)
                (λ (b) (= (unbox b) 1)))
          (list (make-hash `(["hi" . 2]))
                (λ (h)
                  (hash-ref-key h (string-copy "hi"))
                  (and (= (hash-ref h "hi") 2)
                       (hash-set! h "hi" 42)
                       (= (hash-ref h "hi") 42)
                       (equal? (hash-ref-key h (string-copy "hi")) "hi")
                       (hash-remove! h "hi")
                       (not (hash-has-key? h "hi")))))
          (list (hash "hi" 2)
                (λ (h) (= (hash-ref h "hi") 2)))
          (list (mutable-set "hi")
                (λ (s)
                  (and (set-member? s "hi")
                       (set-add! s "foo")
                       (set-member? s "foo")
                       (set-remove! s "hi")
                       (not (set-member? s "hi"))
                       (equal? (set-first s) "foo"))))
          (list (set "hi")
                (λ (s) (set-member? s "hi")))
          (list struct:my-struct
                (λ (chap-st)
                  (struct-type-equal? struct:my-struct chap-st)))
          (list always-evt
                (λ (e)
                  (equal? (sync e) e)))
          (list (make-channel)
                (λ (ch)
                  (define thd (thread (λ () (channel-put ch 42))))
                  (equal? (channel-get ch) 42)))
          (list (make-async-channel)
                (λ (ch)
                  (async-channel-put ch 42)
                  (equal? (async-channel-get ch) 42)))
          (list (make-continuation-prompt-tag)
                (λ (pt)
                  (call-with-continuation-prompt
                   (λ () (abort-current-continuation pt 42))
                   pt
                   values)))
          (list (make-continuation-mark-key)
                (λ (mk)
                  (with-continuation-mark mk "foo"
                    (continuation-mark-set-first
                     (current-continuation-marks)
                     mk))))
          (list (cons 1 2) #f)
          (list (regexp "hel*o") #f)
          (list (pregexp "hel*o") #f)
          (list (byte-regexp #"hel*o") byte-regex-tester)
          (list (byte-pregexp #"hel*o") byte-regex-tester)))

  (for ([p (in-list things)])
    (define-values (t tester st)
      (match p
        [(list t tester) (values t tester #f)]
        [(list t tester st) (values t tester st)]))
    (with-chk (['target t])
      (chk
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       #:! #:t (meta-has-key? t 'foo)

       #:do (define t1 (meta-set t 'foo "bar" #:struct-type st))
       #:t (equal? t t1)
       #:t (chaperone-of? t1 t)
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       #:t (meta-has-key? t1 'foo)
       (meta-ref t1 'foo)  "bar"

       #:do (define t2 (meta-set t1 'foo "baz" #:struct-type st))
       #:x (meta-ref t 'foo) "no value found for key 'foo"
       (meta-ref t1 'foo)  "bar"
       (meta-ref t2 'foo)  "baz"

       #:do (define t3 (meta-remove t2 'foo #:struct-type st))
       (meta-ref t2 'foo)  "baz"
       #:x (meta-ref t3 'foo) "meta-ref: no value found for key 'foo"

       #:do (define t4 (meta-update t2 'foo string-upcase #:struct-type st))
       (meta-ref t4 'foo)  "BAZ"
       #:x (meta-update t3 'bar values #:struct-type st)
       "meta-update: no value found for key 'bar"

       #:do (define t5 (meta-update t2 'fuzz add1 1 #:struct-type st))
       (meta-ref t5 'fuzz)  2

       #:t (if tester (tester t1) (equal? t t1))
       ))))

(module+ test
  (require (prefix-in c: (submod "..")))

  (chk
   #:! #:t (c:meta-ref 42 'key #f)
   #:! #:t (c:meta-has-key? 42 'key)
   (c:meta-remove 42 'key)  42

   #:x
   (c:meta-set 42 'key 'val)
   "expected: meta?"

   #:x
   (c:meta-update 42 'key add1)
   "expected: meta?"
   ))
