#lang scribble/manual

@require[@for-label[meta
                    racket/base
                    racket/contract]
         scribble/example]

@(define evaluator
  (make-base-eval
    '(require meta)))

@title{Metadata}
@author{Cameron Moy}

@defmodule[meta]

@margin-note{
This library is under development;
compatibility may not be maintained.
}

This package provides a mechanism for associating
arbitrary metadata with values.
It's inspired by Clojure's
@link["https://clojure.org/reference/metadata"]{metadata}.
Attaching metadata to a value does not affect
@racket[equal?] comparisons,
but it may affect @racket[eq?] comparisons.
The following datatypes don't support metadata:
booleans, numbers, characters, symbols,
keywords, mutable pairs and lists, void,
undefined, opaque structures.

Under the hood,
it uses
@secref["Impersonator_Properties" #:doc '(lib "scribblings/reference/reference.scrbl")]
to store metadata for datatypes that support
chaperones.
For others,
it copies the value
and uses a weak mutable hash table
(compared with @racket[eq?])
to store the metadata.

@defproc[(meta? [v any/c]) boolean?]{
  Returns whether @racket[v] can have metadata.
  @examples[#:eval evaluator
    (meta? (vector 1 2 3))
    (meta? 42)]
}

@defproc[(meta-ref [v any/c]
                   [key any/c]
                   [failure-result
                    failure-result/c
                    (lambda ()
                      (raise (make-exn:fail:contract ....)))])
         any/c]{
  Returns the value for @racket[key] in the metadata for @racket[v].
  If no value is found for @racket[key], then @racket[failure-result]
  determines the result in the same way as @racket[hash-ref].
  @examples[#:eval evaluator
    (define v (meta-set (vector) 'key "value"))
    (meta-ref v 'key)]
}

@defproc[(meta-has-key? [v any/c] [key any/c]) boolean?]{
  Returns @racket[#t] if the metadata for @racket[v]
  contains a value for the given key,
  @racket[#f] otherwise.
  @examples[#:eval evaluator
    (define v (meta-set (cons 1 2) 'key "value"))
    (meta-has-key? v 'key)]
}

@defproc[(meta-set [v meta?]
                   [key any/c]
                   [val any/c])
         meta?]{
  Returns a value that is @racket[equal?] to @racket[v],
  but where the metadata maps @racket[key] to @racket[val],
  overwriting any existing mapping for @racket[key].
  @examples[#:eval evaluator
    (define func (meta-set (lambda () 2) 'key "value"))
    (func)
    (meta-ref func 'key)]
}

@defproc[(meta-update [v meta?]
                      [key any/c]
                      [updater (-> any/c any/c)]
                      [failure-result
                       failure-result/c
                       (lambda ()
                         (raise (make-exn:fail:contract ....)))])
         meta?]{
  Returns a value that is @racket[equal?] to @racket[v],
  but with the metadata updated in the same way as @racket[hash-update].
  @examples[#:eval evaluator
    (define b (meta-update (box 2) 'key add1 1))
    (meta-ref b 'key)]
}

@defproc[(meta-remove [v any/c] [key any/c]) any/c]{
  Removes any existing mapping for @racket[key] in the metadata
  for @racket[v].
  @examples[#:eval evaluator
    (define s (meta-set "hi" 'key "value"))
    (meta-has-key? (meta-remove s 'key) 'key)]
}
