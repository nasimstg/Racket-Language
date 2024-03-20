#lang racket

(provide (rename-out [mb #%module-begin]
                     [ti #%top-interaction]))

(require "cs450js-lang+rec.rkt" 
         syntax/parse/define)

(define eval450js (compose run450js parse450js))

(define-simple-macro (mb e ...)
  (#%module-begin (eval450js 'e) ...))

(define-simple-macro (ti . e)
  (eval450js 'e))