#lang racket

(require "../src/lexer.rkt")
(require "../src/parser.rkt")
(require syntax/strip-context)

(provide (rename-out [naked-read read]
                     [naked-read-syntax read-syntax]))

(define (lex-this lexer input) (lambda () (lexer input)))

(define (parse in)
  (naked-racket-parser (lex-this (make-lexer) in)))

(define (naked-read-syntax src in)
  (strip-context #`(module anonymous racket
                           #,@(parse in))))

(define (naked-read in)
  (syntax->datum (naked-read-syntax #f in)))
