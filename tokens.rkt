#lang racket

(require parser-tools/lex)

(provide (all-defined-out))

(define-empty-tokens grouping (CHILD SIBLING))
(define-empty-tokens grouping-ws (INDENT
                                   DEDENT
                                   STATEMENT-BREAK))
(define-empty-tokens structures (LIST-OPEN LIST-CLOSE))
(define-tokens racket (DATUM))
(define-empty-tokens quoting (UNQUOTE UNQUOTE-SPLICING))
(define-empty-tokens file (EOF))
