#lang racket

(require parser-tools/lex)

(provide (all-defined-out))

(define-empty-tokens grouping (INDENT SEGMENT-BREAK))
(define-empty-tokens grouping-greedy (INDENT-GREEDY
                                       DEDENT-GREEDY
                                       SEGMENT-BREAK-GREEDY))
(define-empty-tokens structures (LIST-OPEN LIST-CLOSE))
(define-tokens racket (DATUM))
(define-empty-tokens quoting (UNQUOTE UNQUOTE-SPLICING))
(define-empty-tokens file (EOF))
