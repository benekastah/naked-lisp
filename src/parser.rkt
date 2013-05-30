#lang racket

(require (only-in parser-tools/lex
                  position-line
                  position-col))
(require parser-tools/yacc)
(require "tokens.rkt")

(provide naked-racket-parser)

(define (handle-error tok-ok? tok-name tok-value start-pos end-pos)
  (raise
    (format "ParseError: ~a at line ~a, col ~a"
            (cond
              [(not tok-ok?)
               (format "invalid token: ~a" tok-name)]

              [(not tok-value)
               (format "unexpected token ~a" tok-name)]

              ['else
               (format "~a => ~a" tok-name tok-value)])
            (position-line start-pos)
            (position-col start-pos))))


(define (n->list x)
  (if (list? x)
    x
    (list x)))


(define naked-racket-parser
  (parser
    (start program)
    (end EOF)
    (src-pos)
    (error handle-error)
    (tokens grouping
            grouping-ws
            structures
            racket
            quoting
            file)
    (precs (right INDENT COLON COMMA STATEMENT-BREAK))
    (grammar
      (program
        [(statement-list) $1]
        [(statement-breaks program) $2])

      (ls-contents
        [(expr)
         (list $1)]

        [(ls-contents expr)
         `(,@$1 ,$2)])

      (ls
        [(LIST-OPEN LIST-CLOSE)
         '()]

        [(LIST-OPEN ls-contents LIST-CLOSE)
         $2])

      (quoted
        [(QUOTE expr) (list 'quote $2)]
        [(QUASIQUOTE expr) (list 'quasiquote $2)]
        [(UNQUOTE expr) (list 'unquote $2)]
        [(UNQUOTE-SPLICING expr) (list 'unquote-splicing $2)])

      (expr
        [(DATUM) $1]
        [(quoted) $1]
        [(ls) $1])

      (segment
        [(expr) $1]

        [(list-segment expr)
         `(,@$1 ,$2)])

      (list-segment
        [(segment) (n->list $1)])

      (statement-breaks
        [(STATEMENT-BREAK) '|,|]
        [(statement-breaks STATEMENT-BREAK) $1])

      (sibling-list
        [(unterminated-statement) (list $1)]

        [(sibling-list COMMA unterminated-statement)
         `(,@$1 ,$3)])

      (unterminated-statement
        [(segment) $1]

        [(list-segment COLON sibling-list)
         `(,@$1 ,@$3)])

      (statement
        [(unterminated-statement statement-breaks) $1]

        [(unterminated-statement INDENT
                                 statement-list-last-unterminated
                                 DEDENT)
         `(,@$1 ,@$3)]

        [(unterminated-statement INDENT
                                 statement-list
                                 DEDENT)
         `(,@$1 ,@$3)])

      (statement-list
        [(statement)
         (list $1)]

        [(statement-list statement)
         `(,@$1 ,$2)])

      (statement-list-last-unterminated
        [(unterminated-statement) (list $1)]

        [(statement-list unterminated-statement)
         `(,@$1 ,$2)]))))
