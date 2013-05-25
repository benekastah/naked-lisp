#lang racket

(require parser-tools/yacc)
(require "tokens.rkt")

(define naked-racket-parser
  (parser
    (start expr)
    (end EOF)
    (error void)
    (tokens grouping
            grouping-greedy
            structures
            racket
            quoting
            file)
    (prec (left SEGMENT-BREAK)
          (right (INDENT)))
    (grammar
      (ls
        [(LIST-OPEN LIST-CLOSE)
         '()]

        [(LIST-OPEN ls-contents LIST-CLOSE)
         $2])

      (ls-contents
        [(expr)
         (list $1)]

        [(ls-contents expr)
         `(,@$1 ,$2)])

      (statement-part
        [(ls-contents)
         $1]

        [(statement-part INDENT statement-part)
         `(,@$1 ,$3)]

        [(statement-part SEGMENT-BREAK statement-part)]
        )

      (expr
        []
        )
      )
    )
  )
