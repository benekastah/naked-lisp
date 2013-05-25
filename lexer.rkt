#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require racket/class)
(require "tokens.rkt")

(provide naked-racket-lex
         naked-racket-lex-string
         naked-racket-lex-file
         naked-racket-do-lex
         make-lexer)

(define-lex-abbrevs
  (list-open #\()
  (list-close #\))
  (line-break (:+ (:or "\r\n" #\newline #\page)))
  (indent-dedent (:seq line-break (:* whitespace)))
  (segment-break #\,)
  (indent #\:)
  (unqt #\~)
  (unqt-spl "~@")
  (datum (:+ (:~ list-open
                 list-close
                 segment-break
                 indent
                 unqt
                 unqt-spl
                 whitespace))))

(define context%
  (class object%
    (super-new)

    (define indent-stack (list 0))

    (define/public (indent-push! item)
      (set! indent-stack (cons item indent-stack)))

    (define/public (indent-pop!)
      (let ([result (first indent-stack)])
       (set! indent-stack (cdr indent-stack))
       result))

    (define/public (indent-inspect)
      (first indent-stack))))

(define (naked-racket-do-lex lex in tokens)
  (if (eof-object? (peek-char in))
    (reverse (flatten tokens))
    (let ([result (lex in)])
     (if (void? result)
       (naked-racket-do-lex lex in tokens)
       (naked-racket-do-lex lex in (cons result tokens))))))

(define (naked-racket-lex in)
  (naked-racket-do-lex
    (make-lexer) in '()))

(define (naked-racket-lex-string str)
  (naked-racket-lex (open-input-string str)))

(define (naked-racket-lex-file f)
  (naked-racket-lex (open-input-file f)))


(define (make-lexer)
  (letrec ([context (new context%)]
           [lex (lexer
                  [indent-dedent (do-indent-dedent context lexeme)]
                  [segment-break (token-SEGMENT-BREAK)]
                  [indent (token-INDENT)]
                  [list-open (token-LIST-OPEN)]
                  [list-close (token-LIST-CLOSE)]
                  [unqt (token-UNQUOTE)]
                  [unqt-spl (token-UNQUOTE-SPLICING)]
                  [datum (token-DATUM (read (open-input-string lexeme)))]
                  ;; Ignore whitespace
                  [whitespace (lex input-port)]
                  [(eof) (list 'eof (do-indent-dedent context ""))])])
    lex))

(define (do-indent-dedent context indent)
  ;; From http://docs.python.org/3/reference/lexical_analysis.html#indentation
  ;; Before the first line of the file is read, a single zero is pushed
  ;; on the stack; this will never be popped off again. The numbers
  ;; pushed on the stack will always be strictly increasing from bottom
  ;; to top.  At the beginning of each logical line, the line's
  ;; indentation level is compared to the top of the stack. If it is
  ;; equal, nothing happens.  If it is larger, it is pushed on the
  ;; stack, and one INDENT token is generated. If it is smaller, it must
  ;; be one of the numbers occurring on the stack; all numbers on the
  ;; stack that are larger are popped off, and for each number popped
  ;; off a DEDENT token is generated. At the end of the file, a DEDENT
  ;; token is generated for each number remaining on the stack that is
  ;; larger than zero.
  (let ([indent-len (indent-length indent)]
        [stack-top (send context indent-inspect)])
    (cond
      [(equal? indent-len stack-top)
       (token-SEGMENT-BREAK-GREEDY)]

      [(> indent-len stack-top)
       (begin
         (send context indent-push! indent-len)
         (token-INDENT-GREEDY))]

      [(< indent-len stack-top)
       (let loop ([tkns (list (do-dedent context))]
                  [stack-top (send context indent-inspect)])
         (cond
           [(equal? indent-len stack-top)
            tkns]

           [(< indent-len stack-top)
            (loop (cons (do-dedent context) tkns)
                  (send context indent-inspect))]

           [(> indent-len stack-top)
            (raise (string-append "When dedenting the level must "
                                  "match an existing stack level."))]))])))

(define (do-dedent context)
  (send context indent-pop!)
  (token-DEDENT-GREEDY))

(define (indent-length indent)
  ;; It is expected that indent is properly formed, meaning that it
  ;; (optionally) begins with one or more newlines and has zero or more
  ;; whitespace characters that follow. This function will ignore all
  ;; newlines and form feeds it encounters without regard to their placement
  ;; in the indent string.
  (let loop ([chars (string->list indent)]
             [indent-len 0])
    (if (empty? chars)
      indent-len
      (let ([indent-char (first chars)]
            [chars-rest (rest chars)])
        (cond
          ;; Ignore these characters
          [(or (eq? indent-char #\newline)
               (eq? indent-char #\return)
               (eq? indent-char #\page))
           (loop chars-rest indent-len)]

          ;; From http://docs.python.org/3/reference/lexical_analysis.html#indentation
          ;; First, tabs are replaced (from left to right) by one to eight
          ;; spaces such that the total number of characters up to and
          ;; including the replacement is a multiple of eight (this is
          ;; intended to be the same rule as used by Unix). The total number
          ;; of spaces preceding the first non-blank character then
          ;; determines the line's indentation. Indentation cannot be split
          ;; over multiple physical lines using backslashes; the whitespace
          ;; up to the first backslash determines the indentation.
          [(equal? indent-char #\tab)
           (let ([quo (/ indent-len 8)])
             (loop chars-rest
                   (+ indent-len
                      (* (- (ceiling quo) quo) 8))))]

          ;; All other forms of whitespace count as taking a single indent
          ;; column.
          ['else
           (loop chars-rest
                 (add1 indent-len))])))))
