#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require racket/class)
(require "tokens.rkt")

(provide make-lexer)

(define-lex-abbrevs
  (list-open #\()
  (list-close #\))
  (line-break (:+ (:or "\r\n" #\newline #\page)))
  (indent-dedent (:seq line-break (:* whitespace)))
  (comma #\,)
  (colon #\:)
  (qt #\')
  (quasiqt #\`)
  (unqt #\~)
  (unqt-splicing "~@")
  (str (:seq #\" (:* (:or (:~ #\") "\\\"")) #\"))
  (line-comment-begin (:or #\; "#!"))
  (line-comment (:seq line-comment-begin (:* (:~ #\newline))))
  (block-comment-begin "#|")
  (block-comment-end "|#")
  (block-comment (:seq block-comment-begin any-string block-comment-end))
  (datum-comment-begin "#;")
  (comment-begin (:or
                   line-comment-begin
                   block-comment-begin
                   datum-comment-begin))
  (datum-start (:&
                 (:~ list-open qt quasiqt unqt comma #\" #\#)
                 (complement unqt-splicing)
                 (complement comment-begin)
                 datum-middle))
  (datum-middle (:~ whitespace))
  (datum-end (:&
               (:~ list-close comma colon #\")
               datum-middle))
  (datum (:or
           (:seq datum-start (:* datum-middle) datum-end)
           (:& datum-start datum-end)
           str)))

(define (file-position-to in pos)
  (file-position in (position-offset pos)))

(define context%
  (class object%
    (super-new)

    (define indent-stack (list 0))
    (field [block-comment-level 0])
    (field [datum-comment-list-nest-level 0])

    (define/public (indent-push! item)
      (set! indent-stack (cons item indent-stack)))

    (define/public (indent-pop!)
      (let ([result (first indent-stack)])
       (set! indent-stack (cdr indent-stack))
       result))

    (define/public (indent-inspect)
      (first indent-stack))

    (define/public (datum-comment-list-nest-level++!)
      (set!
        datum-comment-list-nest-level
        (add1 datum-comment-list-nest-level)))

    (define/public (datum-comment-list-nest-level--!)
      (set!
        datum-comment-list-nest-level
        (sub1 datum-comment-list-nest-level)))

    (define/public (block-comment-level++!)
      (set!
        block-comment-level
        (add1 block-comment-level)))

    (define/public (block-comment-level--!)
      (set!
        block-comment-level
        (sub1 block-comment-level)))))

(define (make-block-comment-lexer context main-lex)
  (letrec ([level-add1!
            (lambda (input-port)
              (send context block-comment-level++!)
              (lex input-port))]

           [level-sub1!
            (lambda (input-port)
              (send context block-comment-level--!)
              (maybe-end-comment input-port))]

           [maybe-end-comment
            (lambda (input-port)
              (if (= 0 (get-field block-comment-level context))
                (main-lex input-port)
                (lex input-port)))]

           [lex
            (lexer-src-pos
              [block-comment-begin (return-without-pos
                                     (level-add1! input-port))]
              [block-comment-end (return-without-pos
                                   (level-sub1! input-port))]
              [any-char (return-without-pos (lex input-port))])])
    lex))

(define (make-line-comment-lexer context main-lex)
  (letrec ([end-comment
            (lambda (input-port lexeme)
              ;; Let the main lexer use this token as well.
              (file-position input-port
                             (- (file-position input-port)
                                (or (string-length lexeme) 1)))
              (main-lex input-port))]

           [lex
            (lexer-src-pos
              [#\newline (return-without-pos
                           (end-comment input-port lexeme))]
              [(eof) (return-without-pos (end-comment input-port lexeme))]
              [any-char (return-without-pos (lex input-port))])])
    lex))

(define (make-datum-comment-lexer context main-lex)
  (letrec ([list-add1!
            (lambda (input-port)
              (send context datum-comment-list-nest-level++!)
              (lex input-port))]

           [list-sub1!
            (lambda (input-port)
              (send context datum-comment-list-nest-level--!)
              (maybe-end-comment input-port))]

           [maybe-end-comment
            (lambda (input-port)
              (if (= 0 (get-field
                         datum-comment-list-nest-level
                         context))
                (main-lex input-port)
                (lex input-port)))]

           [lex
            (lexer-src-pos
              [datum (return-without-pos (maybe-end-comment input-port))]
              [(:or qt quasiqt unqt unqt-splicing whitespace)
               (return-without-pos (lex input-port))]
              [list-open (return-without-pos (list-add1! input-port))]
              [list-close (return-without-pos (list-sub1! input-port))])])
    lex))

(define (make-lexer)
  (letrec ([context (new context%)]

           [lex (lexer-src-pos
                  [indent-dedent (do-indent-dedent context lexeme
                                                   input-port start-pos)]
                  [comma (token-COMMA)]
                  [colon (token-COLON)]
                  [list-open (token-LIST-OPEN)]
                  [list-close (token-LIST-CLOSE)]
                  [qt (token-QUOTE)]
                  [quasiqt (token-QUASIQUOTE)]
                  [unqt (token-UNQUOTE)]
                  [unqt-splicing (token-UNQUOTE-SPLICING)]
                  [line-comment-begin (return-without-pos
                                        (line-comment-lex input-port))]
                  [block-comment-begin (begin
                                         (send context block-comment-level++!)
                                         (return-without-pos
                                           (block-comment-lex input-port)))]
                  [datum-comment-begin (return-without-pos
                                         (datum-comment-lex input-port))]
                  [datum (token-DATUM (read (open-input-string lexeme)))]
                  [whitespace (return-without-pos (lex input-port))]
                  [(eof) (finish-lex context input-port start-pos)])]

           [line-comment-lex (make-line-comment-lexer context lex)]
           [block-comment-lex (make-block-comment-lexer context lex)]
           [datum-comment-lex (make-datum-comment-lexer context lex)])

    (lambda (in)
      (let ([result (lex in)])
       ; (displayln (token-name (position-token-token result)))
       result))))


(define (finish-lex context input-port start-pos)
  (if (= 0 (send context indent-inspect))
    (token-EOF)
    (do-indent-dedent context "" input-port start-pos)))


(define (do-indent-dedent context indent input-port start-pos)
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
       (token-STATEMENT-BREAK)]

      [(> indent-len stack-top)
       (begin
         (send context indent-push! indent-len)
         (token-INDENT))]

      [(< indent-len stack-top)
       (begin
         (let ([tok (do-dedent context)])
           ;; Check to make sure we have not dropped below the next dedent
           ;; level.
           (let ([stack-top (send context indent-inspect)])
             (cond
               [(> indent-len stack-top)
                (raise (string-append "When dedenting the level must "
                                      "match an existing stack level."))]

               [(= indent-len stack-top)
                tok]

               ['else
                (begin
                  ;; Put the file's position back to where it was before
                  ;; reading this item so we will gather as many dedents as
                  ;; we need.
                  (file-position-to input-port start-pos)
                  tok)]))))])))


(define (do-dedent context)
  (send context indent-pop!)
  (token-DEDENT))


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
