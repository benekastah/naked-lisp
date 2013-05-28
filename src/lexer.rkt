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
  (sibling-expr #\,)
  (child-expr #\:)
  (unqt #\~)
  (unqt-spl "~@")
  (datum (:+ (:~ list-open
                 list-close
                 sibling-expr
                 child-expr
                 whitespace))))

(define context%
  (class object%
    (super-new)

    (define indent-stack (list 0))
    (field [backtokens '()])

    (define/public (indent-push! item)
      (set! indent-stack (cons item indent-stack)))

    (define/public (indent-pop!)
      (let ([result (first indent-stack)])
       (set! indent-stack (cdr indent-stack))
       result))

    (define/public (indent-inspect)
      (first indent-stack))

    (define/public (backtokens-pop!)
      (let ([result (first backtokens)])
       (set! backtokens (rest backtokens))
       result))

    (define/public (set-backtokens-pop! toks)
      (let ([result (first toks)])
       (set! backtokens (rest toks))
       result))))

(define (make-lexer)
  (letrec ([context (new context%)]
           [ignore-token (lambda (input-port)
                           (let ([pos-tok (lex input-port)])
                             (position-token-token pos-tok)))]
           [lex (lexer-src-pos
                  [indent-dedent (do-indent-dedent context lexeme
                                                   input-port start-pos)]
                  [sibling-expr (token-SIBLING)]
                  [child-expr (token-CHILD)]
                  [list-open (token-LIST-OPEN)]
                  [list-close (token-LIST-CLOSE)]
                  [unqt (token-UNQUOTE)]
                  [unqt-spl (token-UNQUOTE-SPLICING)]
                  [datum (token-DATUM (read (open-input-string lexeme)))]
                  [whitespace (ignore-token input-port)]
                  [(eof) (finish-lex context input-port start-pos)])])
    (lambda (in . args)
      (let ([result (apply lex in args)])
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
                  (file-position input-port (position-offset start-pos))
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
