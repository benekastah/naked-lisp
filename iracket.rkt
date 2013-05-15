#lang racket/base

(port-count-lines-enabled #t)

(provide (rename-out [i-read read])
         (rename-out [i-read-syntax read-syntax]))

(define (datum->syntax-copy ctx datum stx)
  (datum->syntax ctx datum
                 (list
                   (syntax-source stx)
                   (syntax-line stx)
                   (syntax-column stx)
                   (syntax-position stx)
                   (syntax-span stx))))

(define (is-child-of stx-a stx-b)
  (and
    (line-is-incremented stx-a stx-b)
    (> (syntax-column stx-a) (syntax-column stx-b))))

(define (is-sibling-of stx-a stx-b)
  (or
    (= (syntax-line stx-a) (syntax-line stx-b))
    (= (syntax-column stx-a) (syntax-column stx-b))))

(define (line-is-incremented stx-a stx-b)
  (> (syntax-line stx-a) (syntax-line stx-b)))

(define (listify x)
  (if (list? x)
    x
    (list x)))

(define (syntax-append ls item)
  (let* {[*ls (listify (syntax->datum ls))]
   [**ls (append *ls (syntax->datum item))]}
     (datum->syntax-copy #f **ls ls)))

(define (i-read in)
  (syntax->datum (i-read-syntax #f in)))

(define-syntax maybe-list
  (syntax-rules ()
    [(maybe-list x) x]
    [(maybe-list x y ...)
      (x y ...)]))

(define (naked-racket in tokens))

(define naked-racket%
  (class object%
    (super-new)
    (init-field in)
    (init-field program)
    (init-field last-syntax)
    (public (read-syntax)
      (if (port-closed? in)
        program
        (let* ([stx (read-syntax src in)]
               [stx (datum->syntax-copy
                       #f
                       `(maybe-list ,(datum->syntax stx))
                       stx)])
          (send (send this process-syntax stx) read-syntax))))

    (define (process-syntax stx)
      (let ([*program (send this get-program)]
            [*last-syntax (send this get-last-syntax)])
        (cond
          ((is-child-of stx last-syntax)
           (set! last-syntax (syntax-append last-syntax stx)))
          ((is-sibling-of stx last-syntax)
           '()))))

    (define (get-program)
      (or program
          (datum->syntax #f '(begin))))

    (define (get-last-syntax)
      (or last-syntax (datum->syntax #f '()
                                     (list #f 0 0 #f #f))))
    ;;
    ))

(define (naked-read-syntax src in)
  (naked-racket in))

