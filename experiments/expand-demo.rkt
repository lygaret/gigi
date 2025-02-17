#lang racket/base

;; pico expander from
;; github.com/mflatt/expander

(require "expand.rkt")

(define (expand-expression e)
  (expand (namespace-syntax-introduce (datum->syntax e))))

(define (compile+eval-expression e)
  (define c (compile (expand-expression e)))
  (values c (eval c)))

(define (eval-expression e #:check [check-val #f])
  (define-values (_ v) (compile+eval-expression e))
  (when check-val
    (unless (equal? v check-val)
      (error "check failed")))
  v)

(module+ test
  (compile+eval-expression
   '(lambda (x) x)))

(define (add-let e)
  `(let-syntax ([let (lambda (stx)
                       (datum->syntax
                        (cons
                         (list (quote-syntax lambda)
                               (map car (car (cdr stx)))
                               (car (cdr (cdr stx))))
                         (map (lambda (b)
                                (car (cdr b)))
                              (car (cdr stx))))))])
     ,e))

(module+ test
  (compile+eval-expression
   (add-let
    '(lambda (x)
       (let ([y x])
         y))))
  
  (compile+eval-expression
   `(lambda (x)
      (let-syntax ([y (lambda (stx) (quote-syntax '7))])
        (y))))

  (compile+eval-expression
   `(lambda (x y z)
      (let-syntax ([y (lambda (stx) (quote-syntax z))])
        (y))))
  
  (compile+eval-expression
   (add-let
    '(let ([z '0])
       (let-syntax ([m (lambda (stx) (car (cdr stx)))])
         (let ([x '5]
               [y (lambda (z) z)])
           (let ([z '10])
             (list z (y x) (m '10))))))))
  
  "non capturing"
  (eval-expression
   #:check 'x-1
   (add-let
    '(let ([x 'x-1])
       (let-syntax ([m (lambda (stx) (quote-syntax x))])
         (let ([x 'x-3])
           (m))))))
  
  "non-capturing"
  (eval-expression
   #:check 'x-3
   (add-let
    '(let ([x 'x-1])
       (let-syntax ([m (lambda (stx)
                         (datum->syntax
                          (list (quote-syntax let)
                                (list (list (quote-syntax x)
                                            (quote-syntax 'x-2)))
                                (car (cdr stx)))))])
         (let ([x 'x-3])
           (m x))))))
  
  "non-transformer binding misuse"
  (with-handlers ([exn:fail? (lambda (exn)
                               (unless (regexp-match? #rx"illegal use of syntax"
                                                      (exn-message exn))
                                 (error "wrong error"))
                               'illegal-use)])
    (expand (namespace-syntax-introduce
             (datum->syntax '(let-syntax ([v 1])
                               v))))
    (error "shouldn't get here")))
