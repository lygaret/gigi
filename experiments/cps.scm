;; run in guile

;; based on the cps transformer
;; https://matt.might.net/articles/cps-conversion/

(import (srfi srfi-1))
(import (srfi srfi-9))
(import (srfi srfi-9 gnu))
(import (ice-9 match))
(import (ice-9 format))
(import (ice-9 pretty-print))
(import (ice-9 receive))

;;; 1. Reader - just use Scheme's built-in reader

(define (read-expr)
  (read))

;;; 2. CPS Transform - converts direct style to continuation-passing style

(define (genpsym prefix)
  (let ([counter 0])
    (lambda (x)
      (if (number? x)
          (set! counter x)
          (begin
            (set! counter (+ 1 counter))
            (string->symbol
             (format #f "~a$~a$~3,,,'0@a" prefix x counter)))))))

(define genusym (genpsym "u"))
(define genksym (genpsym "k"))

(define (cps-transform expr kont)

  (define (cps-atomic? expr)
    (match expr
      [(? symbol?)  #t]
      [(? number?)  #t]
      [(? string?)  #t]
      [`(quote . _) #t]
      [else #f]))

  (define (cps-primitive? expr)
    (and (symbol? expr)
         (member expr '(+ - / * = print))))

  (define (cps-M expr)
    (match expr
      ;; function definition
      [`(lambda ,(params ...) . ,body)
       (let ((k-param (genksym "kont")))
         `(lambda (,k-param ,@params) ,@(cps-C body k-param)))]

      [else expr]))

  (define (cps-C expr c)
    (match expr
      [(? cps-atomic?) `(%return-kappa ,c ,(cps-M expr))]
      [`(lambda . ,_)  `(%return-kappa ,c ,(cps-M expr))]

      ;; walk begins, throwing away results until the last one
      [`(begin ,first . ,rest)
       (if (null? rest)
           (cps-C first c)
           (cps-K first (lambda (_)
                          ;; todo; is it ok that this retval is thrown away?
                          (cps-C `(begin ,@rest) c))))]

      ;; conditionls 
      [`(if ,test-expr ,then-expr ,else-expr)
       (let ((k-param (genksym "kont")))
         `(%call-lambda (lambda (,k-param)
             ,(cps-K test-expr (lambda (test-result)
                                 `(%if ,test-result
                                      ,(cps-C then-expr k-param)
                                      ,(cps-C else-expr k-param)))))
           ,c))]

      ;; function application
      [`(,f-expr . ,arg-exprs) ; a not empty pair
       (if (cps-primitive? f-expr)
           (cps-Ks arg-exprs (lambda (args-results)
                               `(%call-prim ,f-expr ,c ,@args-results)))
           (let ((f-result (genusym "f-result")))
             (cps-K f-expr (lambda (f-result)
                             (cps-Ks arg-exprs (lambda (args-results)
                                                 `(%call-lambda ,f-result ,c ,@args-results)))))))]))

  (define (cps-K expr k)
    (match expr
      [(? cps-atomic?) (k (cps-M expr))]
      [`(lambda . ,_)  (k (cps-M expr))]

      ;; walk begins, throwing away results until the last one
      [`(begin ,first . ,rest)
       (if (null? rest)
           (cps-K first k)
           (cps-K first (lambda (_)
                          (cps-K `(begin ,@rest) k))))]

      ;; conditionls 
      [`(if ,test-expr ,then-expr ,else-expr)
       (let* ((rv      (genusym "retval"))
              (k-param (genksym "kont"))
              (cont `(kappa (,rv) ,(k rv))))
         `(%call-lambda (lambda (,k-param)
             ,(cps-K test-expr (lambda (test-result)
                            `(%if ,test-result
                                 ,(cps-C then-expr k-param)
                                 ,(cps-C else-expr k-param)))))
           ,cont))]

      ;; function application
      [`(,f-expr . ,arg-exprs) ; a not empty pair
       (let* ((rv    (genusym "retval"))
              (cont `(kappa (,rv) ,(k rv))))
         (if (cps-primitive? f-expr)
             (cps-Ks arg-exprs (lambda (args-results)
                                 `(%call-prim ,f-expr ,cont ,@args-results)))
             (cps-K f-expr (lambda (f-result)
                             (cps-Ks arg-exprs (lambda (args-results)
                                                 `(%call-lambda ,f-result ,cont ,@args-results)))))))]))

  (define (cps-Ks seq build-k)
    (cond
     [(null? seq) (build-k '())]
     [(pair? seq) (cps-K (car seq) (lambda (car-res)
                                     (cps-Ks (cdr seq) (lambda (cdr-res)
                                                         (build-k (cons car-res cdr-res))))))]))

  (cps-C expr kont))

(define (extract-and-reference expr vec)
  (match expr 
    [`(%call-prim ,prim ,kont . ,args)
     (receive (extracted-kont extracted-vec)
         (extract-and-reference kont vec)
       (values `(%call-prim ,prim ,extracted-kont ,args) extracted-vec))]
    [`(%call-lambda ,func ,kont . ,args)
     (receive (extracted-func extracted-vec)
         (extract-and-reference func vec)
       (receive (extracted-kont extracted-vec)
           (extract-and-reference kont extracted-vec)
         (values `(%call-lambda ,extracted-func ,extracted-kont ,args) extracted-vec)))]
    [`(%if ,test ,then ,else)
     (receive (extracted-then extracted-vec)
         (extract-and-reference then vec)
       (receive (extracted-else extracted-vec)
           (extract-and-reference else extracted-vec)
         (values `(%if ,test ,extracted-then ,extracted-else) extracted-vec)))]
    [`(lambda ,args ,body)
     (receive (extracted-body extracted-vec)
         (extract-and-reference body vec)
       (values `(lambda ,args ,extracted-body) extracted-vec))]
    [`(kappa ,args ,body)
       (receive (extracted-body extracted-vec)
           (extract-and-reference body vec)
         (let ((new-id (genksym "regisration")))
           (values new-id (cons `(,new-id ,extracted-body) extracted-vec))))]
    [else
     (values expr vec)]))
    
(define (print-example expr)
  (pretty-print (cps-transform expr 'halt)))

(define (reduce-example expr)
  (receive (reduced labels)
      (extract-and-reference (cps-transform expr 'halt) '())
    (pretty-print reduced)
    (pretty-print labels)))

(print-example
 `(begin
    (print "what")
    (if (foo) 1 2)
    (print "hi")))

(print-example
 `((lambda (n m)
     (if (= n 0)
         m
         (* n (f (- n 1) (+ m 1)))) 5)))


(print-example
 '(begin
    (if (foo) 1 2)
    (if (bar) 3 4)
    (if (baz) 5 6)

    (print "hi")
    (print "hi")
    (print "hi")
    (print "hi")
    ))

;; READ and compile a simple expression:
;; > (define expr (read))
;; (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
;; > (define cps-expr (cps-transform expr))
;; > (define-values (main-expr function-defs) (hoist cps-expr))
