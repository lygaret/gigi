(import (scheme base))
(import (scheme write))

(import (srfi 1))   ; lists
(import (srfi 125)) ; hash tables
(import (srfi 128)) ; comparators

(import (chibi test))

(import (gigi core))
(import (gigi expander))
(import (gigi expander core))
(import (gigi expander context))
(import (gigi expander scopes+binding-tables))
(import (gigi expander syntax))

;; Test utilities
(define (wildcard-equal? a b)
  (let wildcard-equal? ((a a) (b b) (memo (make-hash-table eqv?)))
    (define (wildcard-sym? sym)
      (and (symbol? sym)
           (let ((name (symbol->string sym)))
             (and (> (string-length name) 1)
                  (char=? (string-ref name 0) #\_)
                  (char<=? #\0 (string-ref name 1) #\9)))))

    (define (bind-or-check-wildcard a b)
      (let ((extant (hash-table-ref/default memo a #f)))
        (if extant
            (equal? b extant)
            (hash-table-set! memo a b))))

    (if (and (pair? a) (pair? b))
        (and (wildcard-equal? (car a) (car b) memo)
             (wildcard-equal? (cdr a) (cdr b) memo))
        (cond
         ;; we allow _ wildcard to match anything
         ((equal? '_  a) #t)
         ((equal? '_  b) #t)
         ;; first _1, _2, etc. sets the value, next tests against it
         ((wildcard-sym? a) (bind-or-check-wildcard a b))
         ((wildcard-sym? b) (bind-or-check-wildcard b a))
         ;; otherwise this isn't special, just use equal?
         (else (equal? a b))))))

(define (test-expand expr expected)
  (test-equal wildcard-equal?
    expected
    (syntax->datum (expand (datum->syntax expr)))))

(define (test-expand-with-scope expr scope expected)
  (test-equal wildcard-equal?
    expected
    (syntax->datum (expand (datum->syntax expr (scopeset-add scope empty-scopeset))))))

;; Test simple expressions
(test-group "basic-expansion"
  ;; Test immediate values
  (test-expand 42 42)
  (test-expand "hello" "hello")
  (test-expand '() '())
  
  ;; Test quoted expressions
  (test-expand '(quote hello) '(quote hello))
  
  ;; Test free identifiers
  (test-expand 'x '(%free x))
  
  ;; Test primitive applications
  (test-expand '(+ 1 2)    '((%bound + _ prim) 1 2))
  (test-expand '(cons 1 2) '((%bound cons _ prim) 1 2))
  
  ;; Test nested expressions
  (test-expand '(+ (* 2 3) 4) 
               '((%bound + _ prim) ((%bound * _ prim) 2 3) 4)))

;; Test lambda expressions
(test-group "lambda-expansion"
  ;; Test simple lambda
  (test-expand '(lambda (x) x)
               '(lambda ((%bind x _)) (%bound x _ variable)))
  
  ;; Test nested lambda
  (test-expand '(lambda (x) (lambda (y) (+ x y)))
               '(lambda ((%bind x _1)) 
                  (lambda ((%bind y _2))
                    ((%bound + _ prim) (%bound x _1 variable) (%bound y _2 variable)))))
  
  ;; Test lambda with free and bound variables
  (test-expand '(lambda (x) (+ x y))
               '(lambda ((%bind x _1)) 
                  ((%bound + _ prim) (%bound x _1 variable) (%free y)))))

;; Test let expressions
(test-group "let-expansion"
  ;; Test simple let
  (test-expand '(let ((x 1)) x)
               '(let (((%bind x _1) 1)) (%bound x _1 variable)))
  
  ;; Test nested let
  (test-expand '(let ((x 1)) (let ((y 2)) (+ x y)))
               '(let (((%bind x _1) 1)) 
                  (let (((%bind y _2) 2)) 
                    ((%bound + _ prim) (%bound x _1 variable) (%bound y _2 variable)))))
  
  ;; Test let with shadowing
  (test-expand '(let ((x 1)) (let ((x 2)) x))
               '(let (((%bind x _1) 1)) 
                  (let (((%bind x _2) 2)) 
                    (%bound x _2 variable)))))

;; Test define and set!
(test-group "define-set-expansion"
  ;; Test simple define
  (test-expand '(define x 1)
               '(define x 1))
  
  ;; Test define with complex body
  (test-expand '(define y (+ 1 2))
               '(define y ((%bound + _ prim) 1 2)))
  
  ;; Test set!
  (test-expand '(set! z 3)
               '(set! z 3))
  
  ;; Test set! with complex body
  (test-expand '(set! z (+ 1 2))
               '(set! z ((%bound + _ prim) 1 2))))

;; Test complex expressions combining multiple features
(test-group "complex-expansion"
  ;; Test a more complex expression with lambda, let and application
  (test-expand 
   '((lambda (x) 
       (let ((y (+ x 1))) 
         (* y 2))) 10)
   '((lambda ((%bind x _1)) 
       (let (((%bind y _2) ((%bound + _ prim) (%bound x _1 variable) 1))) 
         ((%bound * _ prim) (%bound y _2 variable) 2))) 10)))

;; Test expansion of special forms
(test-group "special-forms"
  ;; Test if
  (test-expand '(if (< x 10) 'small 'large)
               '(if ((%bound < _ prim) (%free x) 10) 'small 'large))
  
  ;; Test and/or
  (test-expand '(and (< x 10) (> x 0))
               '(and ((%bound < _ prim) (%free x) 10) ((%bound > _ prim) (%free x) 0)))
  
  (test-expand '(or (< x 0) (> x 10))
               '(or ((%bound < _ prim) (%free x) 0) ((%bound > _ prim) (%free x) 10))))
