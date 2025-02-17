#lang racket/base

;; pico expander from
;; github.com/mflatt/expander

(require racket/function)
(require racket/list)
(require racket/match)
(require racket/set)

(define (set-xor s e)
  (if (set-member? s e)
      (set-remove s e)
      (set-add s e)))

(provide
 expand
 compile
 (rename-out [eval-compiled eval])
 
 datum->syntax
 introduce)

(module+ test
  (require rackunit)
  (define (make-exn:fail? rx)
    (lambda (v)
      (and (exn:fail? v)
           (regexp-match? rx (exn-message v))))))

;; syntax objects

(struct syntax (e scopes) #:transparent)

(define (identifier? s)
  (syntax? s))

(module+ test
  (check-equal? (identifier? (syntax 'x (seteq))) #t))
      
;;;; datum->syntax coerces to syntax with empty scope
(define (datum->syntax v)
  (cond
    [(syntax? v) v]
    [(symbol? v) (syntax v (seteq))]
    [(list? v)   (map datum->syntax v)]
    [else v]))
                
;;;; syntax->datum recursively discards scopes
(define (syntax->datum s)
  (cond
    [(syntax? s) (syntax-e s)]
    [(list? s)   (map syntax->datum s)]
    [else s]))

(module+ test
  (check-equal? (datum->syntax 1) 1)
  (check-equal? (datum->syntax 'a) (syntax 'a (seteq)))
  (check-equal? (datum->syntax '(a b c))
                (list (syntax 'a (seteq))
                      (syntax 'b (seteq))
                      (syntax 'c (seteq))))
  (check-equal? (datum->syntax `(a ,(syntax 'b (seteq)) c))
                (list (syntax 'a (seteq))
                      (syntax 'b (seteq))
                      (syntax 'c (seteq))))
  (check-equal? (syntax->datum (datum->syntax 'a)) 'a)
  (check-equal? (syntax->datum (datum->syntax 1)) 1)
  (check-equal? (syntax->datum (datum->syntax '(a b c))) '(a b c)))

;; scopes

(struct scope ())

(module+ test
  (define sc1 (scope))
  (define sc2 (scope))
  
  (check-equal? (eq? sc1 sc2) #f)
  (check-equal? (eq? sc1 sc1) #t))

;;;; handle adjusting scopes

(define (adjust-scope op sc s)
  (cond
    [(syntax? s) (syntax (syntax-e s) (op (syntax-scopes s) sc))]
    [(list? s)   (map (curry adjust-scope op sc) s)]
    [else s]))

(define add-scope (curry adjust-scope set-add))
(define xor-scope (curry adjust-scope set-xor))

(module+ test
  (check-equal? (add-scope sc1 (datum->syntax 'x))
                (syntax 'x (seteq sc1)))
  
  (check-equal? (xor-scope sc1 (add-scope sc1 (datum->syntax 'x)))
                (syntax 'x (seteq)))
  
  (check-equal? (add-scope sc1 (datum->syntax '(x (y))))
                (list (syntax 'x (seteq sc1))
                      (list (syntax 'y (seteq sc1)))))

  (check-equal? (xor-scope sc2 (syntax 'x (seteq sc1 sc2)))
                (syntax 'x (seteq sc1))))

;; global binding table
;; a binding is either a gensym for a local variable, or a symbor for a core-form or prim

(define all-bindings (make-hash))

(define (add-binding! id binding)
  (hash-set! all-bindings id binding))

(module+ test
  ;; simulate
  ;;   (let ([a 1])
  ;;     (let ([z 2])
  ;;       ...))
  ;; where a is bound only once
  (define loc/a (gensym 'a))
  
  ;; simulate
  ;;   (let ([b 1])
  ;;     (let ([b 2])
  ;;       ...))
  ;; where b is shadowing
  (define loc/b-in (gensym 'b))
  (define loc/b-out (gensym 'b))
  
  ;; simulate
  ;;   (list (let ([c 1]) ...)
  ;;         (let ([c 2]) ...)
  ;; where the c's are non-overlapping
  (define loc/c1 (gensym 'c))
  (define loc/c2 (gensym 'c))
  
  ;; same binding in sc1 or sc1+2
  (add-binding! (syntax 'a (seteq sc1)) loc/a)
  
  ;; shadowing
  (add-binding! (syntax 'b (seteq sc1)) loc/b-out)
  (add-binding! (syntax 'b (seteq sc1 sc2)) loc/b-in)
  
  ;; ambiguous
  (add-binding! (syntax 'c (seteq sc1)) loc/c1)
  (add-binding! (syntax 'c (seteq sc2)) loc/c2))

;;;; finds the binding for a given identifier, or #f
(define (resolve id)
  (define candidate-ids (find-all-matching-bindings id))
  (cond
    [(pair? candidate-ids)
     (define max-id
       (argmax (compose set-count syntax-scopes) candidate-ids))
     (check-unambiguous max-id candidate-ids)
     (hash-ref all-bindings max-id)]
    [else #f]))

(module+ test
  (check-equal? (resolve (syntax 'a (seteq sc1))) loc/a)
  (check-equal? (resolve (syntax 'a (seteq sc1 sc2))) loc/a)
  (check-equal? (resolve (syntax 'a (seteq sc2))) #f)
  
  (check-equal? (resolve (syntax 'b (seteq sc1))) loc/b-out)
  (check-equal? (resolve (syntax 'b (seteq sc1 sc2))) loc/b-in)
  (check-equal? (resolve (syntax 'b (seteq sc2))) #f)
  
  (check-equal? (resolve (syntax 'c (seteq sc1))) loc/c1)
  (check-equal? (resolve (syntax 'c (seteq sc2))) loc/c2)
  (check-exn (make-exn:fail? "ambiguous")
             (lambda () (resolve (syntax 'c (seteq sc1 sc2))))))

;;;; find all candidate bindings for id (those with subset of scopes)
(define (find-all-matching-bindings id)
  (for/list ([c-id (in-hash-keys all-bindings)]
             #:when (and (eq? (syntax-e c-id) (syntax-e id))
                         (subset? (syntax-scopes c-id) (syntax-scopes id))))
    c-id))

(module+ test
  (check-equal? (find-all-matching-bindings (syntax 'a (seteq sc1)))
                (list (syntax 'a (seteq sc1))))
  (check-equal? (find-all-matching-bindings (syntax 'a (seteq sc2)))
                (list))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'b (seteq sc1 sc2))))
                (set (syntax 'b (seteq sc1))
                     (syntax 'b (seteq sc1 sc2))))
  
  (check-equal? (list->set (find-all-matching-bindings (syntax 'c (seteq sc1 sc2))))
                (set (syntax 'c (seteq sc1))
                     (syntax 'c (seteq sc2)))))

;; ensures that the binding with the biggest scope is a superset
(define (check-unambiguous max-id candidate-ids)
  (for ([c-id (in-list candidate-ids)])
    (unless (subset? (syntax-scopes c-id) (syntax-scopes max-id))
      (error "ambiguous:" max-id))))

(module+ test
  (check-equal? (check-unambiguous (syntax 'b (seteq sc1 sc2))
                                   (list (syntax 'b (seteq sc1))
                                         (syntax 'b (seteq sc1 sc2))))
                (void))
  
  (check-exn (make-exn:fail? "ambiguous")
             (lambda ()
               (check-unambiguous (syntax 'c (seteq sc2))
                                  (syntax 'c (seteq sc1))
                                  (syntax 'c (seteq sc2))))))

;; core syntax and prims

;;;; accumulate core bindings in core-scope
(define core-scope (scope))
(define core-forms (seteq 'lambda 'let-syntax 'quote 'quote-syntax))
(define core-prims (seteq 'datum->syntax 'syntax->datum 'syntax-e 'list 'cons 'car 'cdr 'map))
(for ([sym (in-set (set-union core-forms core-prims))])
  (add-binding! (syntax sym (seteq core-scope)) sym))

;;;; adds the core scope to a syntax object
(define (introduce s)
  (add-scope core-scope s))

(module+ test
  (check-equal? (introduce (datum->syntax 'lambda))
                (syntax 'lambda (seteq core-scope)))
  
  (check-equal? (resolve (datum->syntax 'lambda)) #f)
  (check-equal? (resolve (introduce (datum->syntax 'lambda))) 'lambda))

;; compile time environment

;;;; an expansion envirnoment maps a local-binding gemsym to a proc
;;;; for a macro or the constant `variable for a runtime var

(define empty-env (hasheq))
(define variable (gensym 'variable))

(define (env-extend env key val)
  (hash-set env key val))

(define (env-lookup env binding)
  (hash-ref env binding #f))

;;;; helper for registering a local binding in the set of scopes
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id key)
  key)

(module+ test
  (check-equal? (env-lookup empty-env loc/a) #f)
  
  (let ([new-env (env-extend empty-env loc/a variable)])
    (check-equal? (env-lookup new-env loc/a) variable))
  
  (define loc/d (add-local-binding! (syntax 'd (seteq sc1 sc2))))
  (check-equal? (resolve (syntax 'd (seteq sc1 sc2))) loc/d))

;; expansion dispatch

(define (expand s [env empty-env])
  (cond
    [(identifier? s)
     (expand-identifier s env)]
    [(and (pair? s) (identifier? (car s)))
     (expand-identifier-application s env)]
    [(or (pair? s) (null? s))
     (expand-application s env)]
    [else
     (error "bad syntax:" s)]))

(define (expand-identifier s env)
  (define binding (resolve s))
  (cond
    [(not binding) (error "free variable" s)]
    [(set-member? core-prims binding) s]
    [(set-member? core-forms binding) (error "bad syntax:" s)]
    [else
     (define v (env-lookup env binding))
     (cond
       [(eq? v variable) s]
       [(not v) (error "out of context" s)]
       [else    (error "bad syntax" s)])]))

(define (expand-identifier-application s env)
  (define binding (resolve (car s)))
  (case binding
    [(lambda)       (expand-lambda s env)]
    [(let-syntax)   (expand-let-syntax s env)]
    [(quote)        s]
    [(quote-syntax) s]
    [else
     (define v (env-lookup env binding))
     (cond
       [(procedure? v) (expand (apply-transformer v s) env)]
       [else (expand-application s env)])]))

(define (apply-transformer t s)
  (define intro-scope   (scope))
  (define intro-s       (add-scope intro-scope s))
  (define transformed-s (t intro-s))
  (xor-scope intro-scope transformed-s))

(module+ test
  ;; check that applying the transformer adds a scope to introduced parts
  ;; and that original parts get left alone
  (define transformed-s
    (apply-transformer (lambda (s) ;; converts `(_ f)` to `(f x)`
                         (list (list-ref s 1)
                               (syntax 'x (seteq))))
                       (list (syntax 'm (seteq))
                             (syntax 'f (seteq sc1)))))
  
  (check-equal? (syntax->datum transformed-s) '(f x))
  (check-equal? (list-ref transformed-s 0) (syntax 'f (seteq sc1)))
  (check-equal? (set-count (syntax-scopes (list-ref transformed-s 1))) 1))

;;;;

(define (expand-lambda s env)
  (match-define `(,lambda-id (,arg-id) ,body) s)
  (define sc (scope))
  (define id (add-scope sc arg-id))
  (define binding (add-local-binding! id))
  (define body-env (env-extend env binding variable))
  (define exp-body (expand (add-scope sc body) body-env))
  (list lambda-id (list id) exp-body))

(define (expand-let-syntax s env)
  (match-define `(,let-syntax-id ([,lhs-id ,rhs]) ,body) s)
  (define sc (scope))
  (define id (add-scope sc lhs-id))
  (define binding (add-local-binding! id))
  (define rhs-val (eval-for-syntax-binding rhs))
  (define body-env (env-extend env binding rhs-val))
  (expand (add-scope sc body) body-env))

(define (expand-application s env)
  (map (curryr expand env) s))

;;;; expand and eval `rhs` as a compile time expression
(define (eval-for-syntax-binding rhs)
  (eval-compiled (compile (expand rhs empty-env))))

(module+ test
  (check-equal? (eval-for-syntax-binding
                 (introduce (datum->syntax '(car (list '1 '2)))))
                1)
  (check-equal? ((eval-for-syntax-binding
                  (introduce (datum->syntax '(lambda (x) (syntax-e x)))))
                 (syntax 'y (seteq)))
                'y))

(define (compile s)
  (cond
    [(pair? s)
     (define core-sym (and (identifier? (car s))
                           (resolve (car s))))
     (case core-sym
       [(lambda)
        (match-define `(,lambda-id (,id) ,body) s)
        `(lambda (,(resolve id)) ,(compile body))]
       [(quote)
        (match-define `(,quote-id ,datum) s)
        `(quote ,(syntax->datum datum))]
       [(quote-syntax)
        (match-define `(,quote-syntax-id ,datum) s)
        `(quote ,datum)]
       [else
        (map compile s)])]
    [(identifier? s)
     (resolve s)]
    [else
     (error "bad syntax after expansion" s)]))

;; using host racket
;; create a namespace for evaluate expressions
;; install datum->syntax and syntax-e te replace host prims

(define namespace (make-base-namespace))
(namespace-set-variable-value! 'datum->syntax datum->syntax #t namespace)
(namespace-set-variable-value! 'syntax->datum syntax->datum #t namespace)
(namespace-set-variable-value! 'syntax-e syntax-e #t namespace)

(define (eval-compiled s)
  (eval s namespace))

(module+ test
  ;; binding and using a macr
  (check-equal? (syntax->datum
                 (expand (introduce (datum->syntax
                                     '(let-syntax ([one (lambda (stx)
                                                          (quote-syntax '1))])
                                        (one))))))
                '(quote 1))
  
  ;; lambda expands to itself, so long as it has the core scope
  (check-equal? (syntax->datum
                 (expand
                  (introduce (datum->syntax '(lambda (x) x)))))
                '(lambda (x) x))
  
  ;; a reference to a core prim expands to itself
  (check-equal? (expand (syntax 'cons (seteq core-scope)))
                (syntax 'cons (seteq core-scope)))
  
  ;; a locally bound variable expands to itself
  (check-equal? (expand (syntax 'a (seteq sc1)) ; bound to loc/a above
                        (env-extend empty-env loc/a variable))
                (syntax 'a (seteq sc1)))
  
  ;; a free variable triggers an error
  (check-exn (make-exn:fail? "free variable")
             (lambda ()
               (expand (syntax 'a (seteq)) empty-env)))
  
  ;; application of a locally bound variable to a number quotes
  (check-equal? (expand (list (syntax 'a (seteq sc1))
                              (list (syntax 'quote (seteq core-scope)) 1))
                        (env-extend empty-env loc/a variable))
                (list (syntax 'a (seteq sc1))
                      (list (syntax 'quote (seteq core-scope)) 1)))
  
  ;; application of a number to a number expands to an application
  (check-equal? (expand (introduce (datum->syntax '('0 '1))) empty-env)
                (list (list (syntax 'quote (seteq core-scope))
                            0)
                      (list (syntax 'quote (seteq core-scope))
                            1)))
  
  ;; locally bound macro expands by application
  (check-equal? (syntax->datum
                 (expand (list (syntax 'a (seteq sc1)))
                         (env-extend empty-env loc/a (lambda (s)
                                                       (list (syntax 'quote (seteq core-scope))
                                                             1)))))
                '(quote 1))
  (check-equal? (syntax->datum
                 (expand (let ([s (datum->syntax '(a (lambda (x) x)))])
                           (add-scope core-scope (add-scope sc1 s)))
                         (env-extend empty-env loc/a (lambda (s) (list-ref s 1)))))
                '(lambda (x) x)))
