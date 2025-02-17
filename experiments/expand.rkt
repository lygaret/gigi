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

;;;; if identifiers are `bound-identifier=?` they are fully interchangable
(define (bound-identifier=? a b)
  (and (syntax? a) (syntax? b)
       (eq? (syntax-e a) (syntax-e b))
       (equal? (syntax-scopes a) (syntax-scopes b))))

(define (matched-identifier=? a b)
  (and (syntax? a) (syntax? b)
       (eq? (syntax-e a) (syntax-e b))
       (subset? (syntax-scopes a) (syntax-scopes b))))

(module+ test
  (check-equal? (bound-identifier=? (syntax 'a (seteq)) (syntax 'a (seteq))) #t)
  (check-equal? (bound-identifier=? (syntax 'a (seteq)) (syntax 'b (seteq))) #f)
  (check-equal? (bound-identifier=? (syntax 'a (seteq)) (syntax 'a (seteq (scope)))) #f)
  (check-equal? (matched-identifier=? (syntax 'a (seteq)) (syntax 'b (seteq))) #f)
  (check-equal? (matched-identifier=? (syntax 'a (seteq)) (syntax 'a (seteq (scope)))) #t))

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
             #:when (matched-identifier=? c-id id))
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
                                  (list (syntax 'c (seteq sc1))
                                        (syntax 'c (seteq sc2)))))))

(define (free-identifier=? a b)
  (eq? (resolve a) (resolve b)))

(module+ test
  (check-equal? (free-identifier=? (syntax 'a (seteq sc1)) (syntax 'a (seteq sc1 sc2))) #t)
  (check-equal? (free-identifier=? (syntax 'b (seteq sc1)) (syntax 'b (seteq sc1 sc2))) #f))

;; core syntax and prims

;;;; accumulate core bindings in core-scope
(define core-scope (scope))
(define core-forms (seteq 'lambda 'let-syntax '#%app 'quote 'quote-syntax))
(define core-prims (seteq 'datum->syntax 'syntax->datum 'syntax-e 'list 'cons 'car 'cdr 'map))
(for ([sym (in-set (set-union core-forms core-prims))])
  (add-binding! (syntax sym (seteq core-scope)) sym))

;;;; adds the core scope to a syntax object
(define (namespace-syntax-introduce s)
  (add-scope core-scope s))

(module+ test
  (check-equal? (namespace-syntax-introduce (datum->syntax 'lambda))
                (syntax 'lambda (seteq core-scope)))
  
  (check-equal? (resolve (datum->syntax 'lambda)) #f)
  (check-equal? (resolve (namespace-syntax-introduce (datum->syntax 'lambda))) 'lambda))

;; compile time environment

;;;; an expansion envirnoment maps a local-binding gemsym to a proc
;;;; for a macro or the constant `variable for a runtime var

(define empty-env (hasheq))
(define variable (gensym 'variable))
(define missing (gensym 'missing))

(define (env-extend env key val)
  (hash-set env key val))

(define (env-lookup env binding)
  (hash-ref env binding missing))

(module+ test
  (check-equal? (env-lookup empty-env loc/a) missing)
  (check-equal? (env-lookup (env-extend empty-env loc/a 'val) loc/a) 'val))

;;;; helper for registering a local binding in the set of scopes
(define (add-local-binding! id)
  (define key (gensym (syntax-e id)))
  (add-binding! id key)
  key)

(module+ test
  (check-equal? (env-lookup empty-env loc/a) missing)
  
  (let ([new-env (env-extend empty-env loc/a 'value)])
    (check-equal? (env-lookup new-env loc/a) 'value))
  
  (define loc/d (add-local-binding! (syntax 'd (seteq sc1 sc2))))
  (check-equal? (resolve (syntax 'd (seteq sc1 sc2))) loc/d))

;; expansion dispatch

(module+ test
  ;; a number expands to a `quote` form:
  (check-equal? (expand (datum->syntax 1) empty-env)
                (list (syntax 'quote (seteq core-scope))
                      1))

  ;; binding and using a macr
  (check-equal? (syntax->datum
                 (expand (namespace-syntax-introduce (datum->syntax
                                                      '(let-syntax ([one (lambda (stx)
                                                                           (quote-syntax '1))])
                                                         (one))))))
                '(quote 1))

  ;; lambda expands to itself, so long as it has the core scope
  (check-equal? (syntax->datum
                 (expand
                  (namespace-syntax-introduce (datum->syntax '(lambda (x) x)))))
                '(lambda (x) x))

  ;; a reference to a core prim expands to itself
  (check-equal? (expand (syntax 'cons (seteq core-scope)) empty-env)
                (syntax 'cons (seteq core-scope)))

  ;; a locally bound variable expands to itself
  (check-equal? (expand (syntax 'a (seteq sc1)) ; bound to loc/a above
                        (env-extend empty-env loc/a variable))
                (syntax 'a (seteq sc1)))

  ;; a free variable triggers an error
  (check-exn (make-exn:fail? "free variable")
             (lambda ()
               (expand (syntax 'a (seteq)) empty-env)))

  ;; application of a locally-bound variable to a number expands to an #%app form
  (check-equal? (expand (list (syntax 'a (seteq sc1)) 1)
                        (env-extend empty-env loc/a variable))
                (list (syntax '#%app (seteq core-scope))
                      (syntax 'a (seteq sc1))
                      (list (syntax 'quote (seteq core-scope))
                            1)))

  ;; application of a locally bound variable to a number quotes
  (check-equal? (expand (list (syntax 'a (seteq sc1))
                              (list (syntax 'quote (seteq core-scope)) 1))
                        (env-extend empty-env loc/a variable))
                (list (syntax '#%app (seteq core-scope))
                      (syntax 'a (seteq sc1))
                      (list (syntax 'quote (seteq core-scope)) 1)))

  ;; application of a number to a number expands to an application
  (check-equal? (expand (namespace-syntax-introduce (datum->syntax '('0 '1))) empty-env)
                (list (syntax '#%app (seteq core-scope))
                      (list (syntax 'quote (seteq core-scope))
                            0)
                      (list (syntax 'quote (seteq core-scope))
                            1)))

  ;; locally bound macro expands by application
  (check-equal? (syntax->datum
                 (expand (syntax 'a (seteq sc1))
                         (env-extend empty-env loc/a (lambda (s) (datum->syntax 1)))))
                '(quote 1))
  (check-equal? (syntax->datum
                 (expand (let ([s (datum->syntax '(a (lambda (x) x)))])
                           (add-scope core-scope (add-scope sc1 s)))
                         (env-extend empty-env loc/a (lambda (s) (list-ref s 1)))))
                '(lambda (x) x)))

(define (expand s [env empty-env])
  (cond
    [(identifier? s)
     (expand-identifier s env)]
    [(and (pair? s) (identifier? (car s)))
     (expand-identifier-application s env)]
    [(or (pair? s) (null? s))
     (expand-application s env)]
    [else
     ;; anything else is implicitly quoted, so build a quote form
     (list (syntax 'quote (seteq core-scope))
           s)]))

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
       [(eq? v missing) (error "out of context" s)]
       [(procedure? v)  (expand (apply-transformer v s) env)]
       [else (error "illegal use of syntax" s)])]))

(define (expand-identifier-application s env)
  (define binding (resolve (car s)))
  (case binding
    [(lambda)       (expand-lambda s env)]
    [(let-syntax)   (expand-let-syntax s env)]
    [(quote)        s]
    [(quote-syntax) s]
    [(#%app)
     (match-define (list _ es ...) s)
     (expand-application es env)]
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
  (match-define `(,lambda-id (,arg-ids ...) ,body) s)
  (define sc (scope))
  (define ids (map (curry add-scope sc) arg-ids))
  (define bindings (map add-local-binding! ids))
  (define body-env (foldl (lambda (binding env)
                            (env-extend env binding variable))
                          env bindings))
  (define exp-body (expand (add-scope sc body) body-env))
  (list lambda-id ids exp-body))

(define (expand-let-syntax s env)
  (match-define `(,let-syntax-id ([,trans-ids ,trans-rhss] ...) ,body) s)
  (define sc (scope))
  (define ids (map (curry add-scope sc) trans-ids))
  (define bindings (map add-local-binding! ids))
  (define trans-vals (map eval-for-syntax-binding trans-rhss))
  (define body-env (foldl (lambda (binding val env)
                            (env-extend env binding val))
                          env bindings trans-vals))
  (expand (add-scope sc body) body-env))

(define (expand-application s env)
  (match-define `(,rator ,rands ...) s)
  (list* (syntax '#%app (seteq core-scope))
         (expand rator env)
         (map (curryr expand env) rands)))

;;;; expand and eval `rhs` as a compile time expression
(define (eval-for-syntax-binding rhs)
  (eval-compiled (compile (expand rhs empty-env))))

(module+ test
  (check-equal? (eval-for-syntax-binding
                 (namespace-syntax-introduce (datum->syntax '(car (list '1 '2)))))
                1)
  (check-equal? ((eval-for-syntax-binding
                  (namespace-syntax-introduce (datum->syntax '(lambda (x) (syntax-e x)))))
                 (syntax 'y (seteq)))
                'y))

(define (compile s)
  (cond
    [(pair? s)
     (define core-sym (resolve (car s)))
     (case core-sym
       [(lambda)
        (match-define `(,lambda-id (,ids ...) ,body) s)
        `(lambda ,(map resolve ids) ,(compile body))]
       [(#%app)
        (match-define `(,app-id ,rator ,rands ...) s)
        (cons (compile rator) (map compile rands))]
       [(quote)
        (match-define `(,quote-id ,datum) s)
        `(quote ,(syntax->datum datum))]
       [(quote-syntax)
        (match-define `(,quote-syntax-id ,datum) s)
        `(quote ,datum)]
       [else
        (error "unrecognized core form" s)])]
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
