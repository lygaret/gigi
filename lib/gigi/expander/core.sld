(define-library (gigi expander core)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi expander context))
  (import (gigi expander scopes+binding-tables))
  (import (gigi expander syntax))

  (export build-core-context
          add-local-binding!
          scope/core
          scopeset/core)

  (begin
    (define scope/core    (make-scope))
    (define scopeset/core (scopeset-add scope/core empty-scopeset))

    (define-record-type core-token
      (make-core-token-priv counter)
      core-token?
      (counter core-token-counter))

    (define make-core-token
      (let ((counter 0))
        (lambda ()
          (set! counter (+ 1 counter))
          (make-core-token-priv counter))))

    (define (add-core-form! ctx sym transformer)
      (let ((key (make-core-token)))
        (binding-table-insert! (context-bindings ctx) sym scopeset/core key)
        (context-env-insert! ctx key transformer)
        key))

    (define (add-core-prims! ctx syms)
      (let loop ((syms syms))
        (unless (null? syms)
          (let ((key (make-core-token))
                (sym (car syms)))
            (binding-table-insert! (context-bindings ctx) sym scopeset/core key)
            (context-env-insert! ctx key 'prim)
            (loop (cdr syms))))))

    (define (add-local-binding! ctx ss sym)
      (let ((key (make-core-token)))
        (binding-table-insert! (context-bindings ctx) sym ss key)
        (context-env-insert! ctx key 'variable)
        key))

    ;; ---

    (define (core-expand/lambda ctx syn recur)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (args (car (cdr expr)))
             (body (car (cdr (cdr expr))))

             (scope/body    (make-scope))
             (scopeset/body (scopeset-add scope/body (syntax-scopes syn)))
             (ctx/body      (make-expansion-context ctx))
             (body-rescoped (syntax-add-scope body scope/body)))

        ;; add bindings for the arguments in the new scope
        (let ((args (let loop ((args (syntax-expr args)))
                      (if (null? args) '()
                          (let* ((name (syntax-expr (car args)))
                                 (key  (add-local-binding! ctx/body scopeset/body name)))
                            (cons `(%bind ,name ,key) (loop (cdr args))))))))

          ;; and then recur an the reformed body
          `(,op ,args ,(recur ctx/body body-rescoped)))))

    (define (core-expand/let ctx syn recur)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (vars (car (cdr expr)))
             (body (car (cdr (cdr expr))))

             (scope/body    (make-scope))
             (scopeset/body (scopeset-add scope/body (syntax-scopes syn)))
             (ctx/body      (make-expansion-context ctx))
             (body-rescoped (syntax-add-scope body scope/body)))

        ;; add bindings for the arguments in the new scope
        (let ((vars (let loop ((vars (syntax-expr vars)))
                      (if (null? vars)
                          '()
                          (let* ((expr (syntax-expr (car vars)))
                                 (name (car expr))
                                 (defn (car (cdr expr)))
                                 (key  (add-local-binding! ctx/body scopeset/body (syntax-expr name)))
                                 (defn (syntax-add-scope defn scope/body))
                                 (defn (recur ctx/body defn)))
                            (cons `((%bind ,name ,key) ,defn) (loop (cdr vars))))))))

          ;; todo expanded everything
          `(,op ,vars ,(recur ctx/body body-rescoped)))))

    ;; ---

    (define (core-expand/define ctx syn recur)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (name (car (cdr expr)))
             (body (car (cdr (cdr expr)))))
        ;; todo expanded define lambda syntax
        `(,op ,name ,(recur ctx body))))

    (define (core-expand/set! ctx syn recur)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (name (car (cdr expr)))
             (body (car (cdr (cdr expr)))))
        `(,op ,name ,(recur ctx body))))

    ;; ---

    (define (core-expand/cond ctx syn recur)
      ;; (cond ((test) body...) ...)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (vars (let loop ((vars (cdr expr)))
                     (if (null? vars)
                         '()
                         (let* ((expr (syntax-expr (car vars)))
                                (test (car expr))
                                (body (cdr expr)))
                           (cons (cons (if (eqv? (syntax-expr test) 'else) test (recur ctx test))
                                       (recur ctx body))
                                 (loop (cdr vars))))))))
        `(,op ,@vars)))

    ;; ---

    (define (core-expand/generic-recur-args ctx syn recur)
      (let* ((expr (syntax-expr syn))
             (op   (car expr))
             (rest (cdr expr)))
        `(,op ,@(recur ctx rest))))

    (define (core-expand/not-implemented ctx syn recur)
      `(%todo ,(syntax-expr syn)))

    ;; ---

    (define (build-core-context)
      (let ((ctx (make-expansion-context #f)))

        ;; prims get found in context and returned as a special case
        ;; the underlying compiler will handle them when we get there

        (add-core-prims! ctx '(+ - / * < = >))
        (add-core-prims! ctx '(cons car cdr null? pair?))
        (add-core-prims! ctx '(eq? eqv? equal?))

        ;; remember this is just expansion
        ;; we need to handle all core forms, but most of them can just recur on their args

        (add-core-form! ctx 'lambda       core-expand/lambda)

        (add-core-form! ctx 'let          core-expand/let)
        (add-core-form! ctx 'let*         core-expand/let)
        (add-core-form! ctx 'letrec       core-expand/let)

        (add-core-form! ctx 'define       core-expand/define)
        (add-core-form! ctx 'set!         core-expand/set!)

        (add-core-form! ctx 'quote        (lambda (c syn r) (syntax->datum syn)))
        (add-core-form! ctx 'quote-syntax (lambda (c syn r) syn))
        (add-core-form! ctx 'quasiquote   (lambda (c syn r) (syntax->datum syn)))

        (add-core-form! ctx 'and          core-expand/generic-recur-args)
        (add-core-form! ctx 'or           core-expand/generic-recur-args)
        (add-core-form! ctx 'if           core-expand/generic-recur-args)
        (add-core-form! ctx 'cond         core-expand/cond)

        ctx))))
