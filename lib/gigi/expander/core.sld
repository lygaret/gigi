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

        ;; add bindings for the arguments in the new body scope
        (let loop ((args (syntax->datum args)))
          (unless (null? args)
            (add-local-binding! ctx/body scopeset/body (car args))
            (loop (cdr args))))

        ;; and then recur an the reformed body
        `(,op ,args ,(recur ctx/body body-rescoped))))

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
        (let ((vars (let loop ((vars (syntax->datum vars)))
                      (unless (null? vars)
                        (let* ((entry (car vars))
                               (name  (car entry))
                               (defn  (car (cdr entry))))
                          (add-local-binding! ctx/body scopeset/body name)
                          (cons (recur ctx defn) (loop (cdr vars))))))))

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
        (add-core-form! ctx 'cond         core-expand/not-implemented)

        ctx))))
