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
      (make-core-token)
      core-token?)

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

    (define (build-core-context)
      (let ((ctx (make-expansion-context #f)))
        (add-core-form! ctx 'quote-syntax (lambda (c syn r) syn))
        (add-core-form! ctx 'quote        (lambda (c syn r) (syntax->datum syn)))

        (add-core-form! ctx 'if
         (lambda (ctx syn recur)
           (let* ((expr (syntax-expr syn))
                  (test (car (cdr expr)))
                  (then (car (cdr (cdr expr))))
                  (else (car (cdr (cdr (cdr expr))))))
             `(%if ,(recur ctx test)
                   ,(recur ctx then)
                   ,(recur ctx else)))))

        (add-core-form! ctx 'lambda
          (lambda (ctx syn recur)
            (let* ((expr (syntax-expr syn))
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
              `(%lambda ,args ,(recur ctx/body body-rescoped)))))

        (add-core-form! ctx 'define
          (lambda (ctx syn recur)
            (let* ((expr (syntax-expr syn))
                   (head (car (cdr expr)))
                   (body (car (cdr (cdr expr)))))
              `(%define ,head ,(recur ctx body)))))

        ctx))
    ))
