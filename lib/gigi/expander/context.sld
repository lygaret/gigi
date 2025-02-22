(define-library (gigi expander context)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi expander scopes+binding-tables))
  (import (gigi expander syntax))

  (export expansion-context
          expansion-context?
          make-expansion-context

          context-bindings
          context-environ

          context-resolve
          context-env-insert!
          )

  (begin
    (define-record-type expansion-context
      (make-expansion-context-priv bindings environ)
      expansion-context?
      (bindings context-bindings)
      (environ  context-environ set-context-environ!))

    (define (make-expansion-context parent)
      (let* ((bindings (or (and parent (context-bindings parent)) (make-binding-table)))
             (environ  (or (and parent (context-environ parent)) '())))
        (make-expansion-context-priv bindings environ)))

    (define (context-resolve ctx syn)
      (let* ((bt      (context-bindings ctx))
             (env     (context-environ ctx))
             (sym     (syntax-expr syn))
             (ss      (syntax-scopes syn))
             (binding (binding-table-resolve bt sym ss))
             (entry   (and binding (assq binding (context-environ ctx)))))
        (and entry (cdr entry))))

    (define (context-env-insert! ctx key value)
      (set-context-environ! ctx (alist-cons key value (context-environ ctx))))))
