(define-library (gigi expander)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi expander core))
  (import (gigi expander context))
  (import (gigi expander scopes+binding-tables))
  (import (gigi expander syntax))

  (export expand
          scope/core
          scopeset/core)

  (begin
    (define (expand syn)
      (syntax-map expand-syntax (build-core-context) syn))

    (define (expand-syntax ctx syn recur)
      (cond
       ((syntax-immediate? syn)      (syntax-expr syn))
       ((syntax-identifier? syn)     (expand-identifier ctx syn recur))
       ((syntax-id-application? syn) (expand-id-application ctx syn recur))
       (else                         (recur ctx (syntax-expr syn)))))

    (define (expand-identifier ctx syn recur)
      (let ((binding (context-resolve ctx syn)))
        (cond
         ((not binding)        `(%free ,(syntax-expr syn)))
         ((procedure? binding) (binding syn ctx recur))
         (else                 `(%bound ,(syntax-expr syn) ,binding)))))

    (define (expand-id-application ctx syn recur)
      (let* ((syn-id   (car (syntax-expr syn)))
             (syn-args (cdr (syntax-expr syn)))
             (binding  (context-resolve ctx syn-id)))
        (cond
         ((not binding)        `(%apply ,(recur ctx syn-id) ,@(recur ctx syn-args)))
         ((procedure? binding) (binding ctx syn recur))
         (else                 `(%apply (%other ,(recur ctx syn-id)) ,@(recur ctx syn-args))))))))
