(define-library (gigi expander syntax)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi expander scopes+bindings))

  (export syntax
          make-syntax
          syntax?
          syntax-expr
          syntax-scopes
          syntax-srcloc

          syntax->datum
          datum->syntax)

  (begin
    (define-record-type syntax
      (make-syntax expr scopes srcloc)
      syntax?
      (expr   syntax-expr)
      (scopes syntax-scopes)
      (srcloc syntax-srcloc))

    (define (syntax->datum s)
      (let loop ((expr (syntax-expr s)))
        (cond
         ((syntax? expr) (loop (syntax-expr expr)))
         ((list? expr)   (map loop expr))
         (else expr))))

    (define datum->syntax
      (opt-lambda (v (scopeset empty-scopeset) (srcloc #f))
        (define (wrap expr)
          (if (syntax? expr) #;=> expr
              (make-syntax expr scopeset srcloc)))
        (wrap
         (cond
          ((list? v) (map (curryr datum->syntax scopeset srcloc) v))
          ((pair? v) (map (curryr datum->syntax scopeset srcloc) v))
          (else v)))))))
