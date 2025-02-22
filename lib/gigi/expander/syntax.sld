(define-library (gigi expander syntax)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi expander scopes+binding-tables))

  (export syntax
          make-syntax
          syntax?
          syntax-expr
          syntax-scopes
          syntax-srcloc

          syntax-map
          syntax-map-scopeset
          syntax-map-srcloc

          syntax-immediate?
          syntax-identifier?
          syntax-id-application?

          syntax->datum
          syntax->printable
          datum->syntax)

  (begin
    (define-record-type syntax
      (make-syntax expr scopes srcloc)
      syntax?
      (expr   syntax-expr)
      (scopes syntax-scopes)
      (srcloc syntax-srcloc))

    (define (syntax-map op ctx syn)
      (let ((syntax-map (curry syntax-map op)))
        (cond
         ((syntax? syn) (op ctx syn syntax-map))
         ((list? syn)   (map (curry syntax-map ctx) syn))
         ((pair? syn)   (cons (syntax-map ctx (car syn))
                              (syntax-map ctx (cdr syn))))
         (else syn))))

    (define (syntax-map-scopeset op syn)
      (syntax-map
       (lambda (ctx syn recur)
         (make-syntax (recur ctx (syntax-expr syn))
                      (op (syntax-scopes syn))
                      (syntax-srcloc syn)))
       #f syn))

    (define (syntax-map-srcloc op syn)
      (syntax-map
       (lambda (ctx syn recur)
         (make-syntax (recur ctx (syntax-expr syn))
                      (syntax-scopes syn)
                      (op (syntax-srcloc syn))))
       #f syn))

    ;; ---

    (define (syntax-immediate? syn)
      (and (syntax? syn)
           (let ((expr (syntax-expr syn)))
             (cond
              ((number? expr) #t)
              ((string? expr) #t)
              ((null? expr)   #t)
              ((pair? expr)   (eq? (car expr) 'quote))
              ((list? expr)   (eq? (car expr) 'quote))
              (else #f)))))

    (define (syntax-identifier? syn)
      (and (syntax? syn)
           (symbol? (syntax-expr syn))))

    (define (syntax-id-application? syn)
      (and (syntax? syn)
           (list? (syntax-expr syn))
           (syntax-identifier? (car (syntax-expr syn)))))

    ;; ---

    (define (syntax->datum syn)
      (syntax-map (lambda (ctx syn recur) (recur ctx (syntax-expr syn))) #f syn))

    (define (syntax->printable syn)
      (syntax-map (lambda (ctx syn recur)
                    (cond
                     ((syntax-immediate? syn)  (syntax-expr syn))
                     ((syntax-identifier? syn) (list (syntax-expr syn) (scopeset->list (syntax-scopes syn))))
                     (else                     (recur ctx (syntax-expr syn)))))
                  #f syn))

    (define datum->syntax
      (opt-lambda (v (scopeset empty-scopeset) (srcloc #f))
        (define (wrap expr)
          (if (syntax? expr) #;=> expr
              (make-syntax expr scopeset srcloc)))
        (wrap
         (cond
          ((list? v) (map (curryr datum->syntax scopeset srcloc) v))
          ((pair? v) (cons (datum->syntax (car v) scopeset srcloc)
                           (datum->syntax (cdr v) scopeset srcloc)))
          (else v)))))

    ;; ---

    ))
