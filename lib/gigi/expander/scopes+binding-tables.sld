(define-library (gigi expander scopes+binding-tables)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))

  ;; data structures for managaing scopes and bindings within them
  ;; bindings themselves aren't defined here, just the scopeset aware table
  ;;
  ;; expample:
  ;;
  ;; let a [scope/outer] + binding
  ;;     b [scope/outer] + binding
  ;;   let a [scope/inner+scope/outer] + binding
  ;;     (ref a) ; resolve a against current scope (inner+outer) = binding for inner a
  ;;     (ref b) ; resolve b = binding for outer b
  ;;     (ref c) ; resolve c = no binding #f
  ;;   let c [scope/inner2+scope/outer] + binding
  ;;     (ref a) ; outer a
  ;;     (ref c) ; inner c

  (export scope
          make-scope
          scope?

          scopeset
          scopeset?

          empty-scopeset
          scopeset-add
          scopeset-remove
          scopeset-toggle
          scopeset-subset?

          scopeset->list
          scopeset-memq
          scopeset-size

          binding-table
          make-binding-table
          binding-table?
          binding-table-insert!
          binding-table-resolve)

  (begin
    (define-record-type scope
      (make-scope-priv id)
      scope?
      (id scope-id))

    (define make-scope
      (let ((counter 0))
        (lambda ()
          (set! counter (+ 1 counter))
          (make-scope-priv counter))))

    ;; ---

    (define-record-type scopeset
      ;; simple set over scopes
      ;;   srfi 114 was bugging out in chibi
      (make-scopeset-priv scopes)
      scopeset?
      (scopes scopeset-scopes))

    (define (scopeset-subset? a b)
      (let loop ((as (scopeset-scopes a))
                 (bs (scopeset-scopes b)))
        (if (null? as) #t
            (if (not (memq (car as) bs)) #f
                (loop (cdr as) bs)))))

    (define empty-scopeset
      (make-scopeset-priv '()))

    (define (scopeset-add sc ss)
      (let ((scopes (scopeset-scopes ss)))
        (if (memq scopes sc) ss
            (make-scopeset-priv (cons sc scopes)))))

    (define (scopeset-remove sc ss)
      (let ((scopes (scopeset-scopes ss)))
        (make-scopeset-priv (delete sc scopes eq?))))

    (define (scopeset-toggle sc ss)
      ;; loop over the scopeset;
      ;;   if we find the scope, it means toggle=remove, so skip it and early return the tail
      ;;   if we hit the end, we didn't find it and toggle=add, so add it at the end
      (make-scopeset-priv
       (let loop ((scopes (scopeset-scopes ss)))
         (if (null? scopes)
             (list sc) ; we hit the end, add it
             (if (eq? (car scopes) sc)
                 (cdr scopes)  ; we found it, remove it by skipping
                 (cons (car scopes) (loop (cdr scopes))))))))

    (define (scopeset->list ss)
      (scopeset-scopes ss))

    (define (scopeset-size ss)
      (length (scopeset-scopes ss)))

    (define (scopeset-memq sc ss)
      (let ((scopes (scopeset-scopes ss)))
        (memq scopes sc)))

    ;; ---

    (define-record-type binding-table
      ;; sym -> ((scopeset . binding) ...)
      (make-binding-table-priv bindings)
      binding-table?
      (bindings binding-table-bindings))

    (define (make-binding-table)
      (let ((table (make-hash-table (make-eq-comparator))))
        (make-binding-table-priv table)))

    (define (binding-table-insert! bt sym ss binding)
      (let ((table (binding-table-bindings bt)))
        (hash-table-update!/default table sym (curry alist-cons ss binding) '())))

    (define (binding-table-resolve* bt sym ss)
      ;; find all possible matches
      ;; ie, entries where the store scope is a subset of the request
      (let* ((table    (binding-table-bindings bt))
             (by-name  (hash-table-ref/default table sym '()))
             (entry<=? (lambda (entry) (scopeset-subset? (car entry) ss))))
        (filter entry<=? by-name)))

    (define (binding-table-resolve bt sym ss)
      ;; find the closest match
      ;; ie, from the resolve* possibilities, get the largest set (which is a subset of ss)
      (let* ((options (binding-table-resolve* bt sym ss))
             (max     (lambda (a b) (if (< (scopeset-size (car a)) (scopeset-size (car b))) b a)))
             (choice  (reduce max '() options)))
        (and (not (null? choice))
             (cdr choice))))))
