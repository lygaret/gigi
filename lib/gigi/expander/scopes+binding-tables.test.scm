(import (scheme base))

(import (srfi 1))   ; lists
(import (srfi 113)) ; sets and bags
(import (srfi 125)) ; hash tables
(import (srfi 128)) ; comparators

(import (chibi test))

(import (gigi core))
(import (gigi expander scopes+binding-tables))

;; Basic scope creation and comparison
(test-group "scopes"
  (let ((s1 (make-scope))
        (s2 (make-scope)))
    (test-assert "scope creation" (scope? s1))
    (test-assert "different scopes not equal" (not (eq? s1 s2)))))

;; Scopeset operations
(test-group "scopesets"
  (let* ((s1 (make-scope))
         (s2 (make-scope))
         (ss1 (scopeset-add s1 empty-scopeset))
         (ss2 (scopeset-add s2 empty-scopeset))
         (ss-both (scopeset-add s2 ss1)))
    
    (test-assert "scopeset creation" (scopeset? ss1))

    (test "scopeset size 0" 0 (scopeset-size empty-scopeset))
    (test "scopeset size 1" 1 (scopeset-size ss1))
    (test "scopeset size 1" 1 (scopeset-size ss2))
    (test "scopeset size 2" 2 (scopeset-size ss-both))
    
    (test-assert "subset relationship" (scopeset-subset? ss1 ss-both))
    (test-assert "subset relationship" (scopeset-subset? ss2 ss-both))
    (test-assert "not subset" (not (scopeset-subset? ss-both ss2)))
    (test-assert "not subset" (not (scopeset-subset? ss1 ss2)))
    
    (let ((ss-removed (scopeset-remove s1 ss-both)))
      (test "remove scope" 1 (scopeset-size ss-removed))
      (test-assert "removed correct scope" (not (scopeset-memq s1 ss-removed))))
    
    (let ((ss-toggled (scopeset-toggle s1 ss1)))
      (test "toggle removes existing" 0 (scopeset-size ss-toggled))
      (test "toggle adds missing"     1 (scopeset-size (scopeset-toggle s2 ss-toggled))))))

;; Binding table resolution
(test-group "binding-resolution"

  ;; nested scopes
  ;; let a [scope/outer] + binding
  ;;   a
  ;;   b
  ;;   let a [scope/middle] + binding
  ;;     a
  ;;     b
  ;;     let b [scope/inner] + binding
  ;;        a
  ;;        b

  (define t (make-binding-table))

  (define scope/outer (make-scope))
  (define ss/outer (scopeset-add scope/outer empty-scopeset))

  (define scope/middle (make-scope))
  (define ss/middle (scopeset-add scope/middle ss/outer))

  (define scope/inner (make-scope))
  (define ss/inner (scopeset-add scope/inner ss/middle))

  (test 1 (scopeset-size ss/outer))
  (test 2 (scopeset-size ss/middle))
  (test 3 (scopeset-size ss/inner))

  (binding-table-insert! t 'a ss/outer 'outer)
  (binding-table-insert! t 'a ss/middle 'middle)
  (binding-table-insert! t 'b ss/inner 'inner)

  ;; Test basic lookup
  (test "outer scope resolves known" 'outer (binding-table-resolve t 'a ss/outer))

  (test "outer scope fails on unknown" #f (binding-table-resolve t 'b ss/outer))

  (test "middle scope resolves intermediate" 'middle (binding-table-resolve t 'a ss/middle))

  (test "middle scope fails on unknown" #f (binding-table-resolve t 'b ss/middle))

  (test "inner scope resolves upwards" 'middle (binding-table-resolve t 'a ss/inner))

  (test "inner scope resolves local" 'inner (binding-table-resolve t 'b ss/inner))

  ;; Test resolution with gaps
  ;; A scope between outer and middle should see outer binding
  (let* ((scope/gap (make-scope))
         (ss/gap    (scopeset-add scope/gap ss/outer)))
    (let ((gap-res (binding-table-resolve t 'a ss/gap)))
      (test "gap resolution falls back to outer" 'outer gap-res))
  
  ;; test multiple bindings at same level
  (let* ((scope/sibling   (make-scope))
         (ss/sibling      (scopeset-add scope/sibling ss/outer)))
    (binding-table-insert! t 'y ss/sibling 'sibling)
    (binding-table-insert! t 'y ss/outer 'outer)
    
    ;; Sibling scope should see its own binding for y
    (test "sibling scope own binding" 'sibling (binding-table-resolve t 'y ss/sibling))
    
    ;; But outer scope should only see outer binding
    (test "outer scope unaffected by sibling" 'outer (binding-table-resolve t 'y ss/outer)))))

;; Test edge cases
(test-group "edge-cases"
  (let ((t (make-binding-table)))
    (test "lookup of missing binding" #f (binding-table-resolve t 'x empty-scopeset))
    
    ;; Test resolution with empty scopeset
    (let* ((scope1   (make-scope))
           (ss1      (scopeset-add scope1 empty-scopeset)))
      (binding-table-insert! t 'x ss1 'binding)
      (test "resolve with empty scopeset" #f (binding-table-resolve t 'x empty-scopeset)))))
