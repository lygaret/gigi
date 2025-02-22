(define-library (gigi expander bindings)

  (import (except (scheme base) define))
  (import (scheme write))

  (import (srfi 1))   ; lists
  (import (srfi 125)) ; hash tables
  (import (srfi 128)) ; comparators
  (import (srfi 219)) ; higher-order lambda
  (import (srfi 227)) ; optional arguments

  (import (gigi core))
  (import (gigi scopes+binding-tables))

  (export )

  (begin
    (define-record-type runtime-binding
      (env-binding key)
      env-binding?
      (key env-binding-key))

    (define-record-type comptime-binding
      (comptime-binding expander)
      comptime-binding?
      (expander comptime-binding-expander))))
