(import (except (scheme base) define))
(import (scheme write))
(import (gigi core))

(import (gigi expander context))
(import (gigi expander core))
(import (gigi expander scopes+binding-tables))
(import (gigi expander syntax))
(import (gigi expander))

(import (srfi 1))   ; lists
(import (srfi 113)) ; sets and bags
(import (srfi 125)) ; hash tables
(import (srfi 128)) ; comparators
(import (srfi 219)) ; higher-order lambda
(import (srfi 227)) ; optional arguments

(begin
  (define t (datum->syntax
             '((lambda (a b)
                 (if (< (- a b) 0)
                     'bigger
                     ((lambda (b c)
                        (b a c)) b)))
               10 a)
             scopeset/core))
  
  (display (syntax->datum (expand t))))

(%apply (%lambda (a b)
                 (%if (%apply (%free <)
                              (%apply (%free -)
                                      (%bound a {core-token #75 6} variable)
                                      (%bound b {core-token #75 7} variable))
                              0)
                      (quote bigger)
                      (%apply (%lambda (b c)
                                       (%apply (%bound b {core-token #75 8} variable)
                                               (%bound a {core-token #75 6} variable)
                                               (%bound c {core-token #75 9} variable)))
                              (%bound b {core-token #75 7} variable))))
        10 (%free a))
