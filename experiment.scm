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

(define t (datum->syntax
           '((lambda (a b)
               (if (< (- a b) 0)
                   'bigger
                   (do-something-else a)))
             10 a)
           scopeset/core))

(display (syntax->datum (expand t)))
