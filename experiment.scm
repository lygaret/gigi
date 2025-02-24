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
               10 a)))
  (display (syntax->datum (expand t))))

#;
'((lambda ((%bind a {core-token #75 14}) (%bind b {core-token #75 15}))
   (if ((%free <)
        ((%free -)
         (%bound a {core-token #75 14} variable)
         (%bound b {core-token #75 15} variable))
        0)
       (quote bigger)
       ((lambda ((%bind b {core-token #75 16}) (%bind c {core-token #75 17}))
          ((%bound b {core-token #75 16} variable)
           (%bound a {core-token #75 14} variable)
           (%bound c {core-token #75 17} variable)))
        (%bound b {core-token #75 15} variable))))
 10 (%free a))

(begin
  (define s (datum->syntax
             '(letrec ((fib (lambda (n)
                              (if (= n 0)
                                  1
                                  (if (= n 1)
                                      1
                                      (+ (fib (- n 1)) (fib (- n 2))))))))
                (fib 7))))

  (display (syntax->datum (expand s))))

#;
'(letrec (((%bind fib {core-token #75 31})
          (lambda ((%bind n {core-token #75 32}))
            (if ((%free =) (%bound n {core-token #75 32} variable) 0)
                1
                (if ((%free =) (%bound n {core-token #75 32} variable) 1)
                    1
                    ((%free +)
                     ((%bound fib {core-token #75 31} variable)
                      ((%free -) (%bound n {core-token #75 32} variable) 1))
                     ((%bound fib {core-token #75 31} variable)
                      ((%free -) (%bound n {core-token #75 32} variable) 2))))))))
  ((%bound fib {core-token #75 31} variable) 7))
