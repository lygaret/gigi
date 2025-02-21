(define (curry f . pre-args)
  (lambda args
    (apply f (append pre-args args))))

(define (curryr f . post-args)
  (lambda args
    (apply f (append args post-args))))
