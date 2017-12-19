(define h1 (make-hash))
(define h2 (make-hash))
(define h3 (make-hash))

(hash-set! h1 "hello" 'world)
(hash-set! h2 "hello" 'world)
(hash-set! h3 "hello" "world")

(list (eq? h1 h2) h1 h2 (eq? 1 2) (eqv? 1 1) (eqv? h1 h2) (eqv? h1 h3))
