#lang racket

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)


(require "utils.rkt")
(require "desugar.rkt")


; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)

#;(match ir-exp
    [`(let ([,xs ,e0s] ...) ,e1) 'let_blank]
    [`(lambda (,xs ...) ,e0) 'reg_lambda]
    [`(lambda ,x ,e) 'list_lambda]
    [`(apply ,e0 ,e1) 'apply_blank]
    [`(prim ,(? prim? p) ,e0s ...) 'prim_blank]
    [`(apply-prim ,(? prim? p) ,e0) 'apply-prim]
    [`(if ,e0 ,e1 ,e2) 'if_blank]
    [`(set! ,x ,e0) 'set!_blank]
    [`(call/cc ,e0) 'call_cc]
    [`(quote ,dat) 'empty_dat]
    [(? symbol? x) 'sym]
    [`(,e0 ,e1s ...) 'argument_application])


(define (assignment-convert e)
  (define (find-mutated e s)
    (match e
      [`(let ([,xs ,e0s] ...) ,e1) (foldl find-mutated (find-mutated e1 s) e0s)]
      [`(lambda (,xs ...) ,e0) (find-mutated e0 s)]
      [`(lambda ,x ,e0) (find-mutated e0 s)]
      [`(apply ,e0 ,e1) (find-mutated e1 (find-mutated e0 s))]
      [`(prim ,(? prim? p) ,e0s ...) (foldl find-mutated s e0s)]
      [`(apply-prim ,(? prim? p) ,e0) (find-mutated e0 s)]
      [`(if ,e0 ,e1 ,e2) (find-mutated e2 (find-mutated e1 (find-mutated e0 s)))]
      [`(set! ,x ,e0) (find-mutated e0 (set-add s x))]
      [`(call/cc ,e0) (find-mutated e0 s)]
      [`(quote ,dat) s]
      [(? symbol? x) s]
      [`(,e0 ,e1s ...) (foldl find-mutated (find-mutated e0 s) e1s)]))
  ; Suggestion: compute a set of mutated variables and save here.
  ; I.e., a set of all x where there exists a (set! x ...).
  (define mutated-vars (set->list (find-mutated e (set))))
  
  ; A strategy like this can help you to avoid boxing variables that don't need to be boxed
  ; and slowing down compiled programs as a result.
  (define (boxify e)
    (match e
      ; box mutated vars at initialization,
      ; e.g., (let ([x '()]) ...) -> (let ([x (prim make-vector '1 '())]) ...)
      ; What happens to (lambda (x) ...) if x is mutated?
      
      ; .. all all other forms in the language ...
      [`(let ([,xs ,e0s] ...) ,e1) `(let ,(map (lambda (x e0)
                                                  (if (set-member? mutated-vars x) `(,x (prim make-vector '1 ,(boxify e0))) `(,x ,(boxify e0)))) xs e0s) ,(boxify e1))]
      [`(lambda (,xs ...) ,e0) `(lambda ,xs (let ,(map (λ (x) (if (set-member? mutated-vars x) `[,x (prim make-vector '1 ,x)] `[,x ,x])) xs) ,(boxify e0)))]
      [`(lambda ,x ,e0) `(lambda ,x ,(if (set-member? mutated-vars x) `(let ([,x (prim make-vector '1 ,x)]) ,(boxify e0)) (boxify e0)))]
      [`(apply ,e0 ,e1) `(apply ,(boxify e0) ,(boxify e1))]
      [`(prim ,(? prim? p) ,e0s ...) `(prim ,p . ,(map boxify e0s))]
      [`(apply-prim ,(? prim? p) ,e0) `(apply-prim ,p ,(boxify e0))]
      [`(if ,e0 ,e1 ,e2) `(if ,(boxify e0) ,(boxify e1) ,(boxify e2))]
      [`(call/cc ,e0) `(call/cc ,(boxify e0))]
      [`(quote ,dat) e]
      [`(set! ,x ,e0)
       `(prim vector-set! ,x '0 ,(boxify e0))]
      [(? symbol? x)
       (if (set-member? mutated-vars x)
           `(prim vector-ref ,x '0)
           x)]
      [`(,e0 ,e1s ...) `(,(boxify e0) . ,(map boxify e1s))]))
  (boxify e))


; assignment-convert => 

;;; set! is removed and replaced with vector-set!
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

; alphatize both takes and produces this language as well

(define (alphatize e)
  ; Defining curried rename function is convenient for mapping over lists of es
  (define ((rename env) e)
    (match e
      ; Rename all variables 
      [`(lambda (,xs ...) ,e0)
       (define xs+ (map gensym xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(lambda ,xs+ ,((rename env+) e0))]
      [`(let ([,xs ,e0s] ...) ,e1)
       (define xs+ (map gensym xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(let ,(map (lambda (xs e0) `(,xs ,((rename env) e0))) xs+ e0s) ,((rename env+) e1))]
      [`(lambda ,x ,e)
       (define x+ (gensym x))
       (define env+ (hash-set env x x+))
       `(lambda ,x+ ,((rename env+) e))]
      [`(apply ,e0 ,e1) `(apply ,((rename env) e0) ,((rename env) e1))]
      [`(prim ,(? prim? p) ,e0s ...) `(prim ,p . ,(map (rename env) e0s))]
      [`(apply-prim ,(? prim? p) ,e0) `(apply-prim ,p ,((rename env) e0))]
      [`(if ,e0 ,e1 ,e2) `(if ,((rename env) e0) ,((rename env) e1) ,((rename env) e2))]
      [`(call/cc ,e0) `(call/cc ,((rename env) e0))]
      [`(quote ,dat) e]
      [(? symbol? x) (hash-ref env x)]
      [`(,e0 ,e1s ...) `(,((rename env) e0) . ,(map (rename env) e1s))]
           ))
  ((rename (hash)) e))


; Converts to ANF; adapted from Flanagan et al.
(define (anf-convert e)
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [(? symbol? x)
                      (k x)]
                     [`(lambda ,xs ,e0)
                      (k `(lambda ,xs ,e0))]
                     [`(quote ,dat) (k anf)]
                     [else
                      (define tempX (gensym 'temp))
                      `(let ([,tempX ,anf])
                         ,(k tempX))]))))
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es) (lambda (ae)
                                 (normalize-aes (cdr es)
                                                (lambda (aes)
                                                  (k `(,ae ,@aes))))))))
  (define (normalize-lets xs es body k)
    (if (null? xs)
        (k (anf-convert body))
        (normalize-lets (cdr xs) (cdr es) body
                        (lambda (sub-let)
                          (k `(let ([,(car xs) ,(anf-convert (car es))]) ,sub-let))))))
  (define (normalize e k)
    (match e
       [`(let ([,xs ,e0s] ...) ,e1) (normalize-lets xs e0s e1 k)] ;(let ([x1 e1][x2 e2]) e0) -> (let ([x1 e1]) (let ([x2 e2]) e0)))
       [`(lambda (,xs ...) ,e0) (k `(lambda ,xs ,(anf-convert e0)))]
       [`(lambda ,x ,e) (k `(lambda ,x ,(anf-convert e)))]
       [`(apply ,e0 ,e1) (normalize-ae e0 (lambda (ae0)
                                                   (normalize-ae e1 (lambda (ae1)
                                                                      (k `(apply ,ae0 ,ae1))))))]
       [`(prim ,(? prim? p) ,e0s ...) (normalize-aes e0s (lambda (aes) (k `(prim ,p ,@aes))))]
       [`(apply-prim ,(? prim? p) ,e0) (normalize-ae e0 (lambda (ae)
                                                          (k `(apply-prim ,p ,ae))))]
       [`(if ,e0 ,e1 ,e2) (normalize-ae e0 (lambda (ae)
                                             (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
       [`(call/cc ,e0) (normalize-ae e0 (lambda (ae) (k `(call/cc ,ae))))]
       [`(quote ,dat) (k e)]
       [(? symbol? x) (k x)]
       [`(,es ...) (normalize-aes es k)]))
  ; We will write a simplified version in class
  (normalize e (lambda (x) x)))


; anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


(define (cps-convert e)
  (define (T-ae ae)
    (match ae
      [(? symbol? x) x]
      [`(lambda (,x ...) ,e0)
       (define k (gensym 'k))
       `(lambda (,k ,@x) ,(T-e e0 k))] ;an atomic expression which is a lambda becomes a lambda which takes a continuation and passes the bodies result to it
      [`(lambda ,x ,e0)
       (define k (gensym 'k))
       (define p (gensym 'p))
       `(lambda ,p (let ([,k (prim car ,p)]) (let ([,x (prim cdr ,p)]) ,(T-e e0 k))))]; need to figure out prims
      [`(quote ,(? datum? d)) ae]
      ))
  (define (T-e e cae) ; sends the result of the expression to cae (the continuation)
    (match e
      [(? symbol? x)
       `(,cae ,x ,x)]
      [`(quote ,(? datum? dat)) `(,cae ,e ,e)]
      [`(lambda (,x ...) ,rest)
       `(,cae '0 ,(T-ae e))]
      [`(lambda ,x ,ce)
       `(,cae '0 ,(T-ae e))]
      ;complex expressions
      [`(let ([,x ,e0]) ,e1)
       (define ign (gensym 'ign))
       (T-e e0 `(lambda (,ign ,x) ,(T-e e1 cae)))] ;; transform e0 to cps form, then return it to x which will evaluate e1 with the value of x
      [`(if ,ae ,e0 ,e1)
       `(if ,ae ,(T-e e0 cae) ,(T-e e1 cae))] ;; leave the if results in tail position and pass them back to cae
      [`(apply ,e1 ,e2)
       (define x (gensym 'x))
       (define x2 (gensym 'x))
       (define k (gensym 'k))
       (define k2 (gensym 'k))
       (define args (gensym 'args))
       (T-e e1 `(lambda (,k ,x) ,(T-e e2 `(lambda (,k2 ,x2) (let ((,args (prim cons ,cae ,x2))) (apply ,x ,args))))))]
      [`(prim ,(? prim? p) ,aes ...)
       (define temp (gensym 'temp))
       `(let ((,temp (prim ,p ,@(map T-ae aes)))) (,cae '0 ,temp))]
      [`(apply-prim ,(? prim? p) ,ae)
       (define temp (gensym 'temp))
        `(let ((,temp (apply-prim ,p ,ae))) (,cae '0 ,temp))]
      [`(call/cc ,ae)
       `(,(T-ae ae) ,cae ,cae)]
      [`(,aef ,aes ...) 
       `(,(T-ae aef) ,cae ,@(map T-ae aes))] ; convert head to cps form, then pass it the same continuation  and the aes as arguments
      ))
  (define x (gensym 'x))
  (define k (gensym 'k))
  (T-e e `(lambda (,k ,x) (let ([_1 (prim halt ,x)]) (,k ,x)))))


; cps-convert => 

;e ::= (let ([x (apply-prim op ae)]) e)
;    | (let ([x (prim op ae ...)]) e)
;    | (let ([x (lambda (x ...) e)]) e)
;    | (let ([x (lambda x e)]) e)
;    | (let ([x (quote dat)]) e)
;    | (apply ae ae)
;    | (ae ae ...)
;    | (if ae e e)
;ae ::= (lambda (x ...) e)
;     | (lambda x e)
;     | x
;     | (quote dat)


