#lang racket

; by Tal Davidi

(provide desugar)
(require "utils.rkt")

;Output language:

;e ::= (let ([x e] ...) e)
;    | (lambda (x ...) e)
;    | (lambda x e)
;    | (apply e e)
;    | (e e ...)
;    | (prim op e ...) 
;    | (apply-prim op e)
;    | (if e e e)
;    | (set! x e)
;    | (call/cc e)
;    | x
;    | (quote dat)

(define (desugar-conds cond-list)
  (match cond-list
    ['() `(prim void)]
    [(cons `(,e1) rest) (define temp (gensym 'temp)) `(let ((,temp ,(desugar-aux e1))) (if ,temp ,temp ,(desugar-conds rest)))]
    [(cons `(else ,e1) rest) (desugar-aux e1)] ;else must be in tail position (ignore rest)
    [(cons `(,e1 ,e2) rest) `(if ,(desugar-aux e1) ,(desugar-aux e2) ,(desugar-conds rest))]
    ))

(define (desugar-cases case-list temp-name)
  (match case-list
    [(cons `((,d ...) ,e1) rest) `(if (prim member ,temp-name ,(list 'quote d)) ,(desugar-aux e1) ,(desugar-cases rest temp-name))]
    [(cons `(else ,e1) '()) (desugar-aux e1)]
    ['() `(prim void)]
    ))

(define (desugar-aux e)
  (match e
    [`(letrec* ([,xs ,e0s] ...) ,e1)
     (define temp_list (map gensym xs)) ;generate temp list of symbols
     (define let_body (foldr (lambda (x e0 temp acc) `(let ((,temp ,(desugar-aux e0))) (let ((,(gensym 'side) (set! ,x ,temp))) ,acc))) (desugar-aux e1) xs e0s temp_list))
     `(let ,(map (lambda (x) `(,x 'undefined)) xs) ,let_body)] ;set outer shell to be all undefined variables
    [`(letrec ([,xs ,e0s] ...) ,e1)
     (define temp_list (map gensym xs)) ;generate temp list of symbols
     `(let ,(map (lambda (x) `(,x 'undefined)) xs) (let ,(map (lambda (e0 temp) `(,temp ,(desugar-aux e0))) e0s temp_list) ,(desugar-aux (append (cons 'begin (map (lambda (x temp) `(set! ,x ,temp)) xs temp_list)) `(,e1)))))] ;set outer shell to be all undefined variables
    [`(let* ([,xs ,e0s] ...) ,e1)
     (foldr (lambda (x e0 acc) `(let ((,x ,(desugar-aux e0))) ,acc)) (desugar-aux e1) xs e0s)]
    [`(let ([,xs ,e0s] ...) ,body)
     `(let ,(map (lambda (var exp) `(,var ,(desugar-aux exp))) xs e0s) ,(desugar-aux body))]
    [`(let ,x ([,xs ,e0s] ...) ,e1)
     (desugar-aux `(letrec ([,x (lambda ,xs ,e1)]) (,x . ,e0s)))]
    [`(lambda (,xs ...) ,e)
     `(lambda ,xs ,(desugar-aux e))]
    [`(lambda ,(? symbol? x) ,e)
     `(lambda ,x ,(desugar-aux e))]
    [`(lambda ,(list-rest x ... opt_list) ,e1)
     (define cdr_list (foldl (lambda (x acc) (cons `(prim cdr ,(car acc)) acc)) (cons 'x '()) (cdr x)))
     (define car_list (map (lambda (x) `(prim car ,x)) cdr_list))
     (define rev_car (reverse car_list))
     (define opt_cdr `(prim cdr ,(car cdr_list)))
     `(lambda x (let ,(cons `(,opt_list ,opt_cdr) (map (lambda (x car) `(,x ,car)) x rev_car)) ,(desugar-aux e1)))]
    [`(dynamic-wind ,start ,body ,end)
     `(%dynamic-wind ,(desugar-aux start) ,(desugar-aux body) ,(desugar-aux end))]
    [`(guard (,x ,cond-clause ...) ,e1)
     (desugar-aux `(let ([cc (call/cc (lambda (k) k))])
                     (if (cons? cc)
                         (let ((,x (car cc))) (cond . ,(append cond-clause `((else (raise ,x))))))
                         (dynamic-wind
                          (lambda () (begin (set! %handler-stack (cons %exception-handler %handler-stack)) (set! %exception-handler cc)));setup-new-handler
                          (lambda () ,e1)
                          (lambda () (begin (set! %exception-handler (car %handler-stack)) (set! %handler-stack (cdr %handler-stack)))) ;revert to old handler
                          ))))]
    [`(raise ,e1)
     `(%exception-handler (prim cons ,(desugar-aux e1) '()))]
    [`(delay ,e1)
     `(prim list 'promise-tag (lambda () ,(desugar-aux e1)) (prim make-vector '1 'unresolved-tag))]
    [`(force ,e1)
     `(let ((prom  ,(desugar-aux e1)))
        (if ,(desugar-aux '(promise? prom))
            (let ((val
                   (prim vector-ref (prim car (prim cdr (prim cdr prom))) '0)))
              (if (prim equal? val 'unresolved-tag)
                  (let ([new-val ((prim car (prim cdr prom)))])
                    (let ((,(gensym 'side) (prim vector-set! (prim car (prim cdr (prim cdr prom))) '0 new-val)))
                      new-val))
                  val)
              )
            prom))]
    [`(and) ''#t]
    [`(and ,e0s ...)
     (define rev (reverse e0s))
     (define last-exp (car rev))
     (foldl (lambda (e0 acc) `(if ,(desugar-aux e0) ,acc '#f)) `(let ((exp ,(desugar-aux last-exp))) (if exp exp '#f)) (cdr rev))]
    [`(or ,e0s ...)
     (foldr (lambda (e0 acc) `(let ((val ,(desugar-aux e0))) (if val val ,acc))) ''#f e0s)]
    [`(cond ,cond-clause ...)
     (desugar-conds cond-clause)]
    [`(case ,e0 ,case-clause ...)
     (define temp (gensym 'temp))
     `(let ((,temp ,(desugar-aux e0))) ,(desugar-cases case-clause temp))]
    [`(if ,e1 ,e2 ,e3)
     `(if ,(desugar-aux e1) ,(desugar-aux e2) ,(desugar-aux e3))]
    [`(when ,e1 ,e2)
     `(if ,(desugar-aux e1) ,(desugar-aux e2) (prim void))]
    [`(unless ,e1 ,e2)
     `(if ,(desugar-aux e1) (prim void) ,(desugar-aux e2))]
    [`(set! ,x ,e1)
     `(set! ,x ,(desugar-aux e1))]
    [`(begin ,e0s ...)
     (define rev (reverse e0s))
     (foldl (lambda (e0 acc) `(let ((,(gensym 'side) ,(desugar-aux e0))) ,acc)) (desugar-aux (car rev)) (cdr rev))]
    [`(call/cc ,e1)
     `(call/cc ,(desugar-aux `(lambda (k)
                                (,e1  (let ([k-stack %wind-stack])
                                        (lambda (x)
                                          (begin (%do-wind k-stack)
                                                 (k x))))))))]
    [`(let/cc ,k ,body ...) (desugar-aux `(call/cc (lambda (,k) (begin ,body))))]
    [`(apply ,(? prim? p) ,e2)
     `(apply-prim ,p ,(desugar-aux e2))]
    [`(apply ,e1 ,e2)
     `(apply ,(desugar-aux e1) ,(desugar-aux e2))]
    [`(promise? ,e0)
     (desugar-aux `(let ([prom ,e0]) (if (cons? prom) (equal? 'promise-tag (car prom)) '#f)))]
    [`(,(? prim? p) ,es ...)
     `(prim ,p . ,(map desugar-aux es))]
    [(? prim? p) `(lambda args (apply-prim ,p args))]
    [`(quote ,(? datum? d)) e]
    [(? symbol? x) x]
    [`(,e1 ,e0s ...) ;untagged application
     `(,(desugar-aux e1) . ,(map desugar-aux e0s))]
    ))


(define (desugar e)
  ; wrap e in any special functions or definitions you need
  ; and then pass it into a helper function that performs the
  ; case-by-case translation recursively
  (desugar-aux (wrap-with-lib e)))

(define (wrap-with-lib e)
    `(let* ([%wind-stack '()]
            [common-tail (lambda (x y)
                           (let ((lx (length x))
                                 (ly (length y)))
                             (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                        [y (if (> ly lx) (drop y (- ly lx)) y)])
                               (if (eq? x y)
                                   x
                                   (loop (cdr x) (cdr y))))))]
            [%do-wind (lambda (new)
                        (unless (eq? new %wind-stack) 
                                (let ([tail (common-tail new %wind-stack)])
                                  (begin
                                    (let f ((l %wind-stack))
                                      (unless (eq? l tail)
                                              (begin
                                                (set! %wind-stack (cdr l))
                                                ((cdr (car l)))
                                                (f (cdr l)))))
                                    (let f ([l new])
                                      (unless (eq? l tail)
                                              (begin
                                                (f (cdr l))
                                                ((car (car l)))
                                                (set! %wind-stack l))))))))]
            [%dynamic-wind (lambda  (pre body post)
                              (begin
                                (pre)
                                (set! %wind-stack (cons (cons pre post) %wind-stack))
                                (let ([v (body)])
                                  (begin
                                    (set! %wind-stack (cdr %wind-stack))
                                    (post)
                                    v))))]
            [%exception-handler '()]
            [%handler-stack '()])
           ,e))



; I, Tal Davidi, pledge on my honor that I have not given or 
; received any unauthorized assistance on this project.
