#lang racket

(require "utils.rkt")
(require "cps.rkt")
(require "desugar.rkt")

(provide closure-convert
         proc->llvm)

(define (up-to-cps scm)
  (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar scm)))))))

(define (already-ir ir)
  (cps-convert (anf-convert (alphatize (assignment-convert ir)))))

; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
                  (foldr (lambda (ae xs+wrap)
                           (define gx (gensym 'arg))
                           (if (symbol? ae)
                               (cons (cons ae (car xs+wrap))
                                     (cdr xs+wrap))
                               (cons (cons gx (car xs+wrap))
                                     (lambda (e)
                                       (match ae
                                              [`(lambda ,xs ,body) 
                                               `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                                  ,((cdr xs+wrap) e))]
                                              [`',dat
                                               `(let ([,gx ',dat])
                                                  ,((cdr xs+wrap) e))])))))
                         (cons '() wrap)
                         aes))
    (wrap+ xs))
  (define (letbind ae)
    (match ae
      [`(lambda ,xs ,body)
       `(lambda ,xs ,(simplify-ae body))]
      [`',dat
       ae]))
  (match e
         [`(let ([,x (lambda ,xs ,elam)]) ,e0)
          `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(simplify-ae e0))]

         [`(let ([,x (prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]
         [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]

         [`(if (lambda . ,_) ,et ,ef)
          (simplify-ae et)]
         [`(if '#f ,et ,ef)
          (simplify-ae ef)]
         [`(if ',dat ,et ,ef)
          (simplify-ae et)]
         [`(if ,(? symbol? x) ,et ,ef)
          `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

         [`(apply ,ae0 ,ae1)
          (define arg0 (gensym 'arg))
          (define arg1 (gensym 'arg))
          (if (symbol? ae0)
              (if (symbol? ae1)
                  e
                  `(let ([,arg1 ,(letbind ae1)]) (apply ,arg0 ,arg1)))
              (if (symbol? ae1)
                  `(let ([,arg0 ,(letbind ae0)]) (apply ,arg0 ,ae1))
                  `(let ([,arg0 ,(letbind ae0)]) (let ([,arg1 ,(letbind ae1)]) (apply ,arg0 ,arg1)))))
             
          ]
         
         [`(,aes ...)
          (wrap-aes aes (lambda (xs) xs))]))

;Language after simplify-ae
;e ::= (let ([x (apply-prim op ae)]) e)
;    | (let ([x (prim op ae ...)]) e)
;    | (let ([x (lambda (x ...) e)]) e)
;    | (let ([x (lambda x e)]) e)
;    | (let ([x (quote dat)]) e)
;    | (apply ae ae)
;    | (ae ae ...)
;    | (if ae e e)
;ae ::= x
; Helper to remove vararg lambdas/callsites

(define (remove-varargs e) 
  (match e
         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(remove-varargs e0))]
         [`(let ([,x (prim ,op ,xs ...)]) ,e0)
          `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
         [`(let ([,x (apply-prim ,op ,y)]) ,e0)
          `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
         [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
          ; turns (xs ...) into x and immediately into (x)
          ; by adding the needed car/cdr calls and let bindings
          (define gx (gensym 'rvp))
          (define gx+e
            (foldr (lambda (x gx+e)
                     (define gx (gensym 'rvp))
                     (cons gx
                           `(let ([,x (prim car ,gx)])
                              (let ([,(car gx+e) (prim cdr ,gx)])
                                ,(cdr gx+e)))))
                   (cons (gensym 'na) (remove-varargs body))
                   xs))
          `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
             ,(remove-varargs e0))]
         [`(let ([,x (lambda ,y ,body)]) ,e0)
          `(let ([,x (lambda (,y) ,(remove-varargs body))])
             ,(remove-varargs e0))]
         [`(if ,x ,e0 ,e1)
          `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
         [`(apply ,f ,args)
          `(,f ,args)] 
         [`(,f ,xs ...) ;(func arg1 arg2 arg3) -> (let ([p1 cons (arg3 '())] (let ([p2 (cons (arg2 p1)]) (let ([p3 (cons arg1 p2)]) (func p3))))))
          (define (listify arglist body next-var)
            (define genvar (gensym 'arg))
            (if (null? arglist)
                `(let ([,next-var '()]) ,body)
                (listify (cdr arglist)
                         `(let ([,next-var (prim cons ,(car arglist) ,genvar)]) ,body)
                         genvar)))
          (define args (gensym 'arg))
          (listify xs `(,f ,args) args)])) 


;Language after remove-varargs
;e ::= (let ([x (apply-prim op ae)]) e)
;    | (let ([x (prim op ae ...)]) e)
;    | (let ([x (lambda (x) e)]) e)
;    | (let ([x (quote dat)]) e)
;    | (ae ae)
;    | (if ae e e)
;ae ::= x
; Helper to remove vararg lambdas/callsites

(define (closure-convert-h simple-cps)
  ; Exp x List[Proc] -> Exp x Set[Var] x List[Proc]
  (define (bottom-up e procs)
    (match e
      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]
      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]
      [`(let ([,x (apply-prim ,op ,xs)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
         (bottom-up e0 procs))
       `((let ([,x (apply-prim ,op ,xs)]) ,e0+)
         ,(set-remove (set-union free+ (set xs)) x)
         ,procs+)]
      [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
                     (bottom-up body procs0+))
       (define env-vars (foldl (lambda (x fr) (set-remove fr x))
                               freelam+
                               xs))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                     (cons (+ 1 cnt)
                                           `(let ([,x (env-ref ,envx ,cnt)])
                                              ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,@xs) ,body++) . ,procs1+))]
      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
                     (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]
      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]))
  (match-define `(,main-body ,free ,procs) (bottom-up simple-cps '()))
  `((proc (main) ,main-body) . ,procs))

; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  ; case not written; see our livecoding from class
  (closure-convert-h no-varargs-cps))

; After closure-convert (working correctly)
; 
; p ::= ((proc (x x ...) e) ...)
; e ::= (let ([x (apply-prim op x)]) e)
;     | (let ([x (prim op x ...)]) e)
;     | (let ([x (make-closure x x ...)]) e)
;     | (let ([x (env-ref x nat)]) e)
;     | (let ([x (quote dat)]) e)
;     | (clo-app x x ...)
;     | (if x e e)

; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define (proc->llvm procs)
  (define str-set '())
  (define (T-p proc)
    (match proc
      [`(proc (main) ,body)
       (define ll-body (T-e body (hash)))
       (define const-set (set))
       (string-append
        "define i64 @main(i32, i8**) {\n"
        ll-body
        "unreachable\n"
        "ret i64 0\n"
        "}\n\n")]
      [`(proc (,func ,env-name ,arglist) ,body)
       (define env-arg (string-append "%" (symbol->string env-name)))
       (define arg-arg (string-append "%" (symbol->string arglist)))
       (define ll-body (T-e body (hash env-name env-arg arglist arg-arg)))
       (string-append
        "define i64 @" (symbol->string func) "(i64 " env-arg ", i64 " arg-arg ") {\n"
        ll-body
        "unreachable\n"
        "ret i64 0\n"
        "}\n\n")]
    ))
    (define (T-e e var-names)
      (match e
        [`(let ([,x (apply-prim ,op ,arg)]) ,e)
         (define var-name (string-append "%" (symbol->string (gensym 'var))))
         (define ext-vars (hash-set var-names x var-name))
         (define rest (T-e e ext-vars))
         (string-append
          var-name " = call i64 @" (prim-applyname op) "(i64 " (hash-ref var-names arg) ")\n"
          rest)]
        [`(let ([,x (prim ,op ,xs ...)]) ,e)
         (define var-name (string-append "%" (symbol->string (gensym 'var))))
         (define ext-vars (hash-set var-names x var-name))
         (define rest (T-e e ext-vars))
         (define arglist
           (if (null? xs)
               ""
               (foldl
                (lambda (x str)
                  (string-append str ", i64 " (hash-ref var-names x)))
                (string-append "i64 " (hash-ref var-names (car xs)))
                (cdr xs))))
         (string-append
          var-name " = call i64 @" (prim-name op) "(" arglist ")\n"
          rest)]
        [`(let ([,x (make-closure ,name ,xs ...)]) ,e)
         (define var-name (string-append "%" (symbol->string (gensym 'var))))
         (define ext-vars (hash-set var-names x var-name))
         (define rest (T-e e ext-vars))
         (define vec-name (gensym 'vec-name))
         (define vec-len (gensym 'vec-len))
         (string-append
          "%" (symbol->string vec-len) " = call i64 @const_init_int(i64 " (number->string (+ 1 (length xs))) ")\n"
          "%" (symbol->string vec-name) " = call i64 @prim_make_45vector(i64 %" (symbol->string vec-len) ", i64 %" (symbol->string vec-len) ")\n"
          (cdr (foldl
                (lambda (x cnt+str)
                  (match-define (cons cnt str) cnt+str)
                  (define index (gensym 'index))
                  (cons (+ cnt 1) (string-append str
                                 "%" (symbol->string index) " = call i64 @const_init_int(i64 " (number->string cnt) ")\n"
                                 "call i64 @prim_vector_45set_33(i64 %" (symbol->string vec-name)
                                 ", i64 %" (symbol->string index) ", i64 " (hash-ref var-names  x) ")\n")))
                (cons 1 "")
                xs))
          var-name "= call i64 @make_closure(i64 (i64, i64)* @" (symbol->string name) ", i64 %" (symbol->string vec-name) ")\n"
          rest)]
        [`(let ([,x (env-ref ,env ,(? natural? n))]) ,e)
         (define var-name (string-append "%" (symbol->string (gensym 'var))))
         (define ext-vars (hash-set var-names x var-name))
         (define rest (T-e e ext-vars))
         (define index (gensym 'index))
         (string-append
          "%" (symbol->string index) " = call i64 @const_init_int(i64 " (number->string n) ")\n"
          var-name " = call i64 @prim_vector_45ref(i64 %" (symbol->string env) ", i64 %" (symbol->string index) ")\n"
          rest)]
        [`(let ([,x ',dat]) ,e)
         (define var-name (string-append "%" (symbol->string (gensym 'var))))
         (define ext-vars (hash-set var-names x var-name))
         (define rest (T-e e ext-vars))
         (string-append
          (match dat
            #;[`(,p1 . ,p2)
               ]
            ['()
             (string-append
              var-name "= call i64 @const_init_null()\n")]
            [(? string? s)
             (define str (gensym 'str))
             (define init-str
               (string-append
                "@." (symbol->string str) " = private unnamed_addr constant [" (number->string (+ 1 (string-length s))) " x i8] c\"" s "\\00\", align 8"))
             (set! str-set (cons init-str str-set))
             (define str-ptr (gensym 'str-ptr-ptr))
             (define actual-str (gensym 'str-ptr))
             (string-append
              "%" (symbol->string str-ptr) " = alloca i8*, align 8\n"
              "store i8* getelementptr inbounds (["(number->string (+ 1 (string-length s)))" x i8], [" (number->string (+ 1 (string-length s))) " x i8]* @." (symbol->string str) ", i32 0, i32 0), i8** %" (symbol->string str-ptr)", align 8\n"
              "%" (symbol->string actual-str) " = load i8*, i8** %" (symbol->string str-ptr) ", align 8\n"
              var-name " = call i64 @const_init_string(i8* %" (symbol->string actual-str) ")\n" 
              )]
            [(? natural? n)
             (string-append
              var-name "= call i64 @const_init_int(i64 " (number->string n) ")\n")]
            [(? symbol? s)
             (define str (gensym 'str))
             (define init-str
               (string-append
                "@." (symbol->string str) " = private unnamed_addr constant [" (number->string (+ 1 (string-length (symbol->string s)))) " x i8] c\"" (symbol->string s) "\\00\", align 8\n"))
             (set! str-set (cons init-str str-set))
             (define str-ptr (gensym 'str-ptr-ptr))
             (define actual-str (gensym 'str-ptr))
             (string-append
              "%" (symbol->string str-ptr) " = alloca i8*, align 8\n"
              "store i8* getelementptr inbounds (["(number->string (+ 1 (string-length (symbol->string s))))" x i8], [" (number->string (+ 1 (string-length (symbol->string s)))) " x i8]* @." (symbol->string str) ", i32 0, i32 0), i8** %" (symbol->string str-ptr)", align 8\n"
              "%" (symbol->string actual-str) " = load i8*, i8** %" (symbol->string str-ptr) ", align 8\n"
              var-name " = call i64 @const_init_symbol(i8* %" (symbol->string actual-str) ")\n" 
              )
             ]
            [(? boolean? b)
             (if b
                 (string-append var-name " = call i64 @const_init_true()\n")
                 (string-append var-name " = call i64 @const_init_false()\n"))])
          rest)
         ]
        [`(clo-app ,name ,arglist)
         (define na (gensym 'na))
         (string-append "%"
                        (symbol->string na) "= call i64 @closure_apply(i64 " (hash-ref var-names name) ", i64 " (hash-ref var-names arglist)")\n")]
        [`(if ,x ,e0 ,e1)
         (define rest0 (T-e e0 var-names))
         (define rest1 (T-e e1 var-names))
         (define guard (gensym 'guard))
         (define true-lab (gensym 'label))
         (define false-lab (gensym 'label))
         (define post-lab (gensym 'label))
         (define false-val (gensym 'false))
         (string-append
          "%" (symbol->string guard) "= icmp ne i64 " (hash-ref var-names x) ", 15\n"
          "br i1 %" (symbol->string guard) ", label %" (symbol->string true-lab) ", label %" (symbol->string false-lab) "\n\n"
          (symbol->string true-lab) ":\n"
          rest0
          "br label %" (symbol->string post-lab) "\n"
          (symbol->string false-lab) ":\n"
          rest1
          "br label %" (symbol->string post-lab) "\n"
          (symbol->string post-lab) ":\n")
         ]))
  (define proc-strings
    (foldl (lambda (proc s) (string-append s (T-p proc)))
           "; Function Attrs: noinline ssp uwtable
define i64 @make_closure(i64 (i64, i64)*, i64) #0 {
  %3 = alloca i64 (i64, i64)*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  store i64 (i64, i64)* %0, i64 (i64, i64)** %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = call i64* @alloc(i64 16)
  store i64* %6, i64** %5, align 8
  %7 = load i64 (i64, i64)*, i64 (i64, i64)** %3, align 8
  %8 = ptrtoint i64 (i64, i64)* %7 to i64
  %9 = load i64*, i64** %5, align 8
  %10 = getelementptr inbounds i64, i64* %9, i64 0
  store i64 %8, i64* %10, align 8
  %11 = load i64, i64* %4, align 8
  %12 = load i64*, i64** %5, align 8
  %13 = getelementptr inbounds i64, i64* %12, i64 1
  store i64 %11, i64* %13, align 8
  %14 = load i64*, i64** %5, align 8
  %15 = ptrtoint i64* %14 to i64
  ret i64 %15
}

; Function Attrs: noinline ssp uwtable
define i64 @closure_apply(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  %6 = alloca i64 (i64, i64)*, align 8
  %7 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  store i64* %10, i64** %5, align 8
  %11 = load i64*, i64** %5, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 0
  %13 = load i64, i64* %12, align 8
  %14 = inttoptr i64 %13 to i64 (i64, i64)*
  store i64 (i64, i64)* %14, i64 (i64, i64)** %6, align 8
  %15 = load i64*, i64** %5, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 1
  %17 = load i64, i64* %16, align 8
  store i64 %17, i64* %7, align 8
  %18 = load i64 (i64, i64)*, i64 (i64, i64)** %6, align 8
  %19 = load i64, i64* %7, align 8
  %20 = load i64, i64* %4, align 8
  %21 = call i64 %18(i64 %19, i64 %20)
  ret i64 %21
}\n\n"
           procs))
  (foldr
   (lambda (init-str str)
     (string-append init-str str))
   proc-strings
   str-set))





