#lang racket

; Run this file or use the following at the REPL
(require "cps.rkt")
(require "utils.rkt")
(require "desugar.rkt")
(require "closure-convert.rkt")
(require "top-level.rkt")

;(define args (vector->list (current-command-line-arguments)))

(define fileName (make-parameter "bin"))

(define toCompile
    (command-line
        #:once-each
        [("-o" "--output") file "Output file executable" (fileName file)]
        #:args (filename) filename))

; Load the test we're testing, perhaps amb.scm
(define top (with-input-from-file toCompile read-begin #:mode 'text))

; Convert to cps, note that simplify-ir is added after the call to desugar
(define scm (top-level top))
;(display scm)
(define cps (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar scm)))))))
;(cps-exp? cps)

; Phases 1 and 2
(define p (closure-convert cps))
;(eval-proc p)
(define llvm (proc->llvm p))
(eval-llvm llvm #f (fileName))
