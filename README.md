# Scheme Compiler in racket
#### By Tal Davidi

###### I, Tal Davidi, pledge on my honor that I have not given or received any unauthorized assistance on this assignment.

# 1. Running the Compiler

After cloning the repository and having set up racket and clang, you are ready to compile scheme files into executables.

Make sure you have at least _racket 6.10_ and _clang 3.9_ installed in order to run the compiler.

In order to make an executable file you must place the scheme file under the `tests/` directory
in any of the subdirectories `public/`, `release/` or `secret/`. When running the command `racket tests.rkt` you should see the name of your program without the `.scm` ending.
Running `racket tests.rkt [name]` will then compile the scheme file to an executable `bin` and test if the value outputted is the same as when run by the scheme interpreter.

Alternatively, you can use the `racket make.rkt` script provided to directly compile to a binary file and then run it without testing. This will allow you to see the output of your program if an error occurs. Run it using `racket make.rkt [-o outFile] fileName`. You can also display a help message using `racket make.rkt -h`.

# 2. Overview

The compiler itself is divided into 4 phases. Each phase simplifies the input language until it is trivial to convert to llvm which can then be compiled to a binary using clang.

### 1. top-level conversion
The aim of top-level conversion is to take a generic program written in an iterative fashion and to convert it into a functional style by wrapping multiple expressions in `begin` statements and pulling `define` statements at the same level into a `letrec` around the body.

### 2. desugar
The next step removes many of the forms of the scheme language and simplifies them to a series of simpler operations. Some notable difficulties at this point are handling the interaction between `call/cc` and `dynamic-wind`, due to the fact that a continuation created under a `dynamic-wind` must restore any set-up or breakdown code which occurred at the time of creation of the continuation. Another one is how to encode promises using only primitive data structures and lambdas. This is done by creating a list which holds a promise tag, the expression to evaluate, and a vector holding the return value after forcing the promise once.

### 3. assignment-convert, alphatize, anf-convert, cps-convert
The next phase can be split into 4 smaller phases which prepare the input for the last phase, cps-conversion.
- **assignment convert**

    A rather simple pass which removes all uses of `set!` in the program by converting its initial binding point to be a `make-vector` operation. Access to that variable is then replaced with `vector-ref` and `set!` is replaced with `vector-set`.

-  **alphatize**

    The next phase takes every single variable and renames it to a unique name, thus converting the program to SSA. This later allows us to directly transform our variable bindings into unique registers in LLVM.

- **anf-convert** (administrative normal form)

    The next step takes all expressions and let binds complex operations so that everything is an atomic expression or a complex expression in tail position.

- **cps-convert** (continuation passing style)

    conversion to cps makes it so that the program no longer returns values, instead passing a continuation from function to function which tells it where to pass the result of its computation. cps builds off the previous step which lifted complex expressions to let-bind them so that every operation is performed only on atomic expressions. At this point we can also get rid of `call/cc` by passing the lambda its continuation as an argument.

### 4. closure-conversion, llvm emission

- **simplify-ae, remove-varargs**

    Prior to closure-conversion we perform 2 small passes to let bind a few more expressions as atomic expressions such as lambdas and datums. At this point the only kind of atomic expression is just a variable name.

    Next we convert each lambda function from taking any amount of arguments (or a variable list of arguments) in such a way that each function now takes a list of arguments as its only argument. Thus when we simplify to llvm each function only takes one argument.

- **closure-convert**

    The aim of closure conversion is to remove all lambdas. In order to do this we must first compute the set of free variables in the body of a lambda and then passing it in as an additional argument. The environment itself is a vector corresponding to one free variable per item. We then convert each binding of a lambda to a `make-closure` call which takes the name of a procedure (the corresponding lambda body) and a vector of values for the free variables in the lambda body. In the beginning of each procedure we unpack the environment to set up values for each argument to be used by the function using `env-ref`. We also pass one argument to the lambda which is the corresponding argument list to the function. Because of cps-conversion, every application of any arbitrary symbols must be applying one function to another, which we replace with the `clo-app` (short for closure-apply) call.

- **proc->llvm**

    Once we have gotten rid of all lambdas in the program, it is very simple to convert this language into llvm output. For each procedure we generate a function of two arguments which takes the argument list and environment and for the body we recursively generate lines for each expression. primitive functions are delegated to function calls written in c++ and then compiled down to llvm using clang. For closures we create a special data type which contains a function pointer and a vector environment. To apply this closure on an argument list we unpack the closure object and pass into the function pointer the environment we had stored with the argument list. Supported datums at this point are strings, symbols, naturals, and booleans, which are instantiated using a constant initialization function.

# 3. Supported Primitive Operations
 - **\+**
     - method signature: (+ num ...)
     - input types: num must be a number
     - behaviour: Returns the sum of all numbers given (0 if none)
 - **\***
     - method signature: (* num ...)
     - input types: num must be a number
     - behaviour: Returns the product of all numbers given (1 if none)
 - **\-**
     - method signature: (- num ...+)
     - input types: num must be a number
     - behaviour: Returns the first value minus the rest of the values. If no extra values are given, negates the given value.
 - **\/**
     - method signature: (/ num num)
     - input types: num must be a number
     - behaviour: Returns the first value divided by the second value.
 - **\>**
     - method signature: (> a b)
     - input types: a and b are numbers
     - behaviour: returns true if a is greater than b
 - **\>=**
     - method signature: (>= a b)
     - input types: a and b are numbers
     - behaviour: returns true if a is greater than or equal to b
 - **\<=**
     - method signature: (<= a b)
     - input types: a and b are numbers
     - behaviour: returns true if a is less than or equal to b
 - **\<**
    - method signature: (< a b)
    - input types: a and b are numbers
    - behaviour: returns true if a is less than b
 - **eq?**
     - method signature: (eq? v1 v2)
     - input types: v1 and v2 are arbitrary values
     - behaviour: returns true if v1 and v2 point to the same memory
 - **eqv?**
     - Same as eq? in our compiler
 - **list?**
     - method signature: (list? lst)
     - input types: list is any arbitrary data type
     - behaviour: returns true if lst is a well formed list of cons cells.
 - **length**
     - method signature: (length lst)
     - input types: lst is a list
     - behaviour: Returns the length of the list passed in.
 - **list**
     - method signature: (list e ...)
     - input types: e can be any arbitrary value
     - behaviour: Returns a list of all the expressions given
 - **first**
     - method signature: (first lst)
     - input types: lst is a list
     - behaviour: returns the first element of the list
 - **second**
     - method signature: (second lst)
     - input types: lst is a list
     - behaviour: returns the second element of the list (cadr)
 - **third**
     - method signature: (third lst)
     - input types: lst is a list
     - behaviour: returns the third element of the list (caddr)
 - **fourth**
     - method signature: (first lst)
     - input types: lst is a list
     - behaviour: returns the fourth element of the list (cadddr)
 - **last**
     - method signature: (last lst)
     - input types: lst is a list
     - behaviour: returns the last element of the list
 - **cons**
     - method signature: (cons v1 v2)
     - input types: v1 and v2 are arbitrary values
     - behaviour: Creates a cons pair of the two elements
 - **cons?**
     - method signature: (cons? v)
     - input types: v is an arbitrary value
     - behaviour: Returns true if v is a cons cell
 - **car**
     - method signature: (car v)
     - input types: v must be a cons cell
     - behaviour: Returns the first element of the pair v
 - **cdr**
     - method signature: (cdr v)
     - input types: v must be a cons cell
     - behaviour: Returns the second element of the pair v
 - **null?**
     - method signature: (null? v)
     - input types: v can be any value
     - behaviour: Returns true if v is a null value.
 - **foldl**
     - method signature: (foldl proc e lst ...+)
     - input types: proc is a procedure, e is any value, lst must be a list (all same length)
     - behaviour: The procedure given must take the same amount of arguments as the number of lists plus 1. The first argument is the current accumulator and each of the other arguments is the current element of each list (processed left to right). The value returned is fed to the next call and the last value is returned by foldl.
 - **foldr**
     - method signature: (foldr proc e lst ...+)
     - input types: proc is a procedure, e is any value, lst must be a list (all same length)
     - behaviour: The procedure given must take the same amount of arguments as the number of lists plus 1. The first argument is the current accumulator and each of the other arguments is the current element of each list (processed right to left). The value returned is fed to the next call and the last value is returned by foldr.
 - **map**
     - method signature: (map proc lst ...+)
     - input types: proc is a procedure, lst must be a list (all same length)
     - behaviour: The procedure must take a number of arguments equal to the number of lists. Processes each element of the lists in order and creates a list of values returned from calling proc on each corresponding element. The return value is a list of the same length as all the lists.
 - **memv**
     - method signature: (memv val lst)
     - input types: val is any data type, lst is a list
     - behaviour: returns the tail of the list after the element. Uses eqv? to compare equality
 - **append**
     - method signature: (append lst1 lst2)
     - input types: lst1 and lst2 are lists
     - behaviour: Returns a new list which is the result of appending the two lists passed in.
 - **drop**
     - method signature: (drop lst num)
     - input types: lst is a list, num is a positive number
     - behaviour: returns the list with the first num elements removed
 - **take**
     - method signature: (take lst num)
     - input types: lst is a list and num is a number
     - behaviour: Returns a new list with the first num items from the list passed in
 - **drop-right**
     - method signature: (drop-right lst num)
     - input types: lst is a list and num is a number
     - behaviour: drops the first num elements from the right of lst and returns a new list.
 - **print**
     - method signature: (print v)
     - input types: v must be a data type
     - behaviour: Prints out the value v to the console
 - **halt**
     - method signature: (halt v)
     - input types: v must be a data type
     - behaviour: Prints out the value v to the console and exits
 - **vector**     
     - method signature: (vector e ...)
     - input types: e can be any arbitrary value
     - behaviour: Returns a fixed length vector with all of the given expressions
 - **make-vector**
     - method signature: (make-vector n v)
     - input types: n must be a number, v can be any value
     - behaviour: Creates a vector of length n and initializes each element to the value v.
 - **vector-ref**
     - method signature: (make-vector vec n)
     - input types: vec must be a vector, n must be a number
     - behaviour: Returns the item at index v
 - **void**
     - method signature: (void)
     - input types: no arguments
     - behaviour: Returns the primitive value void
 - **promise?**
     - method signature: (promise? p)
     - input types: p can be any value
     - behaviour: Returns true if p is a promise.

# 4. Run-time errors

1. index out of bounds (vector)

2. division by 0
    When attempting to divide by 0, the compiler would normally just hang and infinite loop. After adding a check in header.cpp to `prim__47` it will now throw a division by zero error. The two tests `div0_1.scm` and `div0_2.scm` will both check for this.

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/div0_1.scm
    (define a 3)
    (define b 0)
    (/ a b)
    tal@Tals-MacBook-Prot$ racket make.rkt -o div0_1 tests/public/div0_1.scm
    tal@Tals-MacBook-Pro$ ./div0_1
    library run-time error: Division by 0
    ```

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/div0_2.scm
    (define b 10)
    (map (lambda (a) (/ b a)) '(1 2 3 4 0))
    tal@Tals-MacBook-Pro$ racket make.rkt -o div0_2 tests/public/div0_2.scm
    tal@Tals-MacBook-Pro$ ./div0_2
    library run-time error: Division by 0
    ```
