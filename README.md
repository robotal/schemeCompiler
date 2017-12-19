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
 - **make-hash**
     - method signature: (make-hash)
     - input types: none
     - behaviour: Returns a fresh mutable hash
 - **hash-set!**
     - method signature: (hash-set! h v v)
     - input types: h must be a hash created with **make-hash** and v is any pair of key/values.
     - behaviour: updates the hash table with a new entry for the given key value pair
- **hash-ref**
    - method signature: (hash-ref h v)
    - input types: h must be a hash and v must be a value of a key in the hash
    - behaviour: returns the value currently mapped in that hash, throws an exception if it does not exist.
# 4. Run-time errors

1. index out of bounds (vector)

    When calling vector-ref on an index larger than the length, no checks are made to see if the index is out of bounds. This causes the header to attempt to access memory which is garbage. Adding a check in `prim_vector_45ref` will now throw index out of bounds error.

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/vecBnds_0.scm
    (define v (make-vector 3 'hola))
    (vector-ref v 5)
    tal@Tals-MacBook-Pro$ racket make.rkt -o vecBnds_0 tests/public/vecBnds_0.scm
    tal@Tals-MacBook-Pro$ ./vecBnds_0
    library run-time error: Index out of bounds, given index 5 for vector of length 3.
    ```

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/vecBnds_1.scm
    (define v (vector 'hello 'world 5 'narwhals))
    (vector-ref v 10)
    tal@Tals-MacBook-Pro$ racket make.rkt -o vecBnds_1 tests/public/vecBnds_1.scm
    tal@Tals-MacBook-Pro$ ./vecBnds_1
    library run-time error: Index out of bounds, given index 10 for vector of length 4.
    ```

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

3. Too many arguments on lambda with default parameters

    When desugaring lambda with default parameters we made them into variadic lambdas which would then parse the argument list to see whether to use the default values or ones passed in. The list was never checked for whether extra arguments were passed in. By adding a null? check on the end of the list I was able to throw an exception at the top-level stage.

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/variadicApply_0.scm
    (define f (lambda (x y z (d 1) (e 4)) (+ x y z d)))
    (f 1 2 3 4 5 6)
    tal@Tals-MacBook-Pro$ racket make.rkt -o varApply_0 tests/public/variadicApply_0.scm
    tal@Tals-MacBook-Pro$ ./varApply_0
    "Library run-time error: Too many arguments applied to default lambda"
    ```

    Doesn't fail with correct amount of arguments
    ```
    tal@Tals-MacBook-Pro$ cat tests/public/variadicApply_1.scm
    (define f (lambda (x y z (d 1) (e 4) (g 5)) (+ x y z d e g)))
    (f 1 2 3 4 5 6)
    tal@Tals-MacBook-Pro$ racket make.rkt -o varApply_1 tests/public/variadicApply_1.scm
    tal@Tals-MacBook-Pro$ ./varApply_1
    21
    ```

4. No such key

    When trying to do a hash lookup on a key which does not exist, throws a run time error that the key does not exist.

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/noKey_1.scm
    (define h (make-hash))
    (hash-ref h "foo")
    tal@Tals-MacBook-Pro$ racket make.rkt -o noKey1 tests/public/noKey_1.scm
    tal@Tals-MacBook-Pro$ ./noKey1
    key given:
    "foo"
    library run-time error: No such key found
    ```

    ```
    tal@Tals-MacBook-Pro$ cat tests/public/noKey_2.scm
    (define h (make-hash))
    (hash-set! h 'foo 4)
    (hash-ref h "foo")
    tal@Tals-MacBook-Pro$ racket make.rkt -o noKey2 tests/public/noKey_2.scm
    tal@Tals-MacBook-Pro$ ./noKey2
    key given:
    "foo"
    library run-time error: No such key found
    ```

5. Recursive data structure

    If you look at the following code example, h is a hash which we store as the only element of a vector. We then set an arbitrary key in that hash to be that vector. This creates a looping data structure which we can not print out without some extra effort.

    ```
    (define h (make-hash))
    (define v (make-vector 1 h))
    (hash-set! h 1 v)
    h
    ```

    racket will evaluate this to `#0='#hash((1 . #(#0#)))`

    Our program will throw a `recursive data structure detected` error if it exceeds a depth of 1000 on traversal.

    ```
    tal@Tals-MacBook-Pro$ racket make.rkt -o infStruct tests/public/infStruct.scm
    tal@Tals-MacBook-Pro$ ./infStruct
    library run-time error: Recursive data structure detected.
    ```
# 5. Hash tables

Hash tables are implemented using c++ library std::unordered_map template. Two new methods have been implemented to do the heavy lifting of finding equal values. The function `bool data_equal(u64, u64)`, which returns true if two values (in our language) are equal. Additionally there is the function `std::size_t hash_data(u64)` which computes the hash value for a value in our language. If two values are equal under `data_equal` then the output of `hash_data` will be the same for them.

With these methods in place we write a struct named `Key` which stores a u64 value.

```
struct Key
{
  u64 m_key;

  bool operator==(const Key &other) const
  {
    u64 o_key = other.m_key;
    return data_equal(m_key, o_key);
  }
};
```

The `operator==` is overloaded to extract the u64 value from the other Key and then delegates to `data_equal` to do the actual computation.

With our Key data structure in place we define a hash template on this key as follows:

```
namespace std {

  template <>
  struct hash<Key>
  {
    std::size_t operator()(const Key& k) const
    {
      return hash_data(k.m_key);
    }
  };

}
```

Once again we overload the operator()(Key) and then have it delegate to the hash_data function.

Having implemented `data_equal` for the hash table, I also switched `prim_eqv` to use this function to actually compare values in the language. Unfortunately the function does not handle looping structures like racket does, and will indefinitely follow pointers. Currently the hard cap is 1000 on how deep to traverse a structure.

The actual prim functions for interacting with the hash can be found at the bottom of `header.cpp`.

1. **make-hash**

Similar to vectors, a hash is a tuple of u64 pointers with the first value storing the tag for hashes (instead of vectors). The second value is a casted pointer to the data structure itself.
```
//creates an empty hash
u64 prim_make_45hash(){

    std::unordered_map<Key, u64> *hashMap;
    hashMap = new std::unordered_map<Key, u64>();

    //printf("hashmap = %llu\n", hashMap);
    u64* ret = (u64*)alloc(2 * sizeof(u64));
    ret[0] = HASH_OTHERTAG;
    ret[1] = (u64)(hashMap); // there is no way this is safe

    return ENCODE_OTHER(ret);
}
```
2. **hash-set**

Originally I was trying to initialize hashMap to be an unordered_map instead of a pointer to one. This causes c++ to create a copy of the hash and promptly stack dump as it does not want to allocate this memory.
```
//supported key types
//int, symbol, string, cons, vector, hash ?
u64 prim_hash_45set_33(u64 h, u64 k, u64 v){
    ASSERT_TAG(h, OTHER_TAG, "First argument to hash_set must be a hash");

    if( (((u64*)DECODE_OTHER(h))[0])  != HASH_OTHERTAG){
        fatal_err("hash-set not given a proper hash");
    }

    u64 hashPtr = ((u64*)DECODE_OTHER(h))[1];
    std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
    //printf("hashmap = %llu\n", hashMap);

    Key m_key = {k};
    (*hashMap)[m_key] = v;
    return V_VOID;
}
```

3. **hash-ref**

Currently if there is no key found it will just throw an error since it is not clear what the expression should evaluate to. Would also be useful to provide a primitive function to check for existence of keys in the hash.
```
u64 prim_hash_45ref(u64 h, u64 k){
    ASSERT_TAG(h, OTHER_TAG, "First argument to hash-ref must be a hash");
    if( (((u64*)DECODE_OTHER(h))[0])  != HASH_OTHERTAG){
        fatal_err("hash-ref not given a proper hash");
    }


    u64 hashPtr = ((u64*)DECODE_OTHER(h))[1];
    std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
    Key m_key = {k};


    if((*hashMap).count(m_key) == 0){
        printf("key given: \n");
        prim_print(k);
        printf("\n");
        fatal_err("No such key found");
    }
    return (*hashMap)[m_key];
}
```


## Hash Table tests

Here are a few tests which test the features of hash tables:

### `make-hash.scm`:

```
racket make.rkt -o make-hash tests/public/make-hash.scm
```

This tests the 3 primitive functions and tests printing out the hash map. Note that this may not pass `tests.rkt` since the order of pairs is undefined between racket and c++.

```
(define h (make-hash))
(hash-set! h 3 4)
(hash-set! h "world" 'henlo)
(hash-set! h 'henlo "world")
(list (hash-ref h 3) (hash-ref h "world") (hash-ref h 'henlo) h)
```
#### Output:

```
'(4 . (henlo . ("world" . (#hash((henlo . "world") ("world" . henlo) (3 . 4)) . ()))))
```

### `hash-equal.scm` :

```
racket make.rkt -o hash-equal tests/public/hash-equal.scm
```
 Tests equivalency between hash tables with the same keys in them by `eqv?`. Note that h1 and h2 are `eqv` but not `eq`.

```
(define h1 (make-hash))
(define h2 (make-hash))
(define h3 (make-hash))

(hash-set! h1 "hello" 'world)
(hash-set! h2 "hello" 'world)
(hash-set! h3 "hello" "world")

(list (eq? h1 h2) h1 h2 (eq? 1 2) (eqv? 1 1) (eqv? h1 h2) (eqv? h1 h3))
```

#### Output:

```
 '(#f . (#hash(("hello" . world)) . (#hash(("hello" . world)) . (#f . (#t . (#t . (#f . ())))))))
 ```
