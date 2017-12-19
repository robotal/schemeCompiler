

//#include "gc.h"    // Add back in and change tags if we want to use GC
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include <unordered_map>
#include <cstring>
#include <string>

#define CLO_TAG 0
#define CONS_TAG 1
#define INT_TAG 2
#define STR_TAG 3
#define SYM_TAG 4
#define OTHER_TAG 6
#define ENUM_TAG 7


#define VECTOR_OTHERTAG 1
#define HASH_OTHERTAG 2
// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0



#define MASK64 0xffffffffffffffff // useful for tagging related operations


#define ASSERT_TAG(v,tag,msg) \
    if(((v)&7ULL) != (tag)) \
        fatal_err(msg);

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);


#define DECODE_CLO(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_CLO(v) (((u64)(v)) | CLO_TAG)

#define DECODE_CONS(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_CONS(v) (((u64)(v)) | CONS_TAG)

#define DECODE_INT(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
#define ENCODE_INT(v) ((((u64)((u32)(v))) << 32) | INT_TAG)

#define DECODE_STR(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_STR(v) (((u64)(v)) | STR_TAG)

#define DECODE_SYM(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_SYM(v) (((u64)(v)) | SYM_TAG)

#define DECODE_OTHER(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_OTHER(v) (((u64)(v)) | OTHER_TAG)


// some apply-prim macros for expecting 1 argument or 2 arguments
#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    }

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    }

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    }





// No mangled names
extern "C"
{



typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;



// UTILS


u64* alloc(const u64 m)
{
    return (u64*)(malloc(m));
    //return new u64[m];
    //return (u64*)GC_MALLOC(m);
}

void fatal_err(const char* msg)
{
    printf("library run-time error: ");
    printf("%s", msg);
    printf("\n");
    exit(1);
}

void print_u64(u64 i)
{
    printf("%llu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    ASSERT_TAG(args, CONS_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.")
    u64* p = DECODE_CONS(args);
    ASSERT_VALUE((p[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.")
    return p[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p)
    ASSERT_TAG(p, CONS_TAG, "Expected a cons value. (expect_cons)")

    u64* pp = DECODE_CONS(p);
    *rest = pp[1];
    return pp[0];
}

u64 expect_other(u64 v, u64* rest)
{
    // returns the runtime tag value
    // puts the untagged value at *rest
    ASSERT_TAG(v, OTHER_TAG, "Expected a vector or special value. (expect_other)")

    u64* p = DECODE_OTHER(v);
    *rest = p[1];
    return p[0];
}


/////// CONSTANTS


u64 const_init_int(s64 i)
{
    return ENCODE_INT((s32)i);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}


u64 const_init_false()
{
    return V_FALSE;
}


u64 const_init_string(const char* s)
{
    return ENCODE_STR(s);
}

u64 const_init_symbol(const char* s)
{
    return ENCODE_SYM(s);
}

bool data_equal(u64, u64);
std::size_t hash_data(u64);

//Used to hash key values
struct Key
{
  u64 m_key;

  bool operator==(const Key &other) const
  {
    u64 o_key = other.m_key;
    return data_equal(m_key, o_key);
  }
};

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


//used by hash function to compare equality for keys
bool data_equal_h(u64 v1, u64 v2, int loops){

    if(loops >= 1000){
        printf("library run-time error: Recursive data structure detected.\n");
        exit(1);
    }
    if (v1 == V_NULL && v2 == V_NULL)
        return true;
    else if(v1 == V_TRUE && v2 == V_TRUE){
        return true;
    }
    else if(v1 == V_FALSE && v2 == V_FALSE){
        return true;
    }
    else if(v1 == V_VOID && v2 == V_VOID){
        return true;
    }
    else if ((v1&7) == CLO_TAG) // closures must point to the same object
        return v1 == v2;
    else if ((v1&7) == CONS_TAG) // left side and right side must be equal
    {
        u64* p1 = DECODE_CONS(v1);
        if((v2&7) != CONS_TAG){ // must also be a cons
            return false;
        }

        u64* p2 = DECODE_CONS(v2);

        return data_equal_h(p1[0], p2[0], loops+1) && data_equal_h(p1[1], p2[0],loops+1);
    }
    else if ((v1&7) == INT_TAG)
    {
        if ((v2&7) != INT_TAG){
            return false;
        }

        s32 i1 = DECODE_INT(v1);
        s32 i2 = DECODE_INT(v2);
        return i1 == i2;
    }
    else if ((v1&7) == STR_TAG)
    {
        if ((v2&7) != STR_TAG){
            return false;
        }
        char* s1 = DECODE_STR(v1);
        char* s2 = DECODE_STR(v2);
        return strcmp(s1,s2) == 0;
    }
    else if ((v1&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        if ((v2&7) != SYM_TAG){
            return false;
        }
        char* s1 = DECODE_SYM(v1);
        char* s2 = DECODE_SYM(v2);
        return strcmp(s1,s2) == 0;
    }
    else if ((v1&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v1))[0] & 7)))
    {
        if((v2&7) != OTHER_TAG
                 || (VECTOR_OTHERTAG != (((u64*)DECODE_OTHER(v2))[0] & 7))){ // both vectors?
                     return false;
                 }
        u64* vec1 = (u64*)DECODE_OTHER(v1);
        u64* vec2 = (u64*)DECODE_OTHER(v2);
        u64 len1 = vec1[0] >> 3;
        u64 len2 = vec2[1] >> 3;

        if(len1 != len2){ //compare lengths
            return false;
        }
        for (u64 i = 1; i <= len1; ++i) //compare elemen wise
        {
            if(! data_equal_h(vec1[i], vec2[i], loops+1)){
                return false;
            }
        }
        return true;
    }
    else if ((v1&7) == OTHER_TAG && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v1))[0]))){

        if ((v2&7) != OTHER_TAG || (HASH_OTHERTAG != (((u64*)DECODE_OTHER(v2))[0]))){
            return false;
        }

        u64 hashPtr1 = ((u64*)DECODE_OTHER(v1))[1];
        std::unordered_map<Key, u64> *hashMap1 = ((std::unordered_map<Key, u64>*) hashPtr1);
        std::unordered_map<Key,u64>::iterator it = (*hashMap1).begin();

        u64 hashPtr2 = ((u64*)DECODE_OTHER(v2))[1];
        std::unordered_map<Key, u64> *hashMap2 = ((std::unordered_map<Key, u64>*) hashPtr2);

        //compare sizes of hashmaps
        std::size_t size1 = (*hashMap1).size();
        std::size_t size2 = (*hashMap2).size();

        if(size1 != size2){
            return false;
        }

        while(it != (*hashMap1).end()){

            //key not found
            if((*hashMap2).count(it->first) == 0){
                return false;
            }

            //both values must be equal
            if(!data_equal_h(it->second, (*hashMap2)[it->first], loops+1)){
                return false;
            }

            it++;
        }

        return true;;

    }
    else
        return false;
}

bool data_equal(u64 v1, u64 v2){
    return data_equal_h(v1, v2 , 0);
}

std::size_t hash_data_h(u64 v, int loops){ // will hash scheme values

    using std::size_t;
    using std::hash;
    using std::string;

    if(loops >= 1000){
        printf("library run-time error: Recursive data structure detected.\n");
        exit(1);
    }

    //printf("value to hash: %llu\n", v);
    if (v == V_NULL)
        return 93891; //just a random value for null, true, false, void
    else if(v == V_TRUE){
        return 34875;
    }
    else if(v == V_FALSE){
        return 85741;
    }
    else if(v == V_VOID){
        return 23897;
    }
    else if ((v&7) == CLO_TAG) //hash the pointer itself
        return hash<u64>()(v);
    else if ((v&7) == CONS_TAG) // left side and right side must be equal
    {
        u64* p = DECODE_CONS(v);

        return (hash_data_h(p[0], loops+1) << 1) ^ hash_data_h(p[1], loops + 1);
    }
    else if ((v&7) == INT_TAG)
    {
        s32 i = DECODE_INT(v);

        return hash<s32>()(i);
    }
    else if ((v&7) == STR_TAG)
    {
        char* s = DECODE_STR(v);
        string cppstr(s);
        return hash<string>()(cppstr);
    }
    else if ((v&7) == SYM_TAG)
    {
        char* s = DECODE_SYM(v);
        string cppstr(s);
        return hash<string>()(cppstr) ^ (255 << 4); //just so its different from strings
    }
    else if ((v&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {

        u64* vec = (u64*)DECODE_OTHER(v);
        u64 len = vec[0] >> 3;

        std::size_t h = len << 2;
        for (u64 i = 1; i <= len; ++i) //compare elemen wise (order does matter)
        {
            h = h ^ hash_data_h(vec[i], loops+1);
            h = h << 1;
        }
        return h;
    }
    else if ((v&7) == OTHER_TAG && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v))[0]))){

        u64 hashPtr = ((u64*)DECODE_OTHER(v))[1];
        std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
        std::unordered_map<Key,u64>::iterator it = (*hashMap).begin();

        std::size_t h = ((*hashMap).size()) << 2;
        while(it != (*hashMap).end()){

            //in this case order of pairs doesn't matter
            //k->v is not same as v->k though, so do some XOR on the pairs
            h = h + (hash_data_h(it->first.m_key, loops+1) >> 1 ^  hash_data_h(it->second, loops+1) << 2);
            it++;
        }

        return h;

    }
    else {
        fatal_err("Tried to hash unkown value type");
        return 1; //lets hope this doesn't happen
    }
}

std::size_t hash_data(u64 v){
    return hash_data_h(v, 0);
}



/////////// PRIMS


///// effectful prims:


u64 prim_print_aux(u64 v)
{
    if (v == V_NULL)
        printf("()");
    else if(v == V_TRUE){
        printf("#t");
    }
    else if(v == V_FALSE){
        printf("#f");
    }
    else if(v == V_VOID){
        printf("#<void>");
    }
    else if ((v&7) == CLO_TAG)
        printf("#<procedure>");
    else if ((v&7) == CONS_TAG)
    {
        u64* p = DECODE_CONS(v);
        printf("(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", DECODE_SYM(v));
    }
    else if ((v&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("#(");
        u64* vec = (u64*)DECODE_OTHER(v);
        u64 len = vec[0] >> 3;
        prim_print_aux(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print_aux(vec[i]);
        }
        printf(")");
    }
    else if ((v&7) == OTHER_TAG && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v))[0]))){
        printf("#hash(");
        u64 hashPtr = ((u64*)DECODE_OTHER(v))[1];
        std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
        std::unordered_map<Key,u64>::iterator it = (*hashMap).begin();

        //empty hash
        if(it == (*hashMap).end()){
            printf(")");
        }
        else{
            //first element print without space before pair
            printf("(");
            prim_print_aux(it->first.m_key);
            printf(" . ");
            prim_print_aux(it->second);
            printf(")");
            it++;

            while(it != (*hashMap).end()){

                printf(" (");
                prim_print_aux(it->first.m_key);
                printf(" . ");
                prim_print_aux(it->second);
                printf(")");
                it++;
            }
            printf(")");
        }
    }
    else
        printf("(print.. v); unrecognized value %llu", v);
    //...
    return V_VOID;
}

u64 prim_print(u64 v)
{
    hash_data(v); //hash should compute without throwing error if non-looping

    if (v == V_NULL)
        printf("'()");
    else if(v == V_TRUE){
        printf("#t");
    }
    else if(v == V_FALSE){
        printf("#f");
    }
    else if(v == V_VOID){
        printf("#<void>");
    }
    else if ((v&7) == CLO_TAG)
        printf("#<procedure>");
    else if ((v&7) == CONS_TAG)
    {
        u64* p = (u64*)(v&(7ULL^MASK64));
        printf("'(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", ((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", DECODE_SYM(v));
    }
    else if ((v&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("'#(");
        u64* vec = (u64*)DECODE_OTHER(v);
        u64 len = vec[0] >> 3;
        prim_print(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print(vec[i]);
        }
        printf(")");
    }
    else if ((v&7) == OTHER_TAG && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v))[0]))){
        printf("'#hash(");
        u64 hashPtr = ((u64*)DECODE_OTHER(v))[1];
        std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
        std::unordered_map<Key,u64>::iterator it = (*hashMap).begin();

        //empty hash
        if(it == (*hashMap).end()){
            printf(")");
        }
        else{
            //first element print without space before pair
            printf("(");
            prim_print_aux(it->first.m_key);
            printf(" . ");
            prim_print_aux(it->second);
            printf(")");
            it++;

            while(it != (*hashMap).end()){

                printf(" (");
                prim_print_aux(it->first.m_key);
                printf(" . ");
                prim_print_aux(it->second);
                printf(")");
                it++;
            }
            printf(")");
        }
    }
    else
        printf("(print v); unrecognized value %llu", v);
    //...
    return V_VOID;
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL;
}


u64 applyprim_vector(u64 lst)
{
    // pretty terrible, but works
    u64* buffer = (u64*)malloc(512*sizeof(u64));
    u64 l = 0;
    while ((lst&7) == CONS_TAG && l < 512)
        buffer[l++] = expect_cons(lst, &lst);
    u64* mem = alloc((l + 1) * sizeof(u64));
    mem[0] = (l << 3) | VECTOR_OTHERTAG;
    for (u64 i = 0; i < l; ++i)
        mem[i+1] = buffer[i];
    delete [] buffer;
    return ENCODE_OTHER(mem);
}



u64 prim_make_45vector(u64 lenv, u64 iv)
{
    ASSERT_TAG(lenv, INT_TAG, "first argument to make-vector must be an integer")

    const u64 l = DECODE_INT(lenv);
    u64* vec = (u64*)alloc((l + 1) * sizeof(u64));
    vec[0] = (l << 3) | VECTOR_OTHERTAG;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return ENCODE_OTHER(vec);
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(v, OTHER_TAG, "first argument to vector-ref must be a vector")

    if ((((u64*)DECODE_OTHER(v))[0]&7) != VECTOR_OTHERTAG)
        fatal_err("vector-ref not given a properly formed vector");

    u64 len = (((u64*)DECODE_OTHER(v))[0] >> 3);
    s32 index = DECODE_INT(i);
    if (index >= len){
        char msg [100];
        sprintf(msg, "Index out of bounds, given index %d for vector of length %llu.", index, len);
        fatal_err(msg);
    }

    return ((u64*)DECODE_OTHER(v))[1+(DECODE_INT(i))];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(a, OTHER_TAG, "first argument to vector-ref must be an integer")

    if ((((u64*)DECODE_OTHER(a))[0]&7) != VECTOR_OTHERTAG)
        fatal_err("vector-ref not given a properly formed vector");


    ((u64*)(DECODE_OTHER(a)))[1+DECODE_INT(i)] = v;

    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)


///// void, ...


u64 prim_void()
{
    return V_VOID;
}






///// eq?, eqv?, equal?


u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (data_equal(a,b))
        return V_TRUE;
    //else if  // optional extra logic, see r7rs reference
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/*
u64 prim_equal_63(u64 a, u64 b)
{
    return 0;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
*/


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if ((a&7) == CLO_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)


u64 prim_cons_63(u64 p) // cons?
{
    if ((p&7) == CONS_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)


u64 prim_cons(u64 a, u64 b)
{
    u64* p = alloc(2*sizeof(u64));
    p[0] = a;
    p[1] = b;
    return ENCODE_CONS(p);
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)


u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);

    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)


u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);

    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)


///// s32 prims, +, -, *, =, ...


u64 prim__43(u64 a, u64 b) // +
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim + a b); b is not an integer")

        //printf("sum: %d\n", DECODE_INT(a) + DECODE_INT(b));

    return ENCODE_INT(DECODE_INT(a) + DECODE_INT(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) + DECODE_INT(applyprim__43(pp[1])));
    }
}

u64 prim__45(u64 a, u64 b) // -
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim - a b); b is not an integer")

    return ENCODE_INT(DECODE_INT(a) - DECODE_INT(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        if (pp[1] == V_NULL)
            return ENCODE_INT(0 - DECODE_INT(pp[0]));
        else // ideally would be properly left-to-right
            return ENCODE_INT(DECODE_INT(pp[0]) - DECODE_INT(applyprim__43(pp[1])));
    }
}

u64 prim__42(u64 a, u64 b) // *
{
    ASSERT_TAG(a, INT_TAG, "(prim * a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim * a b); b is not an integer")

    return ENCODE_INT(DECODE_INT(a) * DECODE_INT(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(1);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) * DECODE_INT(applyprim__42(pp[1])));
    }
}

u64 prim__47(u64 a, u64 b) // /
{
    ASSERT_TAG(a, INT_TAG, "(prim / a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim / a b); b is not an integer")

    if (DECODE_INT(b) == 0){
        fatal_err("Division by 0");
    }
    return ENCODE_INT(DECODE_INT(a) / DECODE_INT(b));
}

u64 prim__61(u64 a, u64 b)  // =
{
    ASSERT_TAG(a, INT_TAG, "(prim = a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim = a b); b is not an integer")

    if ((s32)((a&(7ULL^MASK64)) >> 32) == (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    ASSERT_TAG(a, INT_TAG, "(prim < a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim < a b); b is not an integer")

    if ((s32)((a&(7ULL^MASK64)) >> 32) < (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60_61(u64 a, u64 b) // <=
{
    ASSERT_TAG(a, INT_TAG, "(prim <= a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim <= a b); b is not an integer")

    if ((s32)((a&(7ULL^MASK64)) >> 32) <= (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a)
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)

u64 prim_hash_45ref(u64 h, u64 k){
    ASSERT_TAG(h, OTHER_TAG, "First argument to hash_ref must be a hash");
    if( (((u64*)DECODE_OTHER(h))[0])  != HASH_OTHERTAG){
        fatal_err("hash-set not given a proper hash");
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


//supported key types
//int, symbol, string, cons, vector, hash ?
u64 prim_hash_45set_33(u64 h, u64 k, u64 v){
    ASSERT_TAG(h, OTHER_TAG, "First argument to hash-ref must be a hash");

    if( (((u64*)DECODE_OTHER(h))[0])  != HASH_OTHERTAG){
        fatal_err("hash-ref not given a proper hash");
    }

    u64 hashPtr = ((u64*)DECODE_OTHER(h))[1];
    std::unordered_map<Key, u64> *hashMap = ((std::unordered_map<Key, u64>*) hashPtr);
    //printf("hashmap = %llu\n", hashMap);

    Key m_key = {k};
    (*hashMap)[m_key] = v;
    return V_VOID;
}

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

// int main(){
//
//     u64 hash = prim_make_45hash();
//
//     u64 k1 = const_init_int(3);
//     u64 v1 = const_init_int(1);
//     u64 k2 = const_init_int(2);
//     u64 v2 = const_init_int(2);
//     u64 k3 = const_init_int(10);
//
//     prim_hash_45set_33(hash,k1,v1);
//     prim_hash_45set_33(hash,k2,v2);
//     //
//     u64 ret1 = prim_hash_45ref(hash,k1);
//     u64 ret2 = prim_hash_45ref(hash,k2);
//     //
//     //u64 p = prim_cons(ret1,ret2);
//     //
//     prim_halt(hash);
//     //
//     // Key a = {k1};
//     // Key b = {k2};
//     // Key c = {k3};
//     //
//     // testMap[a] = v1;
//     // testMap[b] = v2;
//     //
//     //
//     // printf("%lu %lu %lu\n", testMap.count(a), testMap.count(b), testMap.count(c));
//     // prim_print(testMap[c]);
// }

}
