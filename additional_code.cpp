
u64 make_closure(u64 (*func)(u64, u64), u64 envList){
    u64* p = alloc(2*sizeof(u64));
    p[0] = (u64)func; //casting madness
    p[1] = envList; //supposedly a vector
    return ENCODE_CLO(p);
}

u64 closure_apply(u64 clo, u64 argList){

    ASSERT_TAG(clo, CLO_TAG, "Expected a closure value")

    u64* p  = DECODE_CLO(clo);
    u64 (*func)(u64, u64) = (u64 (*)(u64, u64))p[0];
    u64 env = p[1]; //a vector
    return func(env,argList);
}
