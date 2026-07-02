
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
typedef struct Pair_Int_ListInt {
  int fst;
  ListInt* snd;
} Pair_Int_ListInt;

Pair_Int_ListInt* makePair_Int_ListInt(int fst, ListInt* snd) {
  Pair_Int_ListInt* p = malloc(sizeof(Pair_Int_ListInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

typedef struct Pair_ListInt_ListInt {
  ListInt* fst;
  ListInt* snd;
} Pair_ListInt_ListInt;

Pair_ListInt_ListInt* makePair_ListInt_ListInt(ListInt* fst, ListInt* snd) {
  Pair_ListInt_ListInt* p = malloc(sizeof(Pair_ListInt_ListInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
ListInt* v41(void* env41, void* v14_raw);
Closure* v42(void* env42, void* v13_raw);
ListInt* v44(void* env44, void* v12_raw);
Closure* v45(void* env45, void* v11_raw);
ListInt* v47(void* env47, void* v10_raw);
Closure* v8(void* env8, void* v9_raw);
ListInt* v52(void* env52, void* v7_raw);
Pair_ListInt_ListInt* v61(void* env61, void* v28_raw);
Pair_ListInt_ListInt* v64(void* env64, void* v26_raw);
Closure* v65(void* env65, void* v25_raw);
Pair_ListInt_ListInt* v67(void* env67, void* v24_raw);
Pair_ListInt_ListInt* v69(void* env69, void* v22_raw);
Pair_ListInt_ListInt* v19(void* env19, void* v20_raw);
int v73(void* env73, void* v32_raw);
Closure* v74(void* env74, void* v31_raw);
int v29(void* env29, void* v30_raw);
Pair_ListInt_ListInt* v78(void* env78, void* v16_raw);
ListInt* v81(void* env81, void* v5_raw);
Closure* v82(void* env82, void* v4_raw);
ListInt* v84(void* env84, void* v3_raw);
Closure* v85(void* env85, void* v2_raw);
ListInt* v0(ListInt* v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
} Env_v8;

typedef struct {
} Env_v19;

typedef struct {
} Env_v29;

typedef struct {
    ListInt* v9;
    ListInt* v10;
    int v11;
    ListInt* v12;
    int v13;
} Env_v41;

typedef struct {
    ListInt* v9;
    ListInt* v10;
    int v11;
    ListInt* v12;
} Env_v42;

typedef struct {
    ListInt* v9;
    ListInt* v10;
    int v11;
} Env_v44;

typedef struct {
    ListInt* v9;
    ListInt* v10;
} Env_v45;

typedef struct {
    ListInt* v9;
} Env_v47;

typedef struct {
} Env_v52;

typedef struct {
    int v25;
} Env_v61;

typedef struct {
    int v22;
    int v25;
} Env_v64;

typedef struct {
    int v22;
} Env_v65;

typedef struct {
    int v22;
} Env_v67;

typedef struct {
    Pair_Int_ListInt *v20;
} Env_v69;

typedef struct {
} Env_v73;

typedef struct {
} Env_v74;

typedef struct {
} Env_v78;

typedef struct {
    int v2;
    int v4;
} Env_v81;

typedef struct {
    int v2;
} Env_v82;

typedef struct {
    int v2;
} Env_v84;

typedef struct {
} Env_v85;

// function implementations
ListInt* v41(void* env41, void* v14_raw) {
  ListInt* v14 = (ListInt*)v14_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  if ((((Env_v41*)env41)->v11 < ((Env_v41*)env41)->v13)) {
    Closure* c8 = v8(env8, (void*)(((Env_v41*)env41)->v12));
    return consInt(((Env_v41*)env41)->v11, (ListInt*)(c8)->fn((c8)->env, ((Env_v41*)env41)->v10));
  } else {
    Closure* c8 = v8(env8, (void*)(v14));
    return consInt(((Env_v41*)env41)->v13, (ListInt*)(c8)->fn((c8)->env, ((Env_v41*)env41)->v9));
  }
}

Closure* v42(void* env42, void* v13_raw) {
  int v13 = *(int*)v13_raw;
  Env_v41* env41 = malloc(sizeof(Env_v41));
  env41->v9 = ((Env_v42*)env42)->v9;
  env41->v10 = ((Env_v42*)env42)->v10;
  env41->v11 = ((Env_v42*)env42)->v11;
  env41->v12 = ((Env_v42*)env42)->v12;
  env41->v13 = v13;
  Closure* c41 = malloc(sizeof(Closure));
  c41->env = env41;
  c41->fn = (void* (*)(void*, void*))v41;
  return c41;
}

ListInt* v44(void* env44, void* v12_raw) {
  ListInt* v12 = (ListInt*)v12_raw;
  Env_v42* env42 = malloc(sizeof(Env_v42));
  env42->v9 = ((Env_v44*)env44)->v9;
  env42->v10 = ((Env_v44*)env44)->v10;
  env42->v11 = ((Env_v44*)env44)->v11;
  env42->v12 = v12;
  if (((((Env_v44*)env44)->v10) == NULL)) return ((Env_v44*)env44)->v9;
  Closure* c42 = v42(env42, box_int((((Env_v44*)env44)->v10)->head));
  return (ListInt*)(c42)->fn((c42)->env, (((Env_v44*)env44)->v10)->tail);
}

Closure* v45(void* env45, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v44* env44 = malloc(sizeof(Env_v44));
  env44->v9 = ((Env_v45*)env45)->v9;
  env44->v10 = ((Env_v45*)env45)->v10;
  env44->v11 = v11;
  Closure* c44 = malloc(sizeof(Closure));
  c44->env = env44;
  c44->fn = (void* (*)(void*, void*))v44;
  return c44;
}

ListInt* v47(void* env47, void* v10_raw) {
  ListInt* v10 = (ListInt*)v10_raw;
  Env_v45* env45 = malloc(sizeof(Env_v45));
  env45->v9 = ((Env_v47*)env47)->v9;
  env45->v10 = v10;
  if (((((Env_v47*)env47)->v9) == NULL)) return v10;
  Closure* c45 = v45(env45, box_int((((Env_v47*)env47)->v9)->head));
  return (ListInt*)(c45)->fn((c45)->env, (((Env_v47*)env47)->v9)->tail);
}

Closure* v8(void* env8, void* v9_raw) {
  ListInt* v9 = (ListInt*)v9_raw;
  Env_v47* env47 = malloc(sizeof(Env_v47));
  env47->v9 = v9;
  Closure* c47 = malloc(sizeof(Closure));
  c47->env = env47;
  c47->fn = (void* (*)(void*, void*))v47;
  return c47;
}

ListInt* v52(void* env52, void* v7_raw) {
  Pair_ListInt_ListInt *v7 = (Pair_ListInt_ListInt*)v7_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  Closure* c8 = v8(env8, (void*)(v0((v7)->fst)));
  return (ListInt*)(c8)->fn((c8)->env, v0((v7)->snd));
}

Pair_ListInt_ListInt* v61(void* env61, void* v28_raw) {
  Pair_ListInt_ListInt *v28 = (Pair_ListInt_ListInt*)v28_raw;
  return makePair_ListInt_ListInt(consInt(((Env_v61*)env61)->v25, (v28)->fst), (v28)->snd);
}

Pair_ListInt_ListInt* v64(void* env64, void* v26_raw) {
  ListInt* v26 = (ListInt*)v26_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v61* env61 = malloc(sizeof(Env_v61));
  env61->v25 = ((Env_v64*)env64)->v25;
  return v61(env61, (void*)(v19(env19, (void*)(makePair_Int_ListInt((((Env_v64*)env64)->v22 - 1), v26)))));
}

Closure* v65(void* env65, void* v25_raw) {
  int v25 = *(int*)v25_raw;
  Env_v64* env64 = malloc(sizeof(Env_v64));
  env64->v22 = ((Env_v65*)env65)->v22;
  env64->v25 = v25;
  Closure* c64 = malloc(sizeof(Closure));
  c64->env = env64;
  c64->fn = (void* (*)(void*, void*))v64;
  return c64;
}

Pair_ListInt_ListInt* v67(void* env67, void* v24_raw) {
  ListInt* v24 = (ListInt*)v24_raw;
  Env_v65* env65 = malloc(sizeof(Env_v65));
  env65->v22 = ((Env_v67*)env67)->v22;
  if ((((Env_v67*)env67)->v22 == 0)) return makePair_ListInt_ListInt(NULL, v24);
  if (((v24) == NULL)) return makePair_ListInt_ListInt(NULL, NULL);
  Closure* c65 = v65(env65, box_int((v24)->head));
  return (Pair_ListInt_ListInt*)(c65)->fn((c65)->env, (v24)->tail);
}

Pair_ListInt_ListInt* v69(void* env69, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v67* env67 = malloc(sizeof(Env_v67));
  env67->v22 = v22;
  return v67(env67, (void*)((((Env_v69*)env69)->v20)->snd));
}

Pair_ListInt_ListInt* v19(void* env19, void* v20_raw) {
  Pair_Int_ListInt *v20 = (Pair_Int_ListInt*)v20_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v20 = v20;
  return v69(env69, box_int((v20)->fst));
}

int v73(void* env73, void* v32_raw) {
  ListInt* v32 = (ListInt*)v32_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return (1 + v29(env29, (void*)(v32)));
}

Closure* v74(void* env74, void* v31_raw) {
  int v31 = *(int*)v31_raw;
  Env_v73* env73 = malloc(sizeof(Env_v73));
  Closure* c73 = malloc(sizeof(Closure));
  c73->env = env73;
  c73->fn = (void* (*)(void*, void*))v73;
  return c73;
}

int v29(void* env29, void* v30_raw) {
  ListInt* v30 = (ListInt*)v30_raw;
  Env_v74* env74 = malloc(sizeof(Env_v74));
  if (((v30) == NULL)) return 0;
  Closure* c74 = v74(env74, box_int((v30)->head));
  return (int)(intptr_t)(c74)->fn((c74)->env, (v30)->tail);
}

Pair_ListInt_ListInt* v78(void* env78, void* v16_raw) {
  ListInt* v16 = (ListInt*)v16_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return v19(env19, (void*)(makePair_Int_ListInt((v29(env29, (void*)(v16)) / 2), v16)));
}

ListInt* v81(void* env81, void* v5_raw) {
  ListInt* v5 = (ListInt*)v5_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  Env_v78* env78 = malloc(sizeof(Env_v78));
  return v52(env52, (void*)(v78(env78, (void*)(consInt(((Env_v81*)env81)->v2, consInt(((Env_v81*)env81)->v4, v5))))));
}

Closure* v82(void* env82, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v2 = ((Env_v82*)env82)->v2;
  env81->v4 = v4;
  Closure* c81 = malloc(sizeof(Closure));
  c81->env = env81;
  c81->fn = (void* (*)(void*, void*))v81;
  return c81;
}

ListInt* v84(void* env84, void* v3_raw) {
  ListInt* v3 = (ListInt*)v3_raw;
  Env_v82* env82 = malloc(sizeof(Env_v82));
  env82->v2 = ((Env_v84*)env84)->v2;
  if (((v3) == NULL)) return consInt(((Env_v84*)env84)->v2, NULL);
  Closure* c82 = v82(env82, box_int((v3)->head));
  return (ListInt*)(c82)->fn((c82)->env, (v3)->tail);
}

Closure* v85(void* env85, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v2 = v2;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

ListInt* v0(ListInt* v1) {
  Env_v85* env85 = malloc(sizeof(Env_v85));
  if (((v1) == NULL)) return NULL;
  Closure* c85 = v85(env85, box_int((v1)->head));
  return (ListInt*)(c85)->fn((c85)->env, (v1)->tail);
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

