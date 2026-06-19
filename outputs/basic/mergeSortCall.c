
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
ListInt* v37(void* env37, void* v14_raw);
Closure* v38(void* env38, void* v13_raw);
ListInt* v40(void* env40, void* v12_raw);
Closure* v41(void* env41, void* v11_raw);
ListInt* v43(void* env43, void* v10_raw);
Closure* v8(void* env8, void* v9_raw);
ListInt* v48(void* env48, void* v7_raw);
Pair_ListInt_ListInt* v50(void* env50, void* v28_raw);
Pair_ListInt_ListInt* v53(void* env53, void* v26_raw);
Closure* v54(void* env54, void* v25_raw);
Pair_ListInt_ListInt* v56(void* env56, void* v24_raw);
Pair_ListInt_ListInt* v58(void* env58, void* v22_raw);
Pair_ListInt_ListInt* v19(void* env19, void* v20_raw);
int v62(void* env62, void* v32_raw);
Closure* v63(void* env63, void* v31_raw);
int v29(void* env29, void* v30_raw);
Pair_ListInt_ListInt* v67(void* env67, void* v16_raw);
ListInt* v70(void* env70, void* v5_raw);
Closure* v71(void* env71, void* v4_raw);
ListInt* v73(void* env73, void* v3_raw);
Closure* v74(void* env74, void* v2_raw);
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
} Env_v37;

typedef struct {
    ListInt* v9;
    ListInt* v10;
    int v11;
    ListInt* v12;
} Env_v38;

typedef struct {
    ListInt* v9;
    ListInt* v10;
    int v11;
} Env_v40;

typedef struct {
    ListInt* v9;
    ListInt* v10;
} Env_v41;

typedef struct {
    ListInt* v9;
} Env_v43;

typedef struct {
} Env_v48;

typedef struct {
    int v25;
} Env_v50;

typedef struct {
    int v22;
    int v25;
} Env_v53;

typedef struct {
    int v22;
} Env_v54;

typedef struct {
    int v22;
} Env_v56;

typedef struct {
    Pair_Int_ListInt *v20;
} Env_v58;

typedef struct {
} Env_v62;

typedef struct {
} Env_v63;

typedef struct {
} Env_v67;

typedef struct {
    int v2;
    int v4;
} Env_v70;

typedef struct {
    int v2;
} Env_v71;

typedef struct {
    int v2;
} Env_v73;

typedef struct {
} Env_v74;

// function implementations
ListInt* v37(void* env37, void* v14_raw) {
  ListInt* v14 = (ListInt*)v14_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  if ((((Env_v37*)env37)->v11 < ((Env_v37*)env37)->v13)) {
    Closure* c8 = v8(env8, (void*)(((Env_v37*)env37)->v12));
    return consInt(((Env_v37*)env37)->v11, (ListInt*)(c8)->fn((c8)->env, ((Env_v37*)env37)->v10));
  } else {
    Closure* c8 = v8(env8, (void*)(v14));
    return consInt(((Env_v37*)env37)->v13, (ListInt*)(c8)->fn((c8)->env, ((Env_v37*)env37)->v9));
  }
}

Closure* v38(void* env38, void* v13_raw) {
  int v13 = *(int*)v13_raw;
  Env_v37* env37 = malloc(sizeof(Env_v37));
  env37->v13 = v13;
  env37->v9 = ((Env_v38*)env38)->v9;
  env37->v10 = ((Env_v38*)env38)->v10;
  env37->v11 = ((Env_v38*)env38)->v11;
  env37->v12 = ((Env_v38*)env38)->v12;
  Closure* c37 = malloc(sizeof(Closure));
  c37->env = env37;
  c37->fn = (void* (*)(void*, void*))v37;
  return c37;
}

ListInt* v40(void* env40, void* v12_raw) {
  ListInt* v12 = (ListInt*)v12_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v12 = v12;
  env38->v9 = ((Env_v40*)env40)->v9;
  env38->v10 = ((Env_v40*)env40)->v10;
  env38->v11 = ((Env_v40*)env40)->v11;
  if (((((Env_v40*)env40)->v10) == NULL)) return ((Env_v40*)env40)->v9;
  Closure* c38 = v38(env38, box_int((((Env_v40*)env40)->v10)->head));
  return (ListInt*)(c38)->fn((c38)->env, (((Env_v40*)env40)->v10)->tail);
}

Closure* v41(void* env41, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v40* env40 = malloc(sizeof(Env_v40));
  env40->v11 = v11;
  env40->v9 = ((Env_v41*)env41)->v9;
  env40->v10 = ((Env_v41*)env41)->v10;
  Closure* c40 = malloc(sizeof(Closure));
  c40->env = env40;
  c40->fn = (void* (*)(void*, void*))v40;
  return c40;
}

ListInt* v43(void* env43, void* v10_raw) {
  ListInt* v10 = (ListInt*)v10_raw;
  Env_v41* env41 = malloc(sizeof(Env_v41));
  env41->v10 = v10;
  env41->v9 = ((Env_v43*)env43)->v9;
  if (((((Env_v43*)env43)->v9) == NULL)) return v10;
  Closure* c41 = v41(env41, box_int((((Env_v43*)env43)->v9)->head));
  return (ListInt*)(c41)->fn((c41)->env, (((Env_v43*)env43)->v9)->tail);
}

Closure* v8(void* env8, void* v9_raw) {
  ListInt* v9 = (ListInt*)v9_raw;
  Env_v43* env43 = malloc(sizeof(Env_v43));
  env43->v9 = v9;
  Closure* c43 = malloc(sizeof(Closure));
  c43->env = env43;
  c43->fn = (void* (*)(void*, void*))v43;
  return c43;
}

ListInt* v48(void* env48, void* v7_raw) {
  Pair_ListInt_ListInt *v7 = (Pair_ListInt_ListInt*)v7_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  Closure* c8 = v8(env8, (void*)(v0((v7)->fst)));
  return (ListInt*)(c8)->fn((c8)->env, v0((v7)->snd));
}

Pair_ListInt_ListInt* v50(void* env50, void* v28_raw) {
  Pair_ListInt_ListInt *v28 = (Pair_ListInt_ListInt*)v28_raw;
  return makePair_ListInt_ListInt(consInt(((Env_v50*)env50)->v25, (v28)->fst), (v28)->snd);
}

Pair_ListInt_ListInt* v53(void* env53, void* v26_raw) {
  ListInt* v26 = (ListInt*)v26_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v25 = ((Env_v53*)env53)->v25;
  return v50(env50, (void*)(v19(env19, (void*)(makePair_Int_ListInt((((Env_v53*)env53)->v22 - 1), v26)))));
}

Closure* v54(void* env54, void* v25_raw) {
  int v25 = *(int*)v25_raw;
  Env_v53* env53 = malloc(sizeof(Env_v53));
  env53->v25 = v25;
  env53->v22 = ((Env_v54*)env54)->v22;
  Closure* c53 = malloc(sizeof(Closure));
  c53->env = env53;
  c53->fn = (void* (*)(void*, void*))v53;
  return c53;
}

Pair_ListInt_ListInt* v56(void* env56, void* v24_raw) {
  ListInt* v24 = (ListInt*)v24_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v22 = ((Env_v56*)env56)->v22;
  if ((((Env_v56*)env56)->v22 == 0)) {
    return makePair_ListInt_ListInt(NULL, v24);
  } else {
    if (((v24) == NULL)) {
      return makePair_ListInt_ListInt(NULL, NULL);
    } else {
      Closure* c54 = v54(env54, box_int((v24)->head));
      return (Pair_ListInt_ListInt*)(c54)->fn((c54)->env, (v24)->tail);
    }
  }
}

Pair_ListInt_ListInt* v58(void* env58, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v56* env56 = malloc(sizeof(Env_v56));
  env56->v22 = v22;
  return v56(env56, (void*)((((Env_v58*)env58)->v20)->snd));
}

Pair_ListInt_ListInt* v19(void* env19, void* v20_raw) {
  Pair_Int_ListInt *v20 = (Pair_Int_ListInt*)v20_raw;
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v20 = v20;
  return v58(env58, box_int((v20)->fst));
}

int v62(void* env62, void* v32_raw) {
  ListInt* v32 = (ListInt*)v32_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return (1 + v29(env29, (void*)(v32)));
}

Closure* v63(void* env63, void* v31_raw) {
  int v31 = *(int*)v31_raw;
  Env_v62* env62 = malloc(sizeof(Env_v62));
  Closure* c62 = malloc(sizeof(Closure));
  c62->env = env62;
  c62->fn = (void* (*)(void*, void*))v62;
  return c62;
}

int v29(void* env29, void* v30_raw) {
  ListInt* v30 = (ListInt*)v30_raw;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  if (((v30) == NULL)) return 0;
  Closure* c63 = v63(env63, box_int((v30)->head));
  return (int)(intptr_t)(c63)->fn((c63)->env, (v30)->tail);
}

Pair_ListInt_ListInt* v67(void* env67, void* v16_raw) {
  ListInt* v16 = (ListInt*)v16_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return v19(env19, (void*)(makePair_Int_ListInt((v29(env29, (void*)(v16)) / 2), v16)));
}

ListInt* v70(void* env70, void* v5_raw) {
  ListInt* v5 = (ListInt*)v5_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  Env_v67* env67 = malloc(sizeof(Env_v67));
  return v48(env48, (void*)(v67(env67, (void*)(consInt(((Env_v70*)env70)->v2, consInt(((Env_v70*)env70)->v4, v5))))));
}

Closure* v71(void* env71, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v70* env70 = malloc(sizeof(Env_v70));
  env70->v4 = v4;
  env70->v2 = ((Env_v71*)env71)->v2;
  Closure* c70 = malloc(sizeof(Closure));
  c70->env = env70;
  c70->fn = (void* (*)(void*, void*))v70;
  return c70;
}

ListInt* v73(void* env73, void* v3_raw) {
  ListInt* v3 = (ListInt*)v3_raw;
  Env_v71* env71 = malloc(sizeof(Env_v71));
  env71->v2 = ((Env_v73*)env73)->v2;
  if (((v3) == NULL)) {
    return consInt(((Env_v73*)env73)->v2, NULL);
  } else {
    Closure* c71 = v71(env71, box_int((v3)->head));
    return (ListInt*)(c71)->fn((c71)->env, (v3)->tail);
  }
}

Closure* v74(void* env74, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v73* env73 = malloc(sizeof(Env_v73));
  env73->v2 = v2;
  Closure* c73 = malloc(sizeof(Closure));
  c73->env = env73;
  c73->fn = (void* (*)(void*, void*))v73;
  return c73;
}

ListInt* v0(ListInt* v1) {
  Env_v74* env74 = malloc(sizeof(Env_v74));
  if (((v1) == NULL)) return NULL;
  Closure* c74 = v74(env74, box_int((v1)->head));
  return (ListInt*)(c74)->fn((c74)->env, (v1)->tail);
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

