
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
ListInt* v38(void* env38, void* v13_raw, void* v14_raw);
ListInt* v41(void* env41, void* v11_raw, void* v12_raw);
ListInt* v8(void* env8, void* v9_raw, void* v10_raw);
ListInt* v48(void* env48, void* v7_raw);
Pair_ListInt_ListInt* v50(void* env50, void* v28_raw);
Pair_ListInt_ListInt* v54(void* env54, void* v25_raw, void* v26_raw);
Pair_ListInt_ListInt* v56(void* env56, void* v24_raw);
Pair_ListInt_ListInt* v58(void* env58, void* v22_raw);
Pair_ListInt_ListInt* v19(void* env19, void* v20_raw);
int v63(void* env63, void* v31_raw, void* v32_raw);
int v29(void* env29, void* v30_raw);
Pair_ListInt_ListInt* v67(void* env67, void* v16_raw);
ListInt* v71(void* env71, void* v4_raw, void* v5_raw);
ListInt* v74(void* env74, void* v2_raw, void* v3_raw);
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
} Env_v38;

typedef struct {
    ListInt* v9;
    ListInt* v10;
} Env_v41;

typedef struct {
} Env_v48;

typedef struct {
    int v25;
} Env_v50;

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
} Env_v63;

typedef struct {
} Env_v67;

typedef struct {
    int v2;
} Env_v71;

typedef struct {
} Env_v74;

// function implementations
ListInt* v38(void* env38, void* v13_raw, void* v14_raw) {
  int v13 = *(int*)v13_raw;
  ListInt* v14 = (ListInt*)v14_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  if ((((Env_v38*)env38)->v11 < v13)) {
    return consInt(((Env_v38*)env38)->v11, v8(env8, (void*)(((Env_v38*)env38)->v12), (void*)(((Env_v38*)env38)->v10)));
  } else {
    return consInt(v13, v8(env8, (void*)(v14), (void*)(((Env_v38*)env38)->v9)));
  }
}

ListInt* v41(void* env41, void* v11_raw, void* v12_raw) {
  int v11 = *(int*)v11_raw;
  ListInt* v12 = (ListInt*)v12_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v9 = ((Env_v41*)env41)->v9;
  env38->v10 = ((Env_v41*)env41)->v10;
  env38->v11 = v11;
  env38->v12 = v12;
  if (((((Env_v41*)env41)->v10) == NULL)) return ((Env_v41*)env41)->v9;
  return v38(env38, box_int((((Env_v41*)env41)->v10)->head), (void*)((((Env_v41*)env41)->v10)->tail));
}

ListInt* v8(void* env8, void* v9_raw, void* v10_raw) {
  ListInt* v9 = (ListInt*)v9_raw;
  ListInt* v10 = (ListInt*)v10_raw;
  Env_v41* env41 = malloc(sizeof(Env_v41));
  env41->v9 = v9;
  env41->v10 = v10;
  if (((v9) == NULL)) return v10;
  return v41(env41, box_int((v9)->head), (void*)((v9)->tail));
}

ListInt* v48(void* env48, void* v7_raw) {
  Pair_ListInt_ListInt *v7 = (Pair_ListInt_ListInt*)v7_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  return v8(env8, (void*)(v0((v7)->fst)), (void*)(v0((v7)->snd)));
}

Pair_ListInt_ListInt* v50(void* env50, void* v28_raw) {
  Pair_ListInt_ListInt *v28 = (Pair_ListInt_ListInt*)v28_raw;
  return makePair_ListInt_ListInt(consInt(((Env_v50*)env50)->v25, (v28)->fst), (v28)->snd);
}

Pair_ListInt_ListInt* v54(void* env54, void* v25_raw, void* v26_raw) {
  int v25 = *(int*)v25_raw;
  ListInt* v26 = (ListInt*)v26_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v25 = v25;
  return v50(env50, (void*)(v19(env19, (void*)(makePair_Int_ListInt((((Env_v54*)env54)->v22 - 1), v26)))));
}

Pair_ListInt_ListInt* v56(void* env56, void* v24_raw) {
  ListInt* v24 = (ListInt*)v24_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v22 = ((Env_v56*)env56)->v22;
  if ((((Env_v56*)env56)->v22 == 0)) return makePair_ListInt_ListInt(NULL, v24);
  if (((v24) == NULL)) return makePair_ListInt_ListInt(NULL, NULL);
  return v54(env54, box_int((v24)->head), (void*)((v24)->tail));
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

int v63(void* env63, void* v31_raw, void* v32_raw) {
  int v31 = *(int*)v31_raw;
  ListInt* v32 = (ListInt*)v32_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return (1 + v29(env29, (void*)(v32)));
}

int v29(void* env29, void* v30_raw) {
  ListInt* v30 = (ListInt*)v30_raw;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  if (((v30) == NULL)) return 0;
  return v63(env63, box_int((v30)->head), (void*)((v30)->tail));
}

Pair_ListInt_ListInt* v67(void* env67, void* v16_raw) {
  ListInt* v16 = (ListInt*)v16_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return v19(env19, (void*)(makePair_Int_ListInt((v29(env29, (void*)(v16)) / 2), v16)));
}

ListInt* v71(void* env71, void* v4_raw, void* v5_raw) {
  int v4 = *(int*)v4_raw;
  ListInt* v5 = (ListInt*)v5_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  Env_v67* env67 = malloc(sizeof(Env_v67));
  return v48(env48, (void*)(v67(env67, (void*)(consInt(((Env_v71*)env71)->v2, consInt(v4, v5))))));
}

ListInt* v74(void* env74, void* v2_raw, void* v3_raw) {
  int v2 = *(int*)v2_raw;
  ListInt* v3 = (ListInt*)v3_raw;
  Env_v71* env71 = malloc(sizeof(Env_v71));
  env71->v2 = v2;
  if (((v3) == NULL)) return consInt(v2, NULL);
  return v71(env71, box_int((v3)->head), (void*)((v3)->tail));
}

ListInt* v0(ListInt* v1) {
  Env_v74* env74 = malloc(sizeof(Env_v74));
  if (((v1) == NULL)) return NULL;
  return v74(env74, box_int((v1)->head), (void*)((v1)->tail));
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

