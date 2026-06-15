
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
typedef struct Pair_Int_Int {
  int fst;
  int snd;
} Pair_Int_Int;

Pair_Int_Int* makePair_Int_Int(int fst, int snd) {
  Pair_Int_Int* p = malloc(sizeof(Pair_Int_Int));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
int v45(void* env45, void* v2_raw, void* v3_raw);
int v0(Node* v1);
Node* v52(void* env52, void* v18_raw, void* v19_raw);
Node* v15(void* env15, void* v16_raw, void* v17_raw);
bool v56(void* env56, void* v42_raw);
bool v58(void* env58, void* v39_raw);
Closure* v59(void* env59, void* v38_raw);
Closure* v61(void* env61, void* v35_raw);
bool v68(void* env68, void* v32_raw, void* v33_raw);
bool v29(void* env29, void* v30_raw, void* v31_raw);
Node* v74(void* env74, void* v28_raw);
Node* v80(void* env80, void* v26_raw);
Node* v20(void* env20, void* v21_raw, void* v22_raw, void* v23_raw, void* v24_raw);
Node* v95(void* env95, void* v13_raw, void* v14_raw);
Node* v9(void* env9, void* v10_raw, void* v11_raw, void* v12_raw);
Node* v6(void* env6, void* v7_raw, void* v8_raw);
Node* v106(int v5);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v5;
} Env_v6;

typedef struct {
} Env_v9;

typedef struct {
} Env_v15;

typedef struct {
} Env_v20;

typedef struct {
} Env_v29;

typedef struct {
} Env_v45;

typedef struct {
    Node* v17;
} Env_v52;

typedef struct {
    Pair_Int_Int *v35;
    int v38;
    Pair_Int_Int *v39;
} Env_v56;

typedef struct {
    Pair_Int_Int *v35;
    int v38;
} Env_v58;

typedef struct {
    Pair_Int_Int *v35;
} Env_v59;

typedef struct {
} Env_v61;

typedef struct {
    Pair_Int_Int *v30;
} Env_v68;

typedef struct {
    Node* v23;
    Pair_Int_Int *v26;
} Env_v74;

typedef struct {
    int v21;
    int v22;
    Node* v23;
    int v24;
} Env_v80;

typedef struct {
    int v10;
    int v11;
} Env_v95;

typedef struct {
} Env_v106;

// function implementations
int v45(void* env45, void* v2_raw, void* v3_raw) {
  Node* v2 = (Node*)v2_raw;
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

int v0(Node* v1) {
  Env_v45* env45 = malloc(sizeof(Env_v45));
  if (((v1) == NULL)) return 0;
  return v45(env45, (void*)((v1)->head), (void*)((v1)->tail));
}

Node* v52(void* env52, void* v18_raw, void* v19_raw) {
  Node* v18 = (Node*)v18_raw;
  Node* v19 = (Node*)v19_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  return cons(v18, v15(env15, (void*)(v19), (void*)(((Env_v52*)env52)->v17)));
}

Node* v15(void* env15, void* v16_raw, void* v17_raw) {
  Node* v16 = (Node*)v16_raw;
  Node* v17 = (Node*)v17_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v17 = v17;
  if (((v16) == NULL)) return v17;
  return v52(env52, (void*)((v16)->head), (void*)((v16)->tail));
}

bool v56(void* env56, void* v42_raw) {
  int v42 = *(int*)v42_raw;
  return ((((Env_v56*)env56)->v38 == v42) || (abs((((Env_v56*)env56)->v38 - v42)) == abs(((((Env_v56*)env56)->v35)->fst - (((Env_v56*)env56)->v39)->fst))));
}

bool v58(void* env58, void* v39_raw) {
  Pair_Int_Int *v39 = (Pair_Int_Int*)v39_raw;
  Env_v56* env56 = malloc(sizeof(Env_v56));
  env56->v39 = v39;
  env56->v35 = ((Env_v58*)env58)->v35;
  env56->v38 = ((Env_v58*)env58)->v38;
  return v56(env56, box_int((v39)->snd));
}

Closure* v59(void* env59, void* v38_raw) {
  int v38 = *(int*)v38_raw;
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v38 = v38;
  env58->v35 = ((Env_v59*)env59)->v35;
  Closure* c58 = malloc(sizeof(Closure));
  c58->env = env58;
  c58->fn = (void* (*)(void*, void*))v58;
  return c58;
}

Closure* v61(void* env61, void* v35_raw) {
  Pair_Int_Int *v35 = (Pair_Int_Int*)v35_raw;
  Env_v59* env59 = malloc(sizeof(Env_v59));
  env59->v35 = v35;
  Closure* c59 = v59(env59, box_int((v35)->snd));
  return c59;
}

bool v68(void* env68, void* v32_raw, void* v33_raw) {
  Pair_Int_Int *v32 = (Pair_Int_Int*)v32_raw;
  Node* v33 = (Node*)v33_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Env_v61* env61 = malloc(sizeof(Env_v61));
  Closure* c61 = v61(env61, (void*)(((Env_v68*)env68)->v30));
  return (!((Closure*)(c61)->fn((c61)->env, v32)) && v29(env29, (void*)(((Env_v68*)env68)->v30), (void*)(v33)));
}

bool v29(void* env29, void* v30_raw, void* v31_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Node* v31 = (Node*)v31_raw;
  Env_v68* env68 = malloc(sizeof(Env_v68));
  env68->v30 = v30;
  if (((v31) == NULL)) return true;
  return v68(env68, (void*)((v31)->head), (void*)((v31)->tail));
}

Node* v74(void* env74, void* v28_raw) {
  Node* v28 = (Node*)v28_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  if (v29(env29, (void*)(((Env_v74*)env74)->v26), (void*)(((Env_v74*)env74)->v23))) {
    return cons(cons(((Env_v74*)env74)->v26, ((Env_v74*)env74)->v23), v28);
  } else {
    return v28;
  }
}

Node* v80(void* env80, void* v26_raw) {
  Pair_Int_Int *v26 = (Pair_Int_Int*)v26_raw;
  Env_v20* env20 = malloc(sizeof(Env_v20));
  Env_v74* env74 = malloc(sizeof(Env_v74));
  env74->v26 = v26;
  env74->v23 = ((Env_v80*)env80)->v23;
  return v74(env74, (void*)(v20(env20, box_int(((Env_v80*)env80)->v21), box_int(((Env_v80*)env80)->v22), (void*)(((Env_v80*)env80)->v23), box_int((((Env_v80*)env80)->v24 + 1)))));
}

Node* v20(void* env20, void* v21_raw, void* v22_raw, void* v23_raw, void* v24_raw) {
  int v21 = *(int*)v21_raw;
  int v22 = *(int*)v22_raw;
  Node* v23 = (Node*)v23_raw;
  int v24 = *(int*)v24_raw;
  Env_v80* env80 = malloc(sizeof(Env_v80));
  env80->v21 = v21;
  env80->v22 = v22;
  env80->v23 = v23;
  env80->v24 = v24;
  if ((v24 == v21)) return NULL;
  return v80(env80, (void*)(makePair_Int_Int(v22, v24)));
}

Node* v95(void* env95, void* v13_raw, void* v14_raw) {
  Node* v13 = (Node*)v13_raw;
  Node* v14 = (Node*)v14_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Env_v20* env20 = malloc(sizeof(Env_v20));
  return v15(env15, (void*)(v20(env20, box_int(((Env_v95*)env95)->v10), box_int(((Env_v95*)env95)->v11), (void*)(v13), box_int(0))), (void*)(v9(env9, box_int(((Env_v95*)env95)->v10), box_int(((Env_v95*)env95)->v11), (void*)(v14))));
}

Node* v9(void* env9, void* v10_raw, void* v11_raw, void* v12_raw) {
  int v10 = *(int*)v10_raw;
  int v11 = *(int*)v11_raw;
  Node* v12 = (Node*)v12_raw;
  Env_v95* env95 = malloc(sizeof(Env_v95));
  env95->v10 = v10;
  env95->v11 = v11;
  if (((v12) == NULL)) return NULL;
  return v95(env95, (void*)((v12)->head), (void*)((v12)->tail));
}

Node* v6(void* env6, void* v7_raw, void* v8_raw) {
  int v7 = *(int*)v7_raw;
  Node* v8 = (Node*)v8_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  if ((v7 == ((Env_v6*)env6)->v5)) return v8;
  return v6(env6, box_int((v7 + 1)), (void*)(v9(env9, box_int(((Env_v6*)env6)->v5), box_int(v7), (void*)(v8))));
}

Node* v106(int v5) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = v5;
  return v6(env6, box_int(0), (void*)(cons(NULL, NULL)));
}

// main
int main(void) {
  printInt(v0(v106(4)));
  return 0;
}

