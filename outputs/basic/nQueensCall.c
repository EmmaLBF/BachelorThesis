
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
int v44(void* env44, void* v3_raw);
Closure* v45(void* env45, void* v2_raw);
int v0(Node* v1);
Node* v51(void* env51, void* v19_raw);
Closure* v52(void* env52, void* v18_raw);
Node* v54(void* env54, void* v17_raw);
Closure* v15(void* env15, void* v16_raw);
bool v56(void* env56, void* v42_raw);
bool v58(void* env58, void* v39_raw);
Closure* v59(void* env59, void* v38_raw);
Closure* v61(void* env61, void* v35_raw);
bool v67(void* env67, void* v33_raw);
Closure* v68(void* env68, void* v32_raw);
bool v70(void* env70, void* v31_raw);
Closure* v29(void* env29, void* v30_raw);
Node* v74(void* env74, void* v28_raw);
Node* v80(void* env80, void* v26_raw);
Node* v82(void* env82, void* v24_raw);
Closure* v83(void* env83, void* v23_raw);
Closure* v84(void* env84, void* v22_raw);
Closure* v20(void* env20, void* v21_raw);
Node* v94(void* env94, void* v14_raw);
Closure* v95(void* env95, void* v13_raw);
Node* v97(void* env97, void* v12_raw);
Closure* v98(void* env98, void* v11_raw);
Closure* v9(void* env9, void* v10_raw);
Node* v103(void* env103, void* v8_raw);
Closure* v6(void* env6, void* v7_raw);
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
} Env_v44;

typedef struct {
} Env_v45;

typedef struct {
    Node* v17;
    Node* v18;
} Env_v51;

typedef struct {
    Node* v17;
} Env_v52;

typedef struct {
    Node* v16;
} Env_v54;

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
    Pair_Int_Int *v32;
} Env_v67;

typedef struct {
    Pair_Int_Int *v30;
} Env_v68;

typedef struct {
    Pair_Int_Int *v30;
} Env_v70;

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
    int v21;
    int v22;
    Node* v23;
} Env_v82;

typedef struct {
    int v21;
    int v22;
} Env_v83;

typedef struct {
    int v21;
} Env_v84;

typedef struct {
    int v10;
    int v11;
    Node* v13;
} Env_v94;

typedef struct {
    int v10;
    int v11;
} Env_v95;

typedef struct {
    int v10;
    int v11;
} Env_v97;

typedef struct {
    int v10;
} Env_v98;

typedef struct {
    int v5;
    int v7;
} Env_v103;

typedef struct {
} Env_v106;

// function implementations
int v44(void* env44, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

Closure* v45(void* env45, void* v2_raw) {
  Node* v2 = (Node*)v2_raw;
  Env_v44* env44 = malloc(sizeof(Env_v44));
  Closure* c44 = malloc(sizeof(Closure));
  c44->env = env44;
  c44->fn = (void* (*)(void*, void*))v44;
  return c44;
}

int v0(Node* v1) {
  Env_v45* env45 = malloc(sizeof(Env_v45));
  if (((v1) == NULL)) return 0;
  Closure* c45 = v45(env45, (void*)((v1)->head));
  return (int)(intptr_t)(c45)->fn((c45)->env, (v1)->tail);
}

Node* v51(void* env51, void* v19_raw) {
  Node* v19 = (Node*)v19_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Closure* c15 = v15(env15, (void*)(v19));
  return cons(((Env_v51*)env51)->v18, (Node*)(c15)->fn((c15)->env, ((Env_v51*)env51)->v17));
}

Closure* v52(void* env52, void* v18_raw) {
  Node* v18 = (Node*)v18_raw;
  Env_v51* env51 = malloc(sizeof(Env_v51));
  env51->v18 = v18;
  env51->v17 = ((Env_v52*)env52)->v17;
  Closure* c51 = malloc(sizeof(Closure));
  c51->env = env51;
  c51->fn = (void* (*)(void*, void*))v51;
  return c51;
}

Node* v54(void* env54, void* v17_raw) {
  Node* v17 = (Node*)v17_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v17 = v17;
  if (((((Env_v54*)env54)->v16) == NULL)) return v17;
  Closure* c52 = v52(env52, (void*)((((Env_v54*)env54)->v16)->head));
  return (Node*)(c52)->fn((c52)->env, (((Env_v54*)env54)->v16)->tail);
}

Closure* v15(void* env15, void* v16_raw) {
  Node* v16 = (Node*)v16_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v16 = v16;
  Closure* c54 = malloc(sizeof(Closure));
  c54->env = env54;
  c54->fn = (void* (*)(void*, void*))v54;
  return c54;
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

bool v67(void* env67, void* v33_raw) {
  Node* v33 = (Node*)v33_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Env_v61* env61 = malloc(sizeof(Env_v61));
  Closure* c29 = v29(env29, (void*)(((Env_v67*)env67)->v30));
  Closure* c61 = v61(env61, (void*)(((Env_v67*)env67)->v30));
  return (!((Closure*)(c61)->fn((c61)->env, ((Env_v67*)env67)->v32)) && (bool)(intptr_t)(c29)->fn((c29)->env, v33));
}

Closure* v68(void* env68, void* v32_raw) {
  Pair_Int_Int *v32 = (Pair_Int_Int*)v32_raw;
  Env_v67* env67 = malloc(sizeof(Env_v67));
  env67->v32 = v32;
  env67->v30 = ((Env_v68*)env68)->v30;
  Closure* c67 = malloc(sizeof(Closure));
  c67->env = env67;
  c67->fn = (void* (*)(void*, void*))v67;
  return c67;
}

bool v70(void* env70, void* v31_raw) {
  Node* v31 = (Node*)v31_raw;
  Env_v68* env68 = malloc(sizeof(Env_v68));
  env68->v30 = ((Env_v70*)env70)->v30;
  if (((v31) == NULL)) return true;
  Closure* c68 = v68(env68, (void*)((v31)->head));
  return (bool)(intptr_t)(c68)->fn((c68)->env, (v31)->tail);
}

Closure* v29(void* env29, void* v30_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Env_v70* env70 = malloc(sizeof(Env_v70));
  env70->v30 = v30;
  Closure* c70 = malloc(sizeof(Closure));
  c70->env = env70;
  c70->fn = (void* (*)(void*, void*))v70;
  return c70;
}

Node* v74(void* env74, void* v28_raw) {
  Node* v28 = (Node*)v28_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Closure* c29 = v29(env29, (void*)(((Env_v74*)env74)->v26));
  if ((bool)(intptr_t)(c29)->fn((c29)->env, ((Env_v74*)env74)->v23)) {
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
  Closure* c20 = v20(env20, box_int(((Env_v80*)env80)->v21));
  Closure* c109 = (c20)->fn((c20)->env, box_int(((Env_v80*)env80)->v22));
  Closure* c110 = (c109)->fn((c109)->env, ((Env_v80*)env80)->v23);
  return v74(env74, (void*)((Node*)(c110)->fn((c110)->env, box_int((((Env_v80*)env80)->v24 + 1)))));
}

Node* v82(void* env82, void* v24_raw) {
  int v24 = *(int*)v24_raw;
  Env_v80* env80 = malloc(sizeof(Env_v80));
  env80->v24 = v24;
  env80->v21 = ((Env_v82*)env82)->v21;
  env80->v22 = ((Env_v82*)env82)->v22;
  env80->v23 = ((Env_v82*)env82)->v23;
  if ((v24 == ((Env_v82*)env82)->v21)) return NULL;
  return v80(env80, (void*)(makePair_Int_Int(((Env_v82*)env82)->v22, v24)));
}

Closure* v83(void* env83, void* v23_raw) {
  Node* v23 = (Node*)v23_raw;
  Env_v82* env82 = malloc(sizeof(Env_v82));
  env82->v23 = v23;
  env82->v21 = ((Env_v83*)env83)->v21;
  env82->v22 = ((Env_v83*)env83)->v22;
  Closure* c82 = malloc(sizeof(Closure));
  c82->env = env82;
  c82->fn = (void* (*)(void*, void*))v82;
  return c82;
}

Closure* v84(void* env84, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v83* env83 = malloc(sizeof(Env_v83));
  env83->v22 = v22;
  env83->v21 = ((Env_v84*)env84)->v21;
  Closure* c83 = malloc(sizeof(Closure));
  c83->env = env83;
  c83->fn = (void* (*)(void*, void*))v83;
  return c83;
}

Closure* v20(void* env20, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v21 = v21;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

Node* v94(void* env94, void* v14_raw) {
  Node* v14 = (Node*)v14_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Env_v20* env20 = malloc(sizeof(Env_v20));
  Closure* c20 = v20(env20, box_int(((Env_v94*)env94)->v10));
  Closure* c111 = (c20)->fn((c20)->env, box_int(((Env_v94*)env94)->v11));
  Closure* c112 = (c111)->fn((c111)->env, ((Env_v94*)env94)->v13);
  Closure* c9 = v9(env9, box_int(((Env_v94*)env94)->v10));
  Closure* c113 = (c9)->fn((c9)->env, box_int(((Env_v94*)env94)->v11));
  Closure* c15 = v15(env15, (void*)((Node*)(c112)->fn((c112)->env, box_int(0))));
  return (Node*)(c15)->fn((c15)->env, (Node*)(c113)->fn((c113)->env, v14));
}

Closure* v95(void* env95, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  Env_v94* env94 = malloc(sizeof(Env_v94));
  env94->v13 = v13;
  env94->v10 = ((Env_v95*)env95)->v10;
  env94->v11 = ((Env_v95*)env95)->v11;
  Closure* c94 = malloc(sizeof(Closure));
  c94->env = env94;
  c94->fn = (void* (*)(void*, void*))v94;
  return c94;
}

Node* v97(void* env97, void* v12_raw) {
  Node* v12 = (Node*)v12_raw;
  Env_v95* env95 = malloc(sizeof(Env_v95));
  env95->v10 = ((Env_v97*)env97)->v10;
  env95->v11 = ((Env_v97*)env97)->v11;
  if (((v12) == NULL)) return NULL;
  Closure* c95 = v95(env95, (void*)((v12)->head));
  return (Node*)(c95)->fn((c95)->env, (v12)->tail);
}

Closure* v98(void* env98, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v97* env97 = malloc(sizeof(Env_v97));
  env97->v11 = v11;
  env97->v10 = ((Env_v98*)env98)->v10;
  Closure* c97 = malloc(sizeof(Closure));
  c97->env = env97;
  c97->fn = (void* (*)(void*, void*))v97;
  return c97;
}

Closure* v9(void* env9, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v98* env98 = malloc(sizeof(Env_v98));
  env98->v10 = v10;
  Closure* c98 = malloc(sizeof(Closure));
  c98->env = env98;
  c98->fn = (void* (*)(void*, void*))v98;
  return c98;
}

Node* v103(void* env103, void* v8_raw) {
  Node* v8 = (Node*)v8_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = ((Env_v103*)env103)->v5;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  if ((((Env_v103*)env103)->v7 == ((Env_v103*)env103)->v5)) return v8;
  Closure* c9 = v9(env9, box_int(((Env_v103*)env103)->v5));
  Closure* c114 = (c9)->fn((c9)->env, box_int(((Env_v103*)env103)->v7));
  Closure* c6 = v6(env6, box_int((((Env_v103*)env103)->v7 + 1)));
  return (Node*)(c6)->fn((c6)->env, (Node*)(c114)->fn((c114)->env, v8));
}

Closure* v6(void* env6, void* v7_raw) {
  int v7 = *(int*)v7_raw;
  Env_v103* env103 = malloc(sizeof(Env_v103));
  env103->v7 = v7;
  env103->v5 = ((Env_v6*)env6)->v5;
  Closure* c103 = malloc(sizeof(Closure));
  c103->env = env103;
  c103->fn = (void* (*)(void*, void*))v103;
  return c103;
}

Node* v106(int v5) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = v5;
  Closure* c6 = v6(env6, box_int(0));
  return (Node*)(c6)->fn((c6)->env, cons(NULL, NULL));
}

// main
int main(void) {
  printInt(v0(v106(4)));
  return 0;
}

