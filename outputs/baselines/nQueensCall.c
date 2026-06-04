
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
int v39(void* env39, void* v3_raw);
Closure* v40(void* env40, void* v2_raw);
int v0(Node* v1);
Node* v44(void* env44, void* v5_raw);
Node* v49(void* env49, void* v19_raw);
Closure* v50(void* env50, void* v18_raw);
Node* v52(void* env52, void* v17_raw);
Closure* v15(void* env15, void* v16_raw);
bool v54(void* env54, void* v37_raw);
bool v56(void* env56, void* v36_raw);
bool v58(void* env58, void* v35_raw);
Closure* v59(void* env59, void* v34_raw);
bool (*v61(void* env61, void* v33_raw))(Pair_Int_Int*);
bool (*v63(void* env63, void* v32_raw))(Pair_Int_Int*);
bool v69(void* env69, void* v31_raw);
Closure* v70(void* env70, void* v30_raw);
bool v72(void* env72, void* v29_raw);
Closure* v27(void* env27, void* v28_raw);
Node* v76(void* env76, void* v26_raw);
Node* v82(void* env82, void* v25_raw);
Node* v84(void* env84, void* v24_raw);
Closure* v85(void* env85, void* v23_raw);
Closure* v86(void* env86, void* v22_raw);
Closure* v20(void* env20, void* v21_raw);
Node* v96(void* env96, void* v14_raw);
Closure* v97(void* env97, void* v13_raw);
Node* v99(void* env99, void* v12_raw);
Closure* v100(void* env100, void* v11_raw);
Closure* v9(void* env9, void* v10_raw);
Node* v105(void* env105, void* v8_raw);
Closure* v6(void* env6, void* v7_raw);
Node* v107(int v4);

// closure defitions
typedef struct {
    Node* v3;
    Node* v1;
    Node* v2;
} Env_v39;

typedef struct {
    Node* v2;
    Node* v1;
} Env_v40;

typedef struct {
    Node* v1;
} Env_v0;

typedef struct {
    Node* (*(*v5)(int))(Node*);
    int v4;
} Env_v44;

typedef struct {
    Node* v19;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    Node* v16;
    Node* v17;
    Node* v18;
} Env_v49;

typedef struct {
    Node* v18;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    Node* v16;
    Node* v17;
} Env_v50;

typedef struct {
    Node* v17;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    Node* v16;
} Env_v52;

typedef struct {
    Node* v16;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
} Env_v15;

typedef struct {
    int v37;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v32;
    int v33;
    int v34;
    Pair_Int_Int *v35;
    int v36;
} Env_v54;

typedef struct {
    int v36;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v32;
    int v33;
    int v34;
    Pair_Int_Int *v35;
} Env_v56;

typedef struct {
    Pair_Int_Int *v35;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v32;
    int v33;
    int v34;
} Env_v58;

typedef struct {
    int v34;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v32;
    int v33;
} Env_v59;

typedef struct {
    int v33;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v32;
} Env_v61;

typedef struct {
    Pair_Int_Int *v32;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
    Node* v31;
} Env_v63;

typedef struct {
    Node* v31;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
    Pair_Int_Int *v30;
} Env_v69;

typedef struct {
    Pair_Int_Int *v30;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
    Node* v29;
} Env_v70;

typedef struct {
    Node* v29;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
    Pair_Int_Int *v28;
} Env_v72;

typedef struct {
    Pair_Int_Int *v28;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
    Node* v26;
} Env_v27;

typedef struct {
    Node* v26;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
    Pair_Int_Int *v25;
} Env_v76;

typedef struct {
    Pair_Int_Int *v25;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
    int v24;
} Env_v82;

typedef struct {
    int v24;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
    Node* v23;
} Env_v84;

typedef struct {
    Node* v23;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
    int v22;
} Env_v85;

typedef struct {
    int v22;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
    int v21;
} Env_v86;

typedef struct {
    int v21;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
    Node* v14;
} Env_v20;

typedef struct {
    Node* v14;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
    Node* v13;
} Env_v96;

typedef struct {
    Node* v13;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
    Node* v12;
} Env_v97;

typedef struct {
    Node* v12;
    int v4;
    int v7;
    Node* v8;
    int v10;
    int v11;
} Env_v99;

typedef struct {
    int v11;
    int v4;
    int v7;
    Node* v8;
    int v10;
} Env_v100;

typedef struct {
    int v10;
    int v4;
    int v7;
    Node* v8;
} Env_v9;

typedef struct {
    Node* v8;
    int v4;
    int v7;
} Env_v105;

typedef struct {
    int v7;
    int v4;
} Env_v6;

typedef struct {
    int v4;
} Env_v107;

// function implementations
int v39(void* env39, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

Closure* v40(void* env40, void* v2_raw) {
  Node* v2 = (Node*)v2_raw;
  Env_v39* env39 = malloc(sizeof(Env_v39));
  env39->v2 = v2;
  env39->v1 = ((Env_v40*)env40)->v1;
  Closure* c39 = malloc(sizeof(Closure));
  c39->env = env39;
  c39->fn = (void* (*)(void*, void*))v39;
  return c39;
}

int v0(Node* v1) {
  Env_v40* env40 = malloc(sizeof(Env_v40));
  env40->v1 = v1;
  if (((v1) == NULL)) {
    return 0;
  } else {
    Closure* c40 = v40(env40, (void*)((v1)->head));
    return (void*)(c40)->fn((c40)->env, (v1)->tail);
  }
}

Node* v44(void* env44, void* v5_raw) {
  Node* (*(*v5)(int))(Node*) = (Node* (*)(Node*) (*)(int))v5_raw;
  return v5(0)(cons(NULL, NULL));
}

Node* v49(void* env49, void* v19_raw) {
  Node* v19 = (Node*)v19_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  env15->v4 = ((Env_v49*)env49)->v4;
  env15->v7 = ((Env_v49*)env49)->v7;
  env15->v8 = ((Env_v49*)env49)->v8;
  env15->v10 = ((Env_v49*)env49)->v10;
  env15->v11 = ((Env_v49*)env49)->v11;
  env15->v12 = ((Env_v49*)env49)->v12;
  env15->v13 = ((Env_v49*)env49)->v13;
  env15->v14 = ((Env_v49*)env49)->v14;
  Closure* c15 = v15(env15, (void*)(v19));
  return cons(((Env_v49*)env49)->v18, (void*)(c15)->fn((c15)->env, ((Env_v49*)env49)->v17));
}

Closure* v50(void* env50, void* v18_raw) {
  Node* v18 = (Node*)v18_raw;
  Env_v49* env49 = malloc(sizeof(Env_v49));
  env49->v18 = v18;
  env49->v4 = ((Env_v50*)env50)->v4;
  env49->v7 = ((Env_v50*)env50)->v7;
  env49->v8 = ((Env_v50*)env50)->v8;
  env49->v10 = ((Env_v50*)env50)->v10;
  env49->v11 = ((Env_v50*)env50)->v11;
  env49->v12 = ((Env_v50*)env50)->v12;
  env49->v13 = ((Env_v50*)env50)->v13;
  env49->v14 = ((Env_v50*)env50)->v14;
  env49->v16 = ((Env_v50*)env50)->v16;
  env49->v17 = ((Env_v50*)env50)->v17;
  Closure* c49 = malloc(sizeof(Closure));
  c49->env = env49;
  c49->fn = (void* (*)(void*, void*))v49;
  return c49;
}

Node* v52(void* env52, void* v17_raw) {
  Node* v17 = (Node*)v17_raw;
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v17 = v17;
  env50->v4 = ((Env_v52*)env52)->v4;
  env50->v7 = ((Env_v52*)env52)->v7;
  env50->v8 = ((Env_v52*)env52)->v8;
  env50->v10 = ((Env_v52*)env52)->v10;
  env50->v11 = ((Env_v52*)env52)->v11;
  env50->v12 = ((Env_v52*)env52)->v12;
  env50->v13 = ((Env_v52*)env52)->v13;
  env50->v14 = ((Env_v52*)env52)->v14;
  env50->v16 = ((Env_v52*)env52)->v16;
  if (((((Env_v52*)env52)->v16) == NULL)) {
    return v17;
  } else {
    Closure* c50 = v50(env50, (void*)((((Env_v52*)env52)->v16)->head));
    return (void*)(c50)->fn((c50)->env, (((Env_v52*)env52)->v16)->tail);
  }
}

Closure* v15(void* env15, void* v16_raw) {
  Node* v16 = (Node*)v16_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v16 = v16;
  env52->v4 = ((Env_v15*)env15)->v4;
  env52->v7 = ((Env_v15*)env15)->v7;
  env52->v8 = ((Env_v15*)env15)->v8;
  env52->v10 = ((Env_v15*)env15)->v10;
  env52->v11 = ((Env_v15*)env15)->v11;
  env52->v12 = ((Env_v15*)env15)->v12;
  env52->v13 = ((Env_v15*)env15)->v13;
  env52->v14 = ((Env_v15*)env15)->v14;
  Closure* c52 = malloc(sizeof(Closure));
  c52->env = env52;
  c52->fn = (void* (*)(void*, void*))v52;
  return c52;
}

bool v54(void* env54, void* v37_raw) {
  int v37 = *(int*)v37_raw;
  return ((((Env_v54*)env54)->v34 == v37) || (abs((((Env_v54*)env54)->v34 - v37)) == abs((((Env_v54*)env54)->v33 - ((Env_v54*)env54)->v36))));
}

bool v56(void* env56, void* v36_raw) {
  int v36 = *(int*)v36_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v36 = v36;
  env54->v4 = ((Env_v56*)env56)->v4;
  env54->v7 = ((Env_v56*)env56)->v7;
  env54->v8 = ((Env_v56*)env56)->v8;
  env54->v10 = ((Env_v56*)env56)->v10;
  env54->v11 = ((Env_v56*)env56)->v11;
  env54->v12 = ((Env_v56*)env56)->v12;
  env54->v13 = ((Env_v56*)env56)->v13;
  env54->v14 = ((Env_v56*)env56)->v14;
  env54->v21 = ((Env_v56*)env56)->v21;
  env54->v22 = ((Env_v56*)env56)->v22;
  env54->v23 = ((Env_v56*)env56)->v23;
  env54->v24 = ((Env_v56*)env56)->v24;
  env54->v25 = ((Env_v56*)env56)->v25;
  env54->v26 = ((Env_v56*)env56)->v26;
  env54->v28 = ((Env_v56*)env56)->v28;
  env54->v29 = ((Env_v56*)env56)->v29;
  env54->v30 = ((Env_v56*)env56)->v30;
  env54->v31 = ((Env_v56*)env56)->v31;
  env54->v32 = ((Env_v56*)env56)->v32;
  env54->v33 = ((Env_v56*)env56)->v33;
  env54->v34 = ((Env_v56*)env56)->v34;
  env54->v35 = ((Env_v56*)env56)->v35;
  return v54(env54, box_int((((Env_v56*)env56)->v35)->snd));
}

bool v58(void* env58, void* v35_raw) {
  Pair_Int_Int *v35 = (Pair_Int_Int*)v35_raw;
  Env_v56* env56 = malloc(sizeof(Env_v56));
  env56->v35 = v35;
  env56->v4 = ((Env_v58*)env58)->v4;
  env56->v7 = ((Env_v58*)env58)->v7;
  env56->v8 = ((Env_v58*)env58)->v8;
  env56->v10 = ((Env_v58*)env58)->v10;
  env56->v11 = ((Env_v58*)env58)->v11;
  env56->v12 = ((Env_v58*)env58)->v12;
  env56->v13 = ((Env_v58*)env58)->v13;
  env56->v14 = ((Env_v58*)env58)->v14;
  env56->v21 = ((Env_v58*)env58)->v21;
  env56->v22 = ((Env_v58*)env58)->v22;
  env56->v23 = ((Env_v58*)env58)->v23;
  env56->v24 = ((Env_v58*)env58)->v24;
  env56->v25 = ((Env_v58*)env58)->v25;
  env56->v26 = ((Env_v58*)env58)->v26;
  env56->v28 = ((Env_v58*)env58)->v28;
  env56->v29 = ((Env_v58*)env58)->v29;
  env56->v30 = ((Env_v58*)env58)->v30;
  env56->v31 = ((Env_v58*)env58)->v31;
  env56->v32 = ((Env_v58*)env58)->v32;
  env56->v33 = ((Env_v58*)env58)->v33;
  env56->v34 = ((Env_v58*)env58)->v34;
  return v56(env56, box_int((v35)->fst));
}

Closure* v59(void* env59, void* v34_raw) {
  int v34 = *(int*)v34_raw;
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v34 = v34;
  env58->v4 = ((Env_v59*)env59)->v4;
  env58->v7 = ((Env_v59*)env59)->v7;
  env58->v8 = ((Env_v59*)env59)->v8;
  env58->v10 = ((Env_v59*)env59)->v10;
  env58->v11 = ((Env_v59*)env59)->v11;
  env58->v12 = ((Env_v59*)env59)->v12;
  env58->v13 = ((Env_v59*)env59)->v13;
  env58->v14 = ((Env_v59*)env59)->v14;
  env58->v21 = ((Env_v59*)env59)->v21;
  env58->v22 = ((Env_v59*)env59)->v22;
  env58->v23 = ((Env_v59*)env59)->v23;
  env58->v24 = ((Env_v59*)env59)->v24;
  env58->v25 = ((Env_v59*)env59)->v25;
  env58->v26 = ((Env_v59*)env59)->v26;
  env58->v28 = ((Env_v59*)env59)->v28;
  env58->v29 = ((Env_v59*)env59)->v29;
  env58->v30 = ((Env_v59*)env59)->v30;
  env58->v31 = ((Env_v59*)env59)->v31;
  env58->v32 = ((Env_v59*)env59)->v32;
  env58->v33 = ((Env_v59*)env59)->v33;
  Closure* c58 = malloc(sizeof(Closure));
  c58->env = env58;
  c58->fn = (void* (*)(void*, void*))v58;
  return c58;
}

bool (*v61(void* env61, void* v33_raw))(Pair_Int_Int*) {
  int v33 = *(int*)v33_raw;
  Env_v59* env59 = malloc(sizeof(Env_v59));
  env59->v33 = v33;
  env59->v4 = ((Env_v61*)env61)->v4;
  env59->v7 = ((Env_v61*)env61)->v7;
  env59->v8 = ((Env_v61*)env61)->v8;
  env59->v10 = ((Env_v61*)env61)->v10;
  env59->v11 = ((Env_v61*)env61)->v11;
  env59->v12 = ((Env_v61*)env61)->v12;
  env59->v13 = ((Env_v61*)env61)->v13;
  env59->v14 = ((Env_v61*)env61)->v14;
  env59->v21 = ((Env_v61*)env61)->v21;
  env59->v22 = ((Env_v61*)env61)->v22;
  env59->v23 = ((Env_v61*)env61)->v23;
  env59->v24 = ((Env_v61*)env61)->v24;
  env59->v25 = ((Env_v61*)env61)->v25;
  env59->v26 = ((Env_v61*)env61)->v26;
  env59->v28 = ((Env_v61*)env61)->v28;
  env59->v29 = ((Env_v61*)env61)->v29;
  env59->v30 = ((Env_v61*)env61)->v30;
  env59->v31 = ((Env_v61*)env61)->v31;
  env59->v32 = ((Env_v61*)env61)->v32;
  Closure* c59 = v59(env59, box_int((((Env_v61*)env61)->v32)->snd));
  return c59;
}

bool (*v63(void* env63, void* v32_raw))(Pair_Int_Int*) {
  Pair_Int_Int *v32 = (Pair_Int_Int*)v32_raw;
  Env_v61* env61 = malloc(sizeof(Env_v61));
  env61->v32 = v32;
  env61->v4 = ((Env_v63*)env63)->v4;
  env61->v7 = ((Env_v63*)env63)->v7;
  env61->v8 = ((Env_v63*)env63)->v8;
  env61->v10 = ((Env_v63*)env63)->v10;
  env61->v11 = ((Env_v63*)env63)->v11;
  env61->v12 = ((Env_v63*)env63)->v12;
  env61->v13 = ((Env_v63*)env63)->v13;
  env61->v14 = ((Env_v63*)env63)->v14;
  env61->v21 = ((Env_v63*)env63)->v21;
  env61->v22 = ((Env_v63*)env63)->v22;
  env61->v23 = ((Env_v63*)env63)->v23;
  env61->v24 = ((Env_v63*)env63)->v24;
  env61->v25 = ((Env_v63*)env63)->v25;
  env61->v26 = ((Env_v63*)env63)->v26;
  env61->v28 = ((Env_v63*)env63)->v28;
  env61->v29 = ((Env_v63*)env63)->v29;
  env61->v30 = ((Env_v63*)env63)->v30;
  env61->v31 = ((Env_v63*)env63)->v31;
  return v61(env61, box_int((v32)->fst));
}

bool v69(void* env69, void* v31_raw) {
  Node* v31 = (Node*)v31_raw;
  Env_v27* env27 = malloc(sizeof(Env_v27));
  env27->v4 = ((Env_v69*)env69)->v4;
  env27->v7 = ((Env_v69*)env69)->v7;
  env27->v8 = ((Env_v69*)env69)->v8;
  env27->v10 = ((Env_v69*)env69)->v10;
  env27->v11 = ((Env_v69*)env69)->v11;
  env27->v12 = ((Env_v69*)env69)->v12;
  env27->v13 = ((Env_v69*)env69)->v13;
  env27->v14 = ((Env_v69*)env69)->v14;
  env27->v21 = ((Env_v69*)env69)->v21;
  env27->v22 = ((Env_v69*)env69)->v22;
  env27->v23 = ((Env_v69*)env69)->v23;
  env27->v24 = ((Env_v69*)env69)->v24;
  env27->v25 = ((Env_v69*)env69)->v25;
  env27->v26 = ((Env_v69*)env69)->v26;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  env63->v31 = v31;
  env63->v4 = ((Env_v69*)env69)->v4;
  env63->v7 = ((Env_v69*)env69)->v7;
  env63->v8 = ((Env_v69*)env69)->v8;
  env63->v10 = ((Env_v69*)env69)->v10;
  env63->v11 = ((Env_v69*)env69)->v11;
  env63->v12 = ((Env_v69*)env69)->v12;
  env63->v13 = ((Env_v69*)env69)->v13;
  env63->v14 = ((Env_v69*)env69)->v14;
  env63->v21 = ((Env_v69*)env69)->v21;
  env63->v22 = ((Env_v69*)env69)->v22;
  env63->v23 = ((Env_v69*)env69)->v23;
  env63->v24 = ((Env_v69*)env69)->v24;
  env63->v25 = ((Env_v69*)env69)->v25;
  env63->v26 = ((Env_v69*)env69)->v26;
  env63->v28 = ((Env_v69*)env69)->v28;
  env63->v29 = ((Env_v69*)env69)->v29;
  env63->v30 = ((Env_v69*)env69)->v30;
  Closure* c27 = v27(env27, (void*)(((Env_v69*)env69)->v28));
  return (!(v63(env63, (void*)(((Env_v69*)env69)->v28))(((Env_v69*)env69)->v30)) && (void*)(c27)->fn((c27)->env, v31));
}

Closure* v70(void* env70, void* v30_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v30 = v30;
  env69->v4 = ((Env_v70*)env70)->v4;
  env69->v7 = ((Env_v70*)env70)->v7;
  env69->v8 = ((Env_v70*)env70)->v8;
  env69->v10 = ((Env_v70*)env70)->v10;
  env69->v11 = ((Env_v70*)env70)->v11;
  env69->v12 = ((Env_v70*)env70)->v12;
  env69->v13 = ((Env_v70*)env70)->v13;
  env69->v14 = ((Env_v70*)env70)->v14;
  env69->v21 = ((Env_v70*)env70)->v21;
  env69->v22 = ((Env_v70*)env70)->v22;
  env69->v23 = ((Env_v70*)env70)->v23;
  env69->v24 = ((Env_v70*)env70)->v24;
  env69->v25 = ((Env_v70*)env70)->v25;
  env69->v26 = ((Env_v70*)env70)->v26;
  env69->v28 = ((Env_v70*)env70)->v28;
  env69->v29 = ((Env_v70*)env70)->v29;
  Closure* c69 = malloc(sizeof(Closure));
  c69->env = env69;
  c69->fn = (void* (*)(void*, void*))v69;
  return c69;
}

bool v72(void* env72, void* v29_raw) {
  Node* v29 = (Node*)v29_raw;
  Env_v70* env70 = malloc(sizeof(Env_v70));
  env70->v29 = v29;
  env70->v4 = ((Env_v72*)env72)->v4;
  env70->v7 = ((Env_v72*)env72)->v7;
  env70->v8 = ((Env_v72*)env72)->v8;
  env70->v10 = ((Env_v72*)env72)->v10;
  env70->v11 = ((Env_v72*)env72)->v11;
  env70->v12 = ((Env_v72*)env72)->v12;
  env70->v13 = ((Env_v72*)env72)->v13;
  env70->v14 = ((Env_v72*)env72)->v14;
  env70->v21 = ((Env_v72*)env72)->v21;
  env70->v22 = ((Env_v72*)env72)->v22;
  env70->v23 = ((Env_v72*)env72)->v23;
  env70->v24 = ((Env_v72*)env72)->v24;
  env70->v25 = ((Env_v72*)env72)->v25;
  env70->v26 = ((Env_v72*)env72)->v26;
  env70->v28 = ((Env_v72*)env72)->v28;
  if (((v29) == NULL)) {
    return true;
  } else {
    Closure* c70 = v70(env70, (void*)((v29)->head));
    return (void*)(c70)->fn((c70)->env, (v29)->tail);
  }
}

Closure* v27(void* env27, void* v28_raw) {
  Pair_Int_Int *v28 = (Pair_Int_Int*)v28_raw;
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v28 = v28;
  env72->v4 = ((Env_v27*)env27)->v4;
  env72->v7 = ((Env_v27*)env27)->v7;
  env72->v8 = ((Env_v27*)env27)->v8;
  env72->v10 = ((Env_v27*)env27)->v10;
  env72->v11 = ((Env_v27*)env27)->v11;
  env72->v12 = ((Env_v27*)env27)->v12;
  env72->v13 = ((Env_v27*)env27)->v13;
  env72->v14 = ((Env_v27*)env27)->v14;
  env72->v21 = ((Env_v27*)env27)->v21;
  env72->v22 = ((Env_v27*)env27)->v22;
  env72->v23 = ((Env_v27*)env27)->v23;
  env72->v24 = ((Env_v27*)env27)->v24;
  env72->v25 = ((Env_v27*)env27)->v25;
  env72->v26 = ((Env_v27*)env27)->v26;
  Closure* c72 = malloc(sizeof(Closure));
  c72->env = env72;
  c72->fn = (void* (*)(void*, void*))v72;
  return c72;
}

Node* v76(void* env76, void* v26_raw) {
  Node* v26 = (Node*)v26_raw;
  Env_v27* env27 = malloc(sizeof(Env_v27));
  env27->v26 = v26;
  env27->v4 = ((Env_v76*)env76)->v4;
  env27->v7 = ((Env_v76*)env76)->v7;
  env27->v8 = ((Env_v76*)env76)->v8;
  env27->v10 = ((Env_v76*)env76)->v10;
  env27->v11 = ((Env_v76*)env76)->v11;
  env27->v12 = ((Env_v76*)env76)->v12;
  env27->v13 = ((Env_v76*)env76)->v13;
  env27->v14 = ((Env_v76*)env76)->v14;
  env27->v21 = ((Env_v76*)env76)->v21;
  env27->v22 = ((Env_v76*)env76)->v22;
  env27->v23 = ((Env_v76*)env76)->v23;
  env27->v24 = ((Env_v76*)env76)->v24;
  env27->v25 = ((Env_v76*)env76)->v25;
  Closure* c27 = v27(env27, (void*)(((Env_v76*)env76)->v25));
  if ((void*)(c27)->fn((c27)->env, ((Env_v76*)env76)->v23)) {
    return cons(cons(((Env_v76*)env76)->v25, ((Env_v76*)env76)->v23), v26);
  } else {
    return v26;
  }
}

Node* v82(void* env82, void* v25_raw) {
  Pair_Int_Int *v25 = (Pair_Int_Int*)v25_raw;
  Env_v20* env20 = malloc(sizeof(Env_v20));
  env20->v4 = ((Env_v82*)env82)->v4;
  env20->v7 = ((Env_v82*)env82)->v7;
  env20->v8 = ((Env_v82*)env82)->v8;
  env20->v10 = ((Env_v82*)env82)->v10;
  env20->v11 = ((Env_v82*)env82)->v11;
  env20->v12 = ((Env_v82*)env82)->v12;
  env20->v13 = ((Env_v82*)env82)->v13;
  env20->v14 = ((Env_v82*)env82)->v14;
  Env_v76* env76 = malloc(sizeof(Env_v76));
  env76->v25 = v25;
  env76->v4 = ((Env_v82*)env82)->v4;
  env76->v7 = ((Env_v82*)env82)->v7;
  env76->v8 = ((Env_v82*)env82)->v8;
  env76->v10 = ((Env_v82*)env82)->v10;
  env76->v11 = ((Env_v82*)env82)->v11;
  env76->v12 = ((Env_v82*)env82)->v12;
  env76->v13 = ((Env_v82*)env82)->v13;
  env76->v14 = ((Env_v82*)env82)->v14;
  env76->v21 = ((Env_v82*)env82)->v21;
  env76->v22 = ((Env_v82*)env82)->v22;
  env76->v23 = ((Env_v82*)env82)->v23;
  env76->v24 = ((Env_v82*)env82)->v24;
  Closure* c20 = v20(env20, box_int(((Env_v82*)env82)->v21));
  return v76(env76, (void*)(((c20)->fn((c20)->env, box_int(((Env_v82*)env82)->v22), ((Env_v82*)env82)->v23))->fn(((c20)->fn((c20)->env, box_int(((Env_v82*)env82)->v22), ((Env_v82*)env82)->v23))->env, box_int((((Env_v82*)env82)->v24 + 1)))));
}

Node* v84(void* env84, void* v24_raw) {
  int v24 = *(int*)v24_raw;
  Env_v82* env82 = malloc(sizeof(Env_v82));
  env82->v24 = v24;
  env82->v4 = ((Env_v84*)env84)->v4;
  env82->v7 = ((Env_v84*)env84)->v7;
  env82->v8 = ((Env_v84*)env84)->v8;
  env82->v10 = ((Env_v84*)env84)->v10;
  env82->v11 = ((Env_v84*)env84)->v11;
  env82->v12 = ((Env_v84*)env84)->v12;
  env82->v13 = ((Env_v84*)env84)->v13;
  env82->v14 = ((Env_v84*)env84)->v14;
  env82->v21 = ((Env_v84*)env84)->v21;
  env82->v22 = ((Env_v84*)env84)->v22;
  env82->v23 = ((Env_v84*)env84)->v23;
  if ((v24 == ((Env_v84*)env84)->v21)) {
    return NULL;
  } else {
    return v82(env82, (void*)(makePair_Int_Int(((Env_v84*)env84)->v22, v24)));
  }
}

Closure* v85(void* env85, void* v23_raw) {
  Node* v23 = (Node*)v23_raw;
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v23 = v23;
  env84->v4 = ((Env_v85*)env85)->v4;
  env84->v7 = ((Env_v85*)env85)->v7;
  env84->v8 = ((Env_v85*)env85)->v8;
  env84->v10 = ((Env_v85*)env85)->v10;
  env84->v11 = ((Env_v85*)env85)->v11;
  env84->v12 = ((Env_v85*)env85)->v12;
  env84->v13 = ((Env_v85*)env85)->v13;
  env84->v14 = ((Env_v85*)env85)->v14;
  env84->v21 = ((Env_v85*)env85)->v21;
  env84->v22 = ((Env_v85*)env85)->v22;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

Closure* v86(void* env86, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v85* env85 = malloc(sizeof(Env_v85));
  env85->v22 = v22;
  env85->v4 = ((Env_v86*)env86)->v4;
  env85->v7 = ((Env_v86*)env86)->v7;
  env85->v8 = ((Env_v86*)env86)->v8;
  env85->v10 = ((Env_v86*)env86)->v10;
  env85->v11 = ((Env_v86*)env86)->v11;
  env85->v12 = ((Env_v86*)env86)->v12;
  env85->v13 = ((Env_v86*)env86)->v13;
  env85->v14 = ((Env_v86*)env86)->v14;
  env85->v21 = ((Env_v86*)env86)->v21;
  Closure* c85 = malloc(sizeof(Closure));
  c85->env = env85;
  c85->fn = (void* (*)(void*, void*))v85;
  return c85;
}

Closure* v20(void* env20, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v86* env86 = malloc(sizeof(Env_v86));
  env86->v21 = v21;
  env86->v4 = ((Env_v20*)env20)->v4;
  env86->v7 = ((Env_v20*)env20)->v7;
  env86->v8 = ((Env_v20*)env20)->v8;
  env86->v10 = ((Env_v20*)env20)->v10;
  env86->v11 = ((Env_v20*)env20)->v11;
  env86->v12 = ((Env_v20*)env20)->v12;
  env86->v13 = ((Env_v20*)env20)->v13;
  env86->v14 = ((Env_v20*)env20)->v14;
  Closure* c86 = malloc(sizeof(Closure));
  c86->env = env86;
  c86->fn = (void* (*)(void*, void*))v86;
  return c86;
}

Node* v96(void* env96, void* v14_raw) {
  Node* v14 = (Node*)v14_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  env9->v4 = ((Env_v96*)env96)->v4;
  env9->v7 = ((Env_v96*)env96)->v7;
  env9->v8 = ((Env_v96*)env96)->v8;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  env15->v14 = v14;
  env15->v4 = ((Env_v96*)env96)->v4;
  env15->v7 = ((Env_v96*)env96)->v7;
  env15->v8 = ((Env_v96*)env96)->v8;
  env15->v10 = ((Env_v96*)env96)->v10;
  env15->v11 = ((Env_v96*)env96)->v11;
  env15->v12 = ((Env_v96*)env96)->v12;
  env15->v13 = ((Env_v96*)env96)->v13;
  Env_v20* env20 = malloc(sizeof(Env_v20));
  env20->v14 = v14;
  env20->v4 = ((Env_v96*)env96)->v4;
  env20->v7 = ((Env_v96*)env96)->v7;
  env20->v8 = ((Env_v96*)env96)->v8;
  env20->v10 = ((Env_v96*)env96)->v10;
  env20->v11 = ((Env_v96*)env96)->v11;
  env20->v12 = ((Env_v96*)env96)->v12;
  env20->v13 = ((Env_v96*)env96)->v13;
  Closure* c15 = v15(env15, (void*)(v20(env20, box_int(((Env_v96*)env96)->v10))(((Env_v96*)env96)->v11, ((Env_v96*)env96)->v13, 0)));
  return (void*)(c15)->fn((c15)->env, v9(env9, box_int(((Env_v96*)env96)->v10))(((Env_v96*)env96)->v11, v14));
}

Closure* v97(void* env97, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  Env_v96* env96 = malloc(sizeof(Env_v96));
  env96->v13 = v13;
  env96->v4 = ((Env_v97*)env97)->v4;
  env96->v7 = ((Env_v97*)env97)->v7;
  env96->v8 = ((Env_v97*)env97)->v8;
  env96->v10 = ((Env_v97*)env97)->v10;
  env96->v11 = ((Env_v97*)env97)->v11;
  env96->v12 = ((Env_v97*)env97)->v12;
  Closure* c96 = malloc(sizeof(Closure));
  c96->env = env96;
  c96->fn = (void* (*)(void*, void*))v96;
  return c96;
}

Node* v99(void* env99, void* v12_raw) {
  Node* v12 = (Node*)v12_raw;
  Env_v97* env97 = malloc(sizeof(Env_v97));
  env97->v12 = v12;
  env97->v4 = ((Env_v99*)env99)->v4;
  env97->v7 = ((Env_v99*)env99)->v7;
  env97->v8 = ((Env_v99*)env99)->v8;
  env97->v10 = ((Env_v99*)env99)->v10;
  env97->v11 = ((Env_v99*)env99)->v11;
  if (((v12) == NULL)) {
    return NULL;
  } else {
    Closure* c97 = v97(env97, (void*)((v12)->head));
    return (void*)(c97)->fn((c97)->env, (v12)->tail);
  }
}

Closure* v100(void* env100, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v99* env99 = malloc(sizeof(Env_v99));
  env99->v11 = v11;
  env99->v4 = ((Env_v100*)env100)->v4;
  env99->v7 = ((Env_v100*)env100)->v7;
  env99->v8 = ((Env_v100*)env100)->v8;
  env99->v10 = ((Env_v100*)env100)->v10;
  Closure* c99 = malloc(sizeof(Closure));
  c99->env = env99;
  c99->fn = (void* (*)(void*, void*))v99;
  return c99;
}

Closure* v9(void* env9, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v100* env100 = malloc(sizeof(Env_v100));
  env100->v10 = v10;
  env100->v4 = ((Env_v9*)env9)->v4;
  env100->v7 = ((Env_v9*)env9)->v7;
  env100->v8 = ((Env_v9*)env9)->v8;
  Closure* c100 = malloc(sizeof(Closure));
  c100->env = env100;
  c100->fn = (void* (*)(void*, void*))v100;
  return c100;
}

Node* v105(void* env105, void* v8_raw) {
  Node* v8 = (Node*)v8_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v4 = ((Env_v105*)env105)->v4;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  env9->v8 = v8;
  env9->v4 = ((Env_v105*)env105)->v4;
  env9->v7 = ((Env_v105*)env105)->v7;
  if ((((Env_v105*)env105)->v7 == ((Env_v105*)env105)->v4)) {
    return v8;
  } else {
    Closure* c6 = v6(env6, box_int((((Env_v105*)env105)->v7 + 1)));
    return (void*)(c6)->fn((c6)->env, v9(env9, box_int(((Env_v105*)env105)->v4))(((Env_v105*)env105)->v7, v8));
  }
}

Closure* v6(void* env6, void* v7_raw) {
  int v7 = *(int*)v7_raw;
  Env_v105* env105 = malloc(sizeof(Env_v105));
  env105->v7 = v7;
  env105->v4 = ((Env_v6*)env6)->v4;
  Closure* c105 = malloc(sizeof(Closure));
  c105->env = env105;
  c105->fn = (void* (*)(void*, void*))v105;
  return c105;
}

Node* v107(int v4) {
  Env_v44* env44 = malloc(sizeof(Env_v44));
  env44->v4 = v4;
  return v44(env44, (void*)(v6));
}

// main
int main(void) {
  printInt(v0(v107(4)));
  return 0;
}

