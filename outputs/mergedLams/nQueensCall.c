
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
int v39(void* env39, void* v2_raw, void* v3_raw);
int v0(Node* v1);
Node* v46(void* env46, void* v17_raw, void* v18_raw);
Node* v14(void* env14, void* v15_raw, void* v16_raw);
bool v50(void* env50, void* v36_raw);
bool v52(void* env52, void* v35_raw);
bool v54(void* env54, void* v34_raw);
Closure* v55(void* env55, void* v33_raw);
Closure* v57(void* env57, void* v32_raw);
Closure* v59(void* env59, void* v31_raw);
bool v66(void* env66, void* v29_raw, void* v30_raw);
bool v26(void* env26, void* v27_raw, void* v28_raw);
Node* v72(void* env72, void* v25_raw);
Node* v78(void* env78, void* v24_raw);
Node* v19(void* env19, void* v20_raw, void* v21_raw, void* v22_raw, void* v23_raw);
Node* v93(void* env93, void* v12_raw, void* v13_raw);
Node* v8(void* env8, void* v9_raw, void* v10_raw, void* v11_raw);
Node* v5(void* env5, void* v6_raw, void* v7_raw);
Node* v104(int v4);

// env defitions
typedef struct {
    Node* v2;
    Node* v3;
    Node* v1;
} Env_v39;

typedef struct {
    Node* v1;
} Env_v0;

typedef struct {
    Node* v17;
    Node* v18;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    Node* v15;
    Node* v16;
} Env_v46;

typedef struct {
    Node* v15;
    Node* v16;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
} Env_v14;

typedef struct {
    int v36;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
    Pair_Int_Int *v31;
    int v32;
    int v33;
    Pair_Int_Int *v34;
    int v35;
} Env_v50;

typedef struct {
    int v35;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
    Pair_Int_Int *v31;
    int v32;
    int v33;
    Pair_Int_Int *v34;
} Env_v52;

typedef struct {
    Pair_Int_Int *v34;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
    Pair_Int_Int *v31;
    int v32;
    int v33;
} Env_v54;

typedef struct {
    int v33;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
    Pair_Int_Int *v31;
    int v32;
} Env_v55;

typedef struct {
    int v32;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
    Pair_Int_Int *v31;
} Env_v57;

typedef struct {
    Pair_Int_Int *v31;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
    Pair_Int_Int *v29;
    Node* v30;
} Env_v59;

typedef struct {
    Pair_Int_Int *v29;
    Node* v30;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
    Pair_Int_Int *v27;
    Node* v28;
} Env_v66;

typedef struct {
    Pair_Int_Int *v27;
    Node* v28;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
    Node* v25;
} Env_v26;

typedef struct {
    Node* v25;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
    Pair_Int_Int *v24;
} Env_v72;

typedef struct {
    Pair_Int_Int *v24;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
    int v20;
    int v21;
    Node* v22;
    int v23;
} Env_v78;

typedef struct {
    int v20;
    int v21;
    Node* v22;
    int v23;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
    Node* v12;
    Node* v13;
} Env_v19;

typedef struct {
    Node* v12;
    Node* v13;
    int v4;
    int v6;
    Node* v7;
    int v9;
    int v10;
    Node* v11;
} Env_v93;

typedef struct {
    int v9;
    int v10;
    Node* v11;
    int v4;
    int v6;
    Node* v7;
} Env_v8;

typedef struct {
    int v6;
    Node* v7;
    int v4;
} Env_v5;

typedef struct {
    int v4;
} Env_v104;

// function implementations
int v39(void* env39, void* v2_raw, void* v3_raw) {
  Node* v2 = (Node*)v2_raw;
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

int v0(Node* v1) {
  Env_v39* env39 = malloc(sizeof(Env_v39));
  env39->v1 = v1;
  if (((v1) == NULL)) {
    return 0;
  } else {
    return v39(env39, (void*)((v1)->head), (void*)((v1)->tail));
  }
}

Node* v46(void* env46, void* v17_raw, void* v18_raw) {
  Node* v17 = (Node*)v17_raw;
  Node* v18 = (Node*)v18_raw;
  Env_v14* env14 = malloc(sizeof(Env_v14));
  env14->v4 = ((Env_v46*)env46)->v4;
  env14->v6 = ((Env_v46*)env46)->v6;
  env14->v7 = ((Env_v46*)env46)->v7;
  env14->v9 = ((Env_v46*)env46)->v9;
  env14->v10 = ((Env_v46*)env46)->v10;
  env14->v11 = ((Env_v46*)env46)->v11;
  env14->v12 = ((Env_v46*)env46)->v12;
  env14->v13 = ((Env_v46*)env46)->v13;
  return cons(v17, v14(env14, (void*)(v18), (void*)(((Env_v46*)env46)->v16)));
}

Node* v14(void* env14, void* v15_raw, void* v16_raw) {
  Node* v15 = (Node*)v15_raw;
  Node* v16 = (Node*)v16_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v15 = v15;
  env46->v16 = v16;
  env46->v4 = ((Env_v14*)env14)->v4;
  env46->v6 = ((Env_v14*)env14)->v6;
  env46->v7 = ((Env_v14*)env14)->v7;
  env46->v9 = ((Env_v14*)env14)->v9;
  env46->v10 = ((Env_v14*)env14)->v10;
  env46->v11 = ((Env_v14*)env14)->v11;
  env46->v12 = ((Env_v14*)env14)->v12;
  env46->v13 = ((Env_v14*)env14)->v13;
  if (((v15) == NULL)) {
    return v16;
  } else {
    return v46(env46, (void*)((v15)->head), (void*)((v15)->tail));
  }
}

bool v50(void* env50, void* v36_raw) {
  int v36 = *(int*)v36_raw;
  return ((((Env_v50*)env50)->v33 == v36) || (abs((((Env_v50*)env50)->v33 - v36)) == abs((((Env_v50*)env50)->v32 - ((Env_v50*)env50)->v35))));
}

bool v52(void* env52, void* v35_raw) {
  int v35 = *(int*)v35_raw;
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v35 = v35;
  env50->v4 = ((Env_v52*)env52)->v4;
  env50->v6 = ((Env_v52*)env52)->v6;
  env50->v7 = ((Env_v52*)env52)->v7;
  env50->v9 = ((Env_v52*)env52)->v9;
  env50->v10 = ((Env_v52*)env52)->v10;
  env50->v11 = ((Env_v52*)env52)->v11;
  env50->v12 = ((Env_v52*)env52)->v12;
  env50->v13 = ((Env_v52*)env52)->v13;
  env50->v20 = ((Env_v52*)env52)->v20;
  env50->v21 = ((Env_v52*)env52)->v21;
  env50->v22 = ((Env_v52*)env52)->v22;
  env50->v23 = ((Env_v52*)env52)->v23;
  env50->v24 = ((Env_v52*)env52)->v24;
  env50->v25 = ((Env_v52*)env52)->v25;
  env50->v27 = ((Env_v52*)env52)->v27;
  env50->v28 = ((Env_v52*)env52)->v28;
  env50->v29 = ((Env_v52*)env52)->v29;
  env50->v30 = ((Env_v52*)env52)->v30;
  env50->v31 = ((Env_v52*)env52)->v31;
  env50->v32 = ((Env_v52*)env52)->v32;
  env50->v33 = ((Env_v52*)env52)->v33;
  env50->v34 = ((Env_v52*)env52)->v34;
  return v50(env50, box_int((((Env_v52*)env52)->v34)->snd));
}

bool v54(void* env54, void* v34_raw) {
  Pair_Int_Int *v34 = (Pair_Int_Int*)v34_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v34 = v34;
  env52->v4 = ((Env_v54*)env54)->v4;
  env52->v6 = ((Env_v54*)env54)->v6;
  env52->v7 = ((Env_v54*)env54)->v7;
  env52->v9 = ((Env_v54*)env54)->v9;
  env52->v10 = ((Env_v54*)env54)->v10;
  env52->v11 = ((Env_v54*)env54)->v11;
  env52->v12 = ((Env_v54*)env54)->v12;
  env52->v13 = ((Env_v54*)env54)->v13;
  env52->v20 = ((Env_v54*)env54)->v20;
  env52->v21 = ((Env_v54*)env54)->v21;
  env52->v22 = ((Env_v54*)env54)->v22;
  env52->v23 = ((Env_v54*)env54)->v23;
  env52->v24 = ((Env_v54*)env54)->v24;
  env52->v25 = ((Env_v54*)env54)->v25;
  env52->v27 = ((Env_v54*)env54)->v27;
  env52->v28 = ((Env_v54*)env54)->v28;
  env52->v29 = ((Env_v54*)env54)->v29;
  env52->v30 = ((Env_v54*)env54)->v30;
  env52->v31 = ((Env_v54*)env54)->v31;
  env52->v32 = ((Env_v54*)env54)->v32;
  env52->v33 = ((Env_v54*)env54)->v33;
  return v52(env52, box_int((v34)->fst));
}

Closure* v55(void* env55, void* v33_raw) {
  int v33 = *(int*)v33_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v33 = v33;
  env54->v4 = ((Env_v55*)env55)->v4;
  env54->v6 = ((Env_v55*)env55)->v6;
  env54->v7 = ((Env_v55*)env55)->v7;
  env54->v9 = ((Env_v55*)env55)->v9;
  env54->v10 = ((Env_v55*)env55)->v10;
  env54->v11 = ((Env_v55*)env55)->v11;
  env54->v12 = ((Env_v55*)env55)->v12;
  env54->v13 = ((Env_v55*)env55)->v13;
  env54->v20 = ((Env_v55*)env55)->v20;
  env54->v21 = ((Env_v55*)env55)->v21;
  env54->v22 = ((Env_v55*)env55)->v22;
  env54->v23 = ((Env_v55*)env55)->v23;
  env54->v24 = ((Env_v55*)env55)->v24;
  env54->v25 = ((Env_v55*)env55)->v25;
  env54->v27 = ((Env_v55*)env55)->v27;
  env54->v28 = ((Env_v55*)env55)->v28;
  env54->v29 = ((Env_v55*)env55)->v29;
  env54->v30 = ((Env_v55*)env55)->v30;
  env54->v31 = ((Env_v55*)env55)->v31;
  env54->v32 = ((Env_v55*)env55)->v32;
  Closure* c54 = malloc(sizeof(Closure));
  c54->env = env54;
  c54->fn = (void* (*)(void*, void*))v54;
  return c54;
}

Closure* v57(void* env57, void* v32_raw) {
  int v32 = *(int*)v32_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v32 = v32;
  env55->v4 = ((Env_v57*)env57)->v4;
  env55->v6 = ((Env_v57*)env57)->v6;
  env55->v7 = ((Env_v57*)env57)->v7;
  env55->v9 = ((Env_v57*)env57)->v9;
  env55->v10 = ((Env_v57*)env57)->v10;
  env55->v11 = ((Env_v57*)env57)->v11;
  env55->v12 = ((Env_v57*)env57)->v12;
  env55->v13 = ((Env_v57*)env57)->v13;
  env55->v20 = ((Env_v57*)env57)->v20;
  env55->v21 = ((Env_v57*)env57)->v21;
  env55->v22 = ((Env_v57*)env57)->v22;
  env55->v23 = ((Env_v57*)env57)->v23;
  env55->v24 = ((Env_v57*)env57)->v24;
  env55->v25 = ((Env_v57*)env57)->v25;
  env55->v27 = ((Env_v57*)env57)->v27;
  env55->v28 = ((Env_v57*)env57)->v28;
  env55->v29 = ((Env_v57*)env57)->v29;
  env55->v30 = ((Env_v57*)env57)->v30;
  env55->v31 = ((Env_v57*)env57)->v31;
  Closure* c55 = v55(env55, box_int((((Env_v57*)env57)->v31)->snd));
  return c55;
}

Closure* v59(void* env59, void* v31_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  Env_v57* env57 = malloc(sizeof(Env_v57));
  env57->v31 = v31;
  env57->v4 = ((Env_v59*)env59)->v4;
  env57->v6 = ((Env_v59*)env59)->v6;
  env57->v7 = ((Env_v59*)env59)->v7;
  env57->v9 = ((Env_v59*)env59)->v9;
  env57->v10 = ((Env_v59*)env59)->v10;
  env57->v11 = ((Env_v59*)env59)->v11;
  env57->v12 = ((Env_v59*)env59)->v12;
  env57->v13 = ((Env_v59*)env59)->v13;
  env57->v20 = ((Env_v59*)env59)->v20;
  env57->v21 = ((Env_v59*)env59)->v21;
  env57->v22 = ((Env_v59*)env59)->v22;
  env57->v23 = ((Env_v59*)env59)->v23;
  env57->v24 = ((Env_v59*)env59)->v24;
  env57->v25 = ((Env_v59*)env59)->v25;
  env57->v27 = ((Env_v59*)env59)->v27;
  env57->v28 = ((Env_v59*)env59)->v28;
  env57->v29 = ((Env_v59*)env59)->v29;
  env57->v30 = ((Env_v59*)env59)->v30;
  Closure* c57 = v57(env57, box_int((v31)->fst));
  return c57;
}

bool v66(void* env66, void* v29_raw, void* v30_raw) {
  Pair_Int_Int *v29 = (Pair_Int_Int*)v29_raw;
  Node* v30 = (Node*)v30_raw;
  Env_v26* env26 = malloc(sizeof(Env_v26));
  env26->v4 = ((Env_v66*)env66)->v4;
  env26->v6 = ((Env_v66*)env66)->v6;
  env26->v7 = ((Env_v66*)env66)->v7;
  env26->v9 = ((Env_v66*)env66)->v9;
  env26->v10 = ((Env_v66*)env66)->v10;
  env26->v11 = ((Env_v66*)env66)->v11;
  env26->v12 = ((Env_v66*)env66)->v12;
  env26->v13 = ((Env_v66*)env66)->v13;
  env26->v20 = ((Env_v66*)env66)->v20;
  env26->v21 = ((Env_v66*)env66)->v21;
  env26->v22 = ((Env_v66*)env66)->v22;
  env26->v23 = ((Env_v66*)env66)->v23;
  env26->v24 = ((Env_v66*)env66)->v24;
  env26->v25 = ((Env_v66*)env66)->v25;
  Env_v59* env59 = malloc(sizeof(Env_v59));
  env59->v29 = v29;
  env59->v30 = v30;
  env59->v4 = ((Env_v66*)env66)->v4;
  env59->v6 = ((Env_v66*)env66)->v6;
  env59->v7 = ((Env_v66*)env66)->v7;
  env59->v9 = ((Env_v66*)env66)->v9;
  env59->v10 = ((Env_v66*)env66)->v10;
  env59->v11 = ((Env_v66*)env66)->v11;
  env59->v12 = ((Env_v66*)env66)->v12;
  env59->v13 = ((Env_v66*)env66)->v13;
  env59->v20 = ((Env_v66*)env66)->v20;
  env59->v21 = ((Env_v66*)env66)->v21;
  env59->v22 = ((Env_v66*)env66)->v22;
  env59->v23 = ((Env_v66*)env66)->v23;
  env59->v24 = ((Env_v66*)env66)->v24;
  env59->v25 = ((Env_v66*)env66)->v25;
  env59->v27 = ((Env_v66*)env66)->v27;
  env59->v28 = ((Env_v66*)env66)->v28;
  Closure* c59 = v59(env59, (void*)(((Env_v66*)env66)->v27));
  return (!((Closure*)(c59)->fn((c59)->env, v29)) && v26(env26, (void*)(((Env_v66*)env66)->v27), (void*)(v30)));
}

bool v26(void* env26, void* v27_raw, void* v28_raw) {
  Pair_Int_Int *v27 = (Pair_Int_Int*)v27_raw;
  Node* v28 = (Node*)v28_raw;
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v27 = v27;
  env66->v28 = v28;
  env66->v4 = ((Env_v26*)env26)->v4;
  env66->v6 = ((Env_v26*)env26)->v6;
  env66->v7 = ((Env_v26*)env26)->v7;
  env66->v9 = ((Env_v26*)env26)->v9;
  env66->v10 = ((Env_v26*)env26)->v10;
  env66->v11 = ((Env_v26*)env26)->v11;
  env66->v12 = ((Env_v26*)env26)->v12;
  env66->v13 = ((Env_v26*)env26)->v13;
  env66->v20 = ((Env_v26*)env26)->v20;
  env66->v21 = ((Env_v26*)env26)->v21;
  env66->v22 = ((Env_v26*)env26)->v22;
  env66->v23 = ((Env_v26*)env26)->v23;
  env66->v24 = ((Env_v26*)env26)->v24;
  env66->v25 = ((Env_v26*)env26)->v25;
  if (((v28) == NULL)) {
    return true;
  } else {
    return v66(env66, (void*)((v28)->head), (void*)((v28)->tail));
  }
}

Node* v72(void* env72, void* v25_raw) {
  Node* v25 = (Node*)v25_raw;
  Env_v26* env26 = malloc(sizeof(Env_v26));
  env26->v25 = v25;
  env26->v4 = ((Env_v72*)env72)->v4;
  env26->v6 = ((Env_v72*)env72)->v6;
  env26->v7 = ((Env_v72*)env72)->v7;
  env26->v9 = ((Env_v72*)env72)->v9;
  env26->v10 = ((Env_v72*)env72)->v10;
  env26->v11 = ((Env_v72*)env72)->v11;
  env26->v12 = ((Env_v72*)env72)->v12;
  env26->v13 = ((Env_v72*)env72)->v13;
  env26->v20 = ((Env_v72*)env72)->v20;
  env26->v21 = ((Env_v72*)env72)->v21;
  env26->v22 = ((Env_v72*)env72)->v22;
  env26->v23 = ((Env_v72*)env72)->v23;
  env26->v24 = ((Env_v72*)env72)->v24;
  if (v26(env26, (void*)(((Env_v72*)env72)->v24), (void*)(((Env_v72*)env72)->v22))) {
    return cons(cons(((Env_v72*)env72)->v24, ((Env_v72*)env72)->v22), v25);
  } else {
    return v25;
  }
}

Node* v78(void* env78, void* v24_raw) {
  Pair_Int_Int *v24 = (Pair_Int_Int*)v24_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  env19->v4 = ((Env_v78*)env78)->v4;
  env19->v6 = ((Env_v78*)env78)->v6;
  env19->v7 = ((Env_v78*)env78)->v7;
  env19->v9 = ((Env_v78*)env78)->v9;
  env19->v10 = ((Env_v78*)env78)->v10;
  env19->v11 = ((Env_v78*)env78)->v11;
  env19->v12 = ((Env_v78*)env78)->v12;
  env19->v13 = ((Env_v78*)env78)->v13;
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v24 = v24;
  env72->v4 = ((Env_v78*)env78)->v4;
  env72->v6 = ((Env_v78*)env78)->v6;
  env72->v7 = ((Env_v78*)env78)->v7;
  env72->v9 = ((Env_v78*)env78)->v9;
  env72->v10 = ((Env_v78*)env78)->v10;
  env72->v11 = ((Env_v78*)env78)->v11;
  env72->v12 = ((Env_v78*)env78)->v12;
  env72->v13 = ((Env_v78*)env78)->v13;
  env72->v20 = ((Env_v78*)env78)->v20;
  env72->v21 = ((Env_v78*)env78)->v21;
  env72->v22 = ((Env_v78*)env78)->v22;
  env72->v23 = ((Env_v78*)env78)->v23;
  return v72(env72, (void*)(v19(env19, box_int(((Env_v78*)env78)->v20), box_int(((Env_v78*)env78)->v21), (void*)(((Env_v78*)env78)->v22), box_int((((Env_v78*)env78)->v23 + 1)))));
}

Node* v19(void* env19, void* v20_raw, void* v21_raw, void* v22_raw, void* v23_raw) {
  int v20 = *(int*)v20_raw;
  int v21 = *(int*)v21_raw;
  Node* v22 = (Node*)v22_raw;
  int v23 = *(int*)v23_raw;
  Env_v78* env78 = malloc(sizeof(Env_v78));
  env78->v20 = v20;
  env78->v21 = v21;
  env78->v22 = v22;
  env78->v23 = v23;
  env78->v4 = ((Env_v19*)env19)->v4;
  env78->v6 = ((Env_v19*)env19)->v6;
  env78->v7 = ((Env_v19*)env19)->v7;
  env78->v9 = ((Env_v19*)env19)->v9;
  env78->v10 = ((Env_v19*)env19)->v10;
  env78->v11 = ((Env_v19*)env19)->v11;
  env78->v12 = ((Env_v19*)env19)->v12;
  env78->v13 = ((Env_v19*)env19)->v13;
  if ((v23 == v20)) {
    return NULL;
  } else {
    return v78(env78, (void*)(makePair_Int_Int(v21, v23)));
  }
}

Node* v93(void* env93, void* v12_raw, void* v13_raw) {
  Node* v12 = (Node*)v12_raw;
  Node* v13 = (Node*)v13_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v4 = ((Env_v93*)env93)->v4;
  env8->v6 = ((Env_v93*)env93)->v6;
  env8->v7 = ((Env_v93*)env93)->v7;
  Env_v14* env14 = malloc(sizeof(Env_v14));
  env14->v12 = v12;
  env14->v13 = v13;
  env14->v4 = ((Env_v93*)env93)->v4;
  env14->v6 = ((Env_v93*)env93)->v6;
  env14->v7 = ((Env_v93*)env93)->v7;
  env14->v9 = ((Env_v93*)env93)->v9;
  env14->v10 = ((Env_v93*)env93)->v10;
  env14->v11 = ((Env_v93*)env93)->v11;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  env19->v12 = v12;
  env19->v13 = v13;
  env19->v4 = ((Env_v93*)env93)->v4;
  env19->v6 = ((Env_v93*)env93)->v6;
  env19->v7 = ((Env_v93*)env93)->v7;
  env19->v9 = ((Env_v93*)env93)->v9;
  env19->v10 = ((Env_v93*)env93)->v10;
  env19->v11 = ((Env_v93*)env93)->v11;
  return v14(env14, (void*)(v19(env19, box_int(((Env_v93*)env93)->v9), box_int(((Env_v93*)env93)->v10), (void*)(v12), box_int(0))), (void*)(v8(env8, box_int(((Env_v93*)env93)->v9), box_int(((Env_v93*)env93)->v10), (void*)(v13))));
}

Node* v8(void* env8, void* v9_raw, void* v10_raw, void* v11_raw) {
  int v9 = *(int*)v9_raw;
  int v10 = *(int*)v10_raw;
  Node* v11 = (Node*)v11_raw;
  Env_v93* env93 = malloc(sizeof(Env_v93));
  env93->v9 = v9;
  env93->v10 = v10;
  env93->v11 = v11;
  env93->v4 = ((Env_v8*)env8)->v4;
  env93->v6 = ((Env_v8*)env8)->v6;
  env93->v7 = ((Env_v8*)env8)->v7;
  if (((v11) == NULL)) {
    return NULL;
  } else {
    return v93(env93, (void*)((v11)->head), (void*)((v11)->tail));
  }
}

Node* v5(void* env5, void* v6_raw, void* v7_raw) {
  int v6 = *(int*)v6_raw;
  Node* v7 = (Node*)v7_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v6 = v6;
  env8->v7 = v7;
  env8->v4 = ((Env_v5*)env5)->v4;
  if ((v6 == ((Env_v5*)env5)->v4)) {
    return v7;
  } else {
    return v5(env5, box_int((v6 + 1)), (void*)(v8(env8, box_int(((Env_v5*)env5)->v4), box_int(v6), (void*)(v7))));
  }
}

Node* v104(int v4) {
  Env_v5* env5 = malloc(sizeof(Env_v5));
  env5->v4 = v4;
  return v5(env5, box_int(0), (void*)(cons(NULL, NULL)));
}

// main
int main(void) {
  printInt(v0(v104(4)));
  return 0;
}

