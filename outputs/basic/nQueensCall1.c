
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
int v45(void* env45, void* v3_raw);
Closure* v46(void* env46, void* v2_raw);
int v0(Node* v1);
Node* v52(void* env52, void* v20_raw);
Closure* v53(void* env53, void* v19_raw);
Node* v55(void* env55, void* v18_raw);
Closure* v16(void* env16, void* v17_raw);
bool v57(void* env57, void* v43_raw);
bool v59(void* env59, void* v40_raw);
Closure* v60(void* env60, void* v39_raw);
Closure* v62(void* env62, void* v36_raw);
bool v68(void* env68, void* v34_raw);
Closure* v69(void* env69, void* v33_raw);
bool v71(void* env71, void* v32_raw);
Closure* v30(void* env30, void* v31_raw);
Node* v75(void* env75, void* v29_raw);
Node* v81(void* env81, void* v27_raw);
Node* v83(void* env83, void* v25_raw);
Closure* v84(void* env84, void* v24_raw);
Closure* v85(void* env85, void* v23_raw);
Closure* v21(void* env21, void* v22_raw);
Node* v95(void* env95, void* v15_raw);
Closure* v96(void* env96, void* v14_raw);
Node* v98(void* env98, void* v13_raw);
Closure* v99(void* env99, void* v12_raw);
Closure* v10(void* env10, void* v11_raw);
Node* v104(void* env104, void* v9_raw);
Closure* v7(void* env7, void* v8_raw);
Node* v107(int v5);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int v5;
} Env_v7;

typedef struct {
} Env_v10;

typedef struct {
} Env_v16;

typedef struct {
} Env_v21;

typedef struct {
} Env_v30;

typedef struct {
} Env_v45;

typedef struct {
} Env_v46;

typedef struct {
    Node* v18;
    Node* v19;
} Env_v52;

typedef struct {
    Node* v18;
} Env_v53;

typedef struct {
    Node* v17;
} Env_v55;

typedef struct {
    Pair_Int_Int *v36;
    int v39;
    Pair_Int_Int *v40;
} Env_v57;

typedef struct {
    Pair_Int_Int *v36;
    int v39;
} Env_v59;

typedef struct {
    Pair_Int_Int *v36;
} Env_v60;

typedef struct {
} Env_v62;

typedef struct {
    Pair_Int_Int *v31;
    Pair_Int_Int *v33;
} Env_v68;

typedef struct {
    Pair_Int_Int *v31;
} Env_v69;

typedef struct {
    Pair_Int_Int *v31;
} Env_v71;

typedef struct {
    Node* v24;
    Pair_Int_Int *v27;
} Env_v75;

typedef struct {
    int v22;
    int v23;
    Node* v24;
    int v25;
} Env_v81;

typedef struct {
    int v22;
    int v23;
    Node* v24;
} Env_v83;

typedef struct {
    int v22;
    int v23;
} Env_v84;

typedef struct {
    int v22;
} Env_v85;

typedef struct {
    int v11;
    int v12;
    Node* v14;
} Env_v95;

typedef struct {
    int v11;
    int v12;
} Env_v96;

typedef struct {
    int v11;
    int v12;
} Env_v98;

typedef struct {
    int v11;
} Env_v99;

typedef struct {
    int v5;
    int v8;
} Env_v104;

typedef struct {
} Env_v107;

// function implementations
int v45(void* env45, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

Closure* v46(void* env46, void* v2_raw) {
  Node* v2 = (Node*)v2_raw;
  Env_v45* env45 = malloc(sizeof(Env_v45));
  Closure* c45 = malloc(sizeof(Closure));
  c45->env = env45;
  c45->fn = (void* (*)(void*, void*))v45;
  return c45;
}

int v0(Node* v1) {
  Env_v46* env46 = malloc(sizeof(Env_v46));
  if (((v1) == NULL)) return 0;
  Closure* c46 = v46(env46, (void*)((v1)->head));
  return (int)(intptr_t)(c46)->fn((c46)->env, (v1)->tail);
}

Node* v52(void* env52, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Closure* c16 = v16(env16, (void*)(v20));
  return cons(((Env_v52*)env52)->v19, (Node*)(c16)->fn((c16)->env, ((Env_v52*)env52)->v18));
}

Closure* v53(void* env53, void* v19_raw) {
  Node* v19 = (Node*)v19_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v19 = v19;
  env52->v18 = ((Env_v53*)env53)->v18;
  Closure* c52 = malloc(sizeof(Closure));
  c52->env = env52;
  c52->fn = (void* (*)(void*, void*))v52;
  return c52;
}

Node* v55(void* env55, void* v18_raw) {
  Node* v18 = (Node*)v18_raw;
  Env_v53* env53 = malloc(sizeof(Env_v53));
  env53->v18 = v18;
  if (((((Env_v55*)env55)->v17) == NULL)) return v18;
  Closure* c53 = v53(env53, (void*)((((Env_v55*)env55)->v17)->head));
  return (Node*)(c53)->fn((c53)->env, (((Env_v55*)env55)->v17)->tail);
}

Closure* v16(void* env16, void* v17_raw) {
  Node* v17 = (Node*)v17_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v17 = v17;
  Closure* c55 = malloc(sizeof(Closure));
  c55->env = env55;
  c55->fn = (void* (*)(void*, void*))v55;
  return c55;
}

bool v57(void* env57, void* v43_raw) {
  int v43 = *(int*)v43_raw;
  return ((((Env_v57*)env57)->v39 == v43) || (abs((((Env_v57*)env57)->v39 - v43)) == abs(((((Env_v57*)env57)->v36)->fst - (((Env_v57*)env57)->v40)->fst))));
}

bool v59(void* env59, void* v40_raw) {
  Pair_Int_Int *v40 = (Pair_Int_Int*)v40_raw;
  Env_v57* env57 = malloc(sizeof(Env_v57));
  env57->v40 = v40;
  env57->v36 = ((Env_v59*)env59)->v36;
  env57->v39 = ((Env_v59*)env59)->v39;
  return v57(env57, box_int((v40)->snd));
}

Closure* v60(void* env60, void* v39_raw) {
  int v39 = *(int*)v39_raw;
  Env_v59* env59 = malloc(sizeof(Env_v59));
  env59->v39 = v39;
  env59->v36 = ((Env_v60*)env60)->v36;
  Closure* c59 = malloc(sizeof(Closure));
  c59->env = env59;
  c59->fn = (void* (*)(void*, void*))v59;
  return c59;
}

Closure* v62(void* env62, void* v36_raw) {
  Pair_Int_Int *v36 = (Pair_Int_Int*)v36_raw;
  Env_v60* env60 = malloc(sizeof(Env_v60));
  env60->v36 = v36;
  Closure* c60 = v60(env60, box_int((v36)->snd));
  return c60;
}

bool v68(void* env68, void* v34_raw) {
  Node* v34 = (Node*)v34_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Env_v62* env62 = malloc(sizeof(Env_v62));
  Closure* c30 = v30(env30, (void*)(((Env_v68*)env68)->v31));
  Closure* c62 = v62(env62, (void*)(((Env_v68*)env68)->v31));
  return (!((Closure*)(c62)->fn((c62)->env, ((Env_v68*)env68)->v33)) && (bool)(intptr_t)(c30)->fn((c30)->env, v34));
}

Closure* v69(void* env69, void* v33_raw) {
  Pair_Int_Int *v33 = (Pair_Int_Int*)v33_raw;
  Env_v68* env68 = malloc(sizeof(Env_v68));
  env68->v33 = v33;
  env68->v31 = ((Env_v69*)env69)->v31;
  Closure* c68 = malloc(sizeof(Closure));
  c68->env = env68;
  c68->fn = (void* (*)(void*, void*))v68;
  return c68;
}

bool v71(void* env71, void* v32_raw) {
  Node* v32 = (Node*)v32_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v31 = ((Env_v71*)env71)->v31;
  if (((v32) == NULL)) return true;
  Closure* c69 = v69(env69, (void*)((v32)->head));
  return (bool)(intptr_t)(c69)->fn((c69)->env, (v32)->tail);
}

Closure* v30(void* env30, void* v31_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  Env_v71* env71 = malloc(sizeof(Env_v71));
  env71->v31 = v31;
  Closure* c71 = malloc(sizeof(Closure));
  c71->env = env71;
  c71->fn = (void* (*)(void*, void*))v71;
  return c71;
}

Node* v75(void* env75, void* v29_raw) {
  Node* v29 = (Node*)v29_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Closure* c30 = v30(env30, (void*)(((Env_v75*)env75)->v27));
  if ((bool)(intptr_t)(c30)->fn((c30)->env, ((Env_v75*)env75)->v24)) {
    return cons(cons(((Env_v75*)env75)->v27, ((Env_v75*)env75)->v24), v29);
  } else {
    return v29;
  }
}

Node* v81(void* env81, void* v27_raw) {
  Pair_Int_Int *v27 = (Pair_Int_Int*)v27_raw;
  Env_v21* env21 = malloc(sizeof(Env_v21));
  Env_v75* env75 = malloc(sizeof(Env_v75));
  env75->v27 = v27;
  env75->v24 = ((Env_v81*)env81)->v24;
  Closure* c21 = v21(env21, box_int(((Env_v81*)env81)->v22));
  Closure* c110 = (c21)->fn((c21)->env, box_int(((Env_v81*)env81)->v23));
  Closure* c111 = (c110)->fn((c110)->env, ((Env_v81*)env81)->v24);
  return v75(env75, (void*)((Node*)(c111)->fn((c111)->env, box_int((((Env_v81*)env81)->v25 + 1)))));
}

Node* v83(void* env83, void* v25_raw) {
  int v25 = *(int*)v25_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v25 = v25;
  env81->v22 = ((Env_v83*)env83)->v22;
  env81->v23 = ((Env_v83*)env83)->v23;
  env81->v24 = ((Env_v83*)env83)->v24;
  if ((v25 == ((Env_v83*)env83)->v22)) return NULL;
  return v81(env81, (void*)(makePair_Int_Int(((Env_v83*)env83)->v23, v25)));
}

Closure* v84(void* env84, void* v24_raw) {
  Node* v24 = (Node*)v24_raw;
  Env_v83* env83 = malloc(sizeof(Env_v83));
  env83->v24 = v24;
  env83->v22 = ((Env_v84*)env84)->v22;
  env83->v23 = ((Env_v84*)env84)->v23;
  Closure* c83 = malloc(sizeof(Closure));
  c83->env = env83;
  c83->fn = (void* (*)(void*, void*))v83;
  return c83;
}

Closure* v85(void* env85, void* v23_raw) {
  int v23 = *(int*)v23_raw;
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v23 = v23;
  env84->v22 = ((Env_v85*)env85)->v22;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

Closure* v21(void* env21, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v85* env85 = malloc(sizeof(Env_v85));
  env85->v22 = v22;
  Closure* c85 = malloc(sizeof(Closure));
  c85->env = env85;
  c85->fn = (void* (*)(void*, void*))v85;
  return c85;
}

Node* v95(void* env95, void* v15_raw) {
  Node* v15 = (Node*)v15_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Env_v21* env21 = malloc(sizeof(Env_v21));
  Closure* c21 = v21(env21, box_int(((Env_v95*)env95)->v11));
  Closure* c112 = (c21)->fn((c21)->env, box_int(((Env_v95*)env95)->v12));
  Closure* c113 = (c112)->fn((c112)->env, ((Env_v95*)env95)->v14);
  Closure* c10 = v10(env10, box_int(((Env_v95*)env95)->v11));
  Closure* c114 = (c10)->fn((c10)->env, box_int(((Env_v95*)env95)->v12));
  Closure* c16 = v16(env16, (void*)((Node*)(c113)->fn((c113)->env, box_int(0))));
  return (Node*)(c16)->fn((c16)->env, (Node*)(c114)->fn((c114)->env, v15));
}

Closure* v96(void* env96, void* v14_raw) {
  Node* v14 = (Node*)v14_raw;
  Env_v95* env95 = malloc(sizeof(Env_v95));
  env95->v14 = v14;
  env95->v11 = ((Env_v96*)env96)->v11;
  env95->v12 = ((Env_v96*)env96)->v12;
  Closure* c95 = malloc(sizeof(Closure));
  c95->env = env95;
  c95->fn = (void* (*)(void*, void*))v95;
  return c95;
}

Node* v98(void* env98, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  Env_v96* env96 = malloc(sizeof(Env_v96));
  env96->v11 = ((Env_v98*)env98)->v11;
  env96->v12 = ((Env_v98*)env98)->v12;
  if (((v13) == NULL)) return NULL;
  Closure* c96 = v96(env96, (void*)((v13)->head));
  return (Node*)(c96)->fn((c96)->env, (v13)->tail);
}

Closure* v99(void* env99, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v98* env98 = malloc(sizeof(Env_v98));
  env98->v12 = v12;
  env98->v11 = ((Env_v99*)env99)->v11;
  Closure* c98 = malloc(sizeof(Closure));
  c98->env = env98;
  c98->fn = (void* (*)(void*, void*))v98;
  return c98;
}

Closure* v10(void* env10, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v99* env99 = malloc(sizeof(Env_v99));
  env99->v11 = v11;
  Closure* c99 = malloc(sizeof(Closure));
  c99->env = env99;
  c99->fn = (void* (*)(void*, void*))v99;
  return c99;
}

Node* v104(void* env104, void* v9_raw) {
  Node* v9 = (Node*)v9_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = ((Env_v104*)env104)->v5;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  if ((((Env_v104*)env104)->v8 == ((Env_v104*)env104)->v5)) return v9;
  Closure* c10 = v10(env10, box_int(((Env_v104*)env104)->v5));
  Closure* c115 = (c10)->fn((c10)->env, box_int(((Env_v104*)env104)->v8));
  Closure* c7 = v7(env7, box_int((((Env_v104*)env104)->v8 + 1)));
  return (Node*)(c7)->fn((c7)->env, (Node*)(c115)->fn((c115)->env, v9));
}

Closure* v7(void* env7, void* v8_raw) {
  int v8 = *(int*)v8_raw;
  Env_v104* env104 = malloc(sizeof(Env_v104));
  env104->v8 = v8;
  env104->v5 = ((Env_v7*)env7)->v5;
  Closure* c104 = malloc(sizeof(Closure));
  c104->env = env104;
  c104->fn = (void* (*)(void*, void*))v104;
  return c104;
}

Node* v107(int v5) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = v5;
  Closure* c7 = v7(env7, box_int(0));
  return (Node*)(c7)->fn((c7)->env, cons(NULL, NULL));
}

// main
int main(void) {
  printInt(v0(v107(4)));
  return 0;
}

