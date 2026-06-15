
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
int v46(void* env46, void* v2_raw, void* v3_raw);
int v0(Node* v1);
Node* v53(void* env53, void* v19_raw, void* v20_raw);
Node* v16(void* env16, void* v17_raw, void* v18_raw);
bool v57(void* env57, void* v43_raw);
bool v59(void* env59, void* v40_raw);
Closure* v60(void* env60, void* v39_raw);
Closure* v62(void* env62, void* v36_raw);
bool v69(void* env69, void* v33_raw, void* v34_raw);
bool v30(void* env30, void* v31_raw, void* v32_raw);
Node* v75(void* env75, void* v29_raw);
Node* v81(void* env81, void* v27_raw);
Node* v21(void* env21, void* v22_raw, void* v23_raw, void* v24_raw, void* v25_raw);
Node* v96(void* env96, void* v14_raw, void* v15_raw);
Node* v10(void* env10, void* v11_raw, void* v12_raw, void* v13_raw);
Node* v7(void* env7, void* v8_raw, void* v9_raw);
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
} Env_v46;

typedef struct {
    Node* v18;
} Env_v53;

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
} Env_v69;

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
    int v11;
    int v12;
} Env_v96;

typedef struct {
} Env_v107;

// function implementations
int v46(void* env46, void* v2_raw, void* v3_raw) {
  Node* v2 = (Node*)v2_raw;
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

int v0(Node* v1) {
  Env_v46* env46 = malloc(sizeof(Env_v46));
  if (((v1) == NULL)) return 0;
  return v46(env46, (void*)((v1)->head), (void*)((v1)->tail));
}

Node* v53(void* env53, void* v19_raw, void* v20_raw) {
  Node* v19 = (Node*)v19_raw;
  Node* v20 = (Node*)v20_raw;
  Env_v16* env16 = malloc(sizeof(Env_v16));
  return cons(v19, v16(env16, (void*)(v20), (void*)(((Env_v53*)env53)->v18)));
}

Node* v16(void* env16, void* v17_raw, void* v18_raw) {
  Node* v17 = (Node*)v17_raw;
  Node* v18 = (Node*)v18_raw;
  Env_v53* env53 = malloc(sizeof(Env_v53));
  env53->v18 = v18;
  if (((v17) == NULL)) return v18;
  return v53(env53, (void*)((v17)->head), (void*)((v17)->tail));
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

bool v69(void* env69, void* v33_raw, void* v34_raw) {
  Pair_Int_Int *v33 = (Pair_Int_Int*)v33_raw;
  Node* v34 = (Node*)v34_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Env_v62* env62 = malloc(sizeof(Env_v62));
  Closure* c62 = v62(env62, (void*)(((Env_v69*)env69)->v31));
  return (!((Closure*)(c62)->fn((c62)->env, v33)) && v30(env30, (void*)(((Env_v69*)env69)->v31), (void*)(v34)));
}

bool v30(void* env30, void* v31_raw, void* v32_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  Node* v32 = (Node*)v32_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v31 = v31;
  if (((v32) == NULL)) return true;
  return v69(env69, (void*)((v32)->head), (void*)((v32)->tail));
}

Node* v75(void* env75, void* v29_raw) {
  Node* v29 = (Node*)v29_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  if (v30(env30, (void*)(((Env_v75*)env75)->v27), (void*)(((Env_v75*)env75)->v24))) {
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
  return v75(env75, (void*)(v21(env21, box_int(((Env_v81*)env81)->v22), box_int(((Env_v81*)env81)->v23), (void*)(((Env_v81*)env81)->v24), box_int((((Env_v81*)env81)->v25 + 1)))));
}

Node* v21(void* env21, void* v22_raw, void* v23_raw, void* v24_raw, void* v25_raw) {
  int v22 = *(int*)v22_raw;
  int v23 = *(int*)v23_raw;
  Node* v24 = (Node*)v24_raw;
  int v25 = *(int*)v25_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v22 = v22;
  env81->v23 = v23;
  env81->v24 = v24;
  env81->v25 = v25;
  if ((v25 == v22)) return NULL;
  return v81(env81, (void*)(makePair_Int_Int(v23, v25)));
}

Node* v96(void* env96, void* v14_raw, void* v15_raw) {
  Node* v14 = (Node*)v14_raw;
  Node* v15 = (Node*)v15_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Env_v21* env21 = malloc(sizeof(Env_v21));
  return v16(env16, (void*)(v21(env21, box_int(((Env_v96*)env96)->v11), box_int(((Env_v96*)env96)->v12), (void*)(v14), box_int(0))), (void*)(v10(env10, box_int(((Env_v96*)env96)->v11), box_int(((Env_v96*)env96)->v12), (void*)(v15))));
}

Node* v10(void* env10, void* v11_raw, void* v12_raw, void* v13_raw) {
  int v11 = *(int*)v11_raw;
  int v12 = *(int*)v12_raw;
  Node* v13 = (Node*)v13_raw;
  Env_v96* env96 = malloc(sizeof(Env_v96));
  env96->v11 = v11;
  env96->v12 = v12;
  if (((v13) == NULL)) return NULL;
  return v96(env96, (void*)((v13)->head), (void*)((v13)->tail));
}

Node* v7(void* env7, void* v8_raw, void* v9_raw) {
  int v8 = *(int*)v8_raw;
  Node* v9 = (Node*)v9_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  if ((v8 == ((Env_v7*)env7)->v5)) return v9;
  return v7(env7, box_int((v8 + 1)), (void*)(v10(env10, box_int(((Env_v7*)env7)->v5), box_int(v8), (void*)(v9))));
}

Node* v107(int v5) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = v5;
  return v7(env7, box_int(0), (void*)(cons(NULL, NULL)));
}

// main
int main(void) {
  printInt(v0(v107(4)));
  return 0;
}

