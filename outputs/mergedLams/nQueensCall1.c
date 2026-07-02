
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

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
int v0(List* v1);
List* v136(void* env136, void* v19_raw, void* v20_raw);
List* v16(void* env16, void* v17_raw, void* v18_raw);
bool v167(void* env167, void* v43_raw);
bool v169(void* env169, void* v40_raw);
Closure* v170(void* env170, void* v39_raw);
Closure* v172(void* env172, void* v36_raw);
bool v179(void* env179, void* v33_raw, void* v34_raw);
bool v30(void* env30, void* v31_raw, void* v32_raw);
List* v185(void* env185, void* v29_raw);
List* v191(void* env191, void* v27_raw);
List* v21(void* env21, void* v22_raw, void* v23_raw, void* v24_raw, void* v25_raw);
List* v206(void* env206, void* v14_raw, void* v15_raw);
List* v10(void* env10, void* v11_raw, void* v12_raw, void* v13_raw);
List* v7(void* env7, void* v8_raw, void* v9_raw);
List* v217(int v5);

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
    List* v18;
} Env_v136;

typedef struct {
    Pair_Int_Int *v36;
    int v39;
    Pair_Int_Int *v40;
} Env_v167;

typedef struct {
    Pair_Int_Int *v36;
    int v39;
} Env_v169;

typedef struct {
    Pair_Int_Int *v36;
} Env_v170;

typedef struct {
} Env_v172;

typedef struct {
    Pair_Int_Int *v31;
} Env_v179;

typedef struct {
    List* v24;
    Pair_Int_Int *v27;
} Env_v185;

typedef struct {
    int v22;
    int v23;
    List* v24;
    int v25;
} Env_v191;

typedef struct {
    int v11;
    int v12;
} Env_v206;

typedef struct {
} Env_v217;

// function implementations
int v46(void* env46, void* v2_raw, void* v3_raw) {
  List* v2 = (List*)v2_raw;
  List* v3 = (List*)v3_raw;
  return (1 + v0(v3));
}

int v0(List* v1) {
  Env_v46* env46 = malloc(sizeof(Env_v46));
  if (((v1) == NULL)) return 0;
  return v46(env46, (void*)((v1)->head), (void*)((v1)->tail));
}

List* v136(void* env136, void* v19_raw, void* v20_raw) {
  List* v19 = (List*)v19_raw;
  List* v20 = (List*)v20_raw;
  Env_v16* env16 = malloc(sizeof(Env_v16));
  return cons(v19, v16(env16, (void*)(v20), (void*)(((Env_v136*)env136)->v18)));
}

List* v16(void* env16, void* v17_raw, void* v18_raw) {
  List* v17 = (List*)v17_raw;
  List* v18 = (List*)v18_raw;
  Env_v136* env136 = malloc(sizeof(Env_v136));
  env136->v18 = v18;
  if (((v17) == NULL)) return v18;
  return v136(env136, (void*)((v17)->head), (void*)((v17)->tail));
}

bool v167(void* env167, void* v43_raw) {
  int v43 = *(int*)v43_raw;
  return ((((Env_v167*)env167)->v39 == v43) || (abs((((Env_v167*)env167)->v39 - v43)) == abs(((((Env_v167*)env167)->v36)->fst - (((Env_v167*)env167)->v40)->fst))));
}

bool v169(void* env169, void* v40_raw) {
  Pair_Int_Int *v40 = (Pair_Int_Int*)v40_raw;
  Env_v167* env167 = malloc(sizeof(Env_v167));
  env167->v36 = ((Env_v169*)env169)->v36;
  env167->v39 = ((Env_v169*)env169)->v39;
  env167->v40 = v40;
  return v167(env167, box_int((v40)->snd));
}

Closure* v170(void* env170, void* v39_raw) {
  int v39 = *(int*)v39_raw;
  Env_v169* env169 = malloc(sizeof(Env_v169));
  env169->v36 = ((Env_v170*)env170)->v36;
  env169->v39 = v39;
  Closure* c169 = malloc(sizeof(Closure));
  c169->env = env169;
  c169->fn = (void* (*)(void*, void*))v169;
  return c169;
}

Closure* v172(void* env172, void* v36_raw) {
  Pair_Int_Int *v36 = (Pair_Int_Int*)v36_raw;
  Env_v170* env170 = malloc(sizeof(Env_v170));
  env170->v36 = v36;
  Closure* c170 = v170(env170, box_int((v36)->snd));
  return c170;
}

bool v179(void* env179, void* v33_raw, void* v34_raw) {
  Pair_Int_Int *v33 = (Pair_Int_Int*)v33_raw;
  List* v34 = (List*)v34_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Env_v172* env172 = malloc(sizeof(Env_v172));
  Closure* c172 = v172(env172, (void*)(((Env_v179*)env179)->v31));
  return (!((Closure*)(c172)->fn((c172)->env, v33)) && v30(env30, (void*)(((Env_v179*)env179)->v31), (void*)(v34)));
}

bool v30(void* env30, void* v31_raw, void* v32_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  List* v32 = (List*)v32_raw;
  Env_v179* env179 = malloc(sizeof(Env_v179));
  env179->v31 = v31;
  if (((v32) == NULL)) return true;
  return v179(env179, (void*)((v32)->head), (void*)((v32)->tail));
}

List* v185(void* env185, void* v29_raw) {
  List* v29 = (List*)v29_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  return ((v30(env30, (void*)(((Env_v185*)env185)->v27), (void*)(((Env_v185*)env185)->v24))) ? (cons(cons(((Env_v185*)env185)->v27, ((Env_v185*)env185)->v24), v29)) : (v29));
}

List* v191(void* env191, void* v27_raw) {
  Pair_Int_Int *v27 = (Pair_Int_Int*)v27_raw;
  Env_v21* env21 = malloc(sizeof(Env_v21));
  Env_v185* env185 = malloc(sizeof(Env_v185));
  env185->v24 = ((Env_v191*)env191)->v24;
  env185->v27 = v27;
  return v185(env185, (void*)(v21(env21, box_int(((Env_v191*)env191)->v22), box_int(((Env_v191*)env191)->v23), (void*)(((Env_v191*)env191)->v24), box_int((((Env_v191*)env191)->v25 + 1)))));
}

List* v21(void* env21, void* v22_raw, void* v23_raw, void* v24_raw, void* v25_raw) {
  int v22 = *(int*)v22_raw;
  int v23 = *(int*)v23_raw;
  List* v24 = (List*)v24_raw;
  int v25 = *(int*)v25_raw;
  Env_v191* env191 = malloc(sizeof(Env_v191));
  env191->v22 = v22;
  env191->v23 = v23;
  env191->v24 = v24;
  env191->v25 = v25;
  if ((v25 == v22)) return NULL;
  return v191(env191, (void*)(makePair_Int_Int(v23, v25)));
}

List* v206(void* env206, void* v14_raw, void* v15_raw) {
  List* v14 = (List*)v14_raw;
  List* v15 = (List*)v15_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Env_v21* env21 = malloc(sizeof(Env_v21));
  return v16(env16, (void*)(v21(env21, box_int(((Env_v206*)env206)->v11), box_int(((Env_v206*)env206)->v12), (void*)(v14), box_int(0))), (void*)(v10(env10, box_int(((Env_v206*)env206)->v11), box_int(((Env_v206*)env206)->v12), (void*)(v15))));
}

List* v10(void* env10, void* v11_raw, void* v12_raw, void* v13_raw) {
  int v11 = *(int*)v11_raw;
  int v12 = *(int*)v12_raw;
  List* v13 = (List*)v13_raw;
  Env_v206* env206 = malloc(sizeof(Env_v206));
  env206->v11 = v11;
  env206->v12 = v12;
  if (((v13) == NULL)) return NULL;
  return v206(env206, (void*)((v13)->head), (void*)((v13)->tail));
}

List* v7(void* env7, void* v8_raw, void* v9_raw) {
  int v8 = *(int*)v8_raw;
  List* v9 = (List*)v9_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  if ((v8 == ((Env_v7*)env7)->v5)) return v9;
  return v7(env7, box_int((v8 + 1)), (void*)(v10(env10, box_int(((Env_v7*)env7)->v5), box_int(v8), (void*)(v9))));
}

List* v217(int v5) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = v5;
  return v7(env7, box_int(0), (void*)(cons(NULL, NULL)));
}

// main
int main(void) {
  printInt(v0(v217(4)));
  return 0;
}

