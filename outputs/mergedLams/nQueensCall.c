
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
int v45(void* env45, void* v2_raw, void* v3_raw);
int v0(List* v1);
List* v135(void* env135, void* v18_raw, void* v19_raw);
List* v15(void* env15, void* v16_raw, void* v17_raw);
bool v166(void* env166, void* v42_raw);
bool v168(void* env168, void* v39_raw);
Closure* v169(void* env169, void* v38_raw);
Closure* v171(void* env171, void* v35_raw);
bool v178(void* env178, void* v32_raw, void* v33_raw);
bool v29(void* env29, void* v30_raw, void* v31_raw);
List* v184(void* env184, void* v28_raw);
List* v190(void* env190, void* v26_raw);
List* v20(void* env20, void* v21_raw, void* v22_raw, void* v23_raw, void* v24_raw);
List* v205(void* env205, void* v13_raw, void* v14_raw);
List* v9(void* env9, void* v10_raw, void* v11_raw, void* v12_raw);
List* v6(void* env6, void* v7_raw, void* v8_raw);
List* v216(int v5);

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
    List* v17;
} Env_v135;

typedef struct {
    Pair_Int_Int *v35;
    int v38;
    Pair_Int_Int *v39;
} Env_v166;

typedef struct {
    Pair_Int_Int *v35;
    int v38;
} Env_v168;

typedef struct {
    Pair_Int_Int *v35;
} Env_v169;

typedef struct {
} Env_v171;

typedef struct {
    Pair_Int_Int *v30;
} Env_v178;

typedef struct {
    List* v23;
    Pair_Int_Int *v26;
} Env_v184;

typedef struct {
    int v21;
    int v22;
    List* v23;
    int v24;
} Env_v190;

typedef struct {
    int v10;
    int v11;
} Env_v205;

typedef struct {
} Env_v216;

// function implementations
int v45(void* env45, void* v2_raw, void* v3_raw) {
  List* v2 = (List*)v2_raw;
  List* v3 = (List*)v3_raw;
  return (1 + v0(v3));
}

int v0(List* v1) {
  Env_v45* env45 = malloc(sizeof(Env_v45));
  if (((v1) == NULL)) return 0;
  return v45(env45, (void*)((v1)->head), (void*)((v1)->tail));
}

List* v135(void* env135, void* v18_raw, void* v19_raw) {
  List* v18 = (List*)v18_raw;
  List* v19 = (List*)v19_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  return cons(v18, v15(env15, (void*)(v19), (void*)(((Env_v135*)env135)->v17)));
}

List* v15(void* env15, void* v16_raw, void* v17_raw) {
  List* v16 = (List*)v16_raw;
  List* v17 = (List*)v17_raw;
  Env_v135* env135 = malloc(sizeof(Env_v135));
  env135->v17 = v17;
  if (((v16) == NULL)) return v17;
  return v135(env135, (void*)((v16)->head), (void*)((v16)->tail));
}

bool v166(void* env166, void* v42_raw) {
  int v42 = *(int*)v42_raw;
  return ((((Env_v166*)env166)->v38 == v42) || (abs((((Env_v166*)env166)->v38 - v42)) == abs(((((Env_v166*)env166)->v35)->fst - (((Env_v166*)env166)->v39)->fst))));
}

bool v168(void* env168, void* v39_raw) {
  Pair_Int_Int *v39 = (Pair_Int_Int*)v39_raw;
  Env_v166* env166 = malloc(sizeof(Env_v166));
  env166->v35 = ((Env_v168*)env168)->v35;
  env166->v38 = ((Env_v168*)env168)->v38;
  env166->v39 = v39;
  return v166(env166, box_int((v39)->snd));
}

Closure* v169(void* env169, void* v38_raw) {
  int v38 = *(int*)v38_raw;
  Env_v168* env168 = malloc(sizeof(Env_v168));
  env168->v35 = ((Env_v169*)env169)->v35;
  env168->v38 = v38;
  Closure* c168 = malloc(sizeof(Closure));
  c168->env = env168;
  c168->fn = (void* (*)(void*, void*))v168;
  return c168;
}

Closure* v171(void* env171, void* v35_raw) {
  Pair_Int_Int *v35 = (Pair_Int_Int*)v35_raw;
  Env_v169* env169 = malloc(sizeof(Env_v169));
  env169->v35 = v35;
  Closure* c169 = v169(env169, box_int((v35)->snd));
  return c169;
}

bool v178(void* env178, void* v32_raw, void* v33_raw) {
  Pair_Int_Int *v32 = (Pair_Int_Int*)v32_raw;
  List* v33 = (List*)v33_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Env_v171* env171 = malloc(sizeof(Env_v171));
  Closure* c171 = v171(env171, (void*)(((Env_v178*)env178)->v30));
  return (!((Closure*)(c171)->fn((c171)->env, v32)) && v29(env29, (void*)(((Env_v178*)env178)->v30), (void*)(v33)));
}

bool v29(void* env29, void* v30_raw, void* v31_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  List* v31 = (List*)v31_raw;
  Env_v178* env178 = malloc(sizeof(Env_v178));
  env178->v30 = v30;
  if (((v31) == NULL)) return true;
  return v178(env178, (void*)((v31)->head), (void*)((v31)->tail));
}

List* v184(void* env184, void* v28_raw) {
  List* v28 = (List*)v28_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  return ((v29(env29, (void*)(((Env_v184*)env184)->v26), (void*)(((Env_v184*)env184)->v23))) ? (cons(cons(((Env_v184*)env184)->v26, ((Env_v184*)env184)->v23), v28)) : (v28));
}

List* v190(void* env190, void* v26_raw) {
  Pair_Int_Int *v26 = (Pair_Int_Int*)v26_raw;
  Env_v20* env20 = malloc(sizeof(Env_v20));
  Env_v184* env184 = malloc(sizeof(Env_v184));
  env184->v23 = ((Env_v190*)env190)->v23;
  env184->v26 = v26;
  return v184(env184, (void*)(v20(env20, box_int(((Env_v190*)env190)->v21), box_int(((Env_v190*)env190)->v22), (void*)(((Env_v190*)env190)->v23), box_int((((Env_v190*)env190)->v24 + 1)))));
}

List* v20(void* env20, void* v21_raw, void* v22_raw, void* v23_raw, void* v24_raw) {
  int v21 = *(int*)v21_raw;
  int v22 = *(int*)v22_raw;
  List* v23 = (List*)v23_raw;
  int v24 = *(int*)v24_raw;
  Env_v190* env190 = malloc(sizeof(Env_v190));
  env190->v21 = v21;
  env190->v22 = v22;
  env190->v23 = v23;
  env190->v24 = v24;
  if ((v24 == v21)) return NULL;
  return v190(env190, (void*)(makePair_Int_Int(v22, v24)));
}

List* v205(void* env205, void* v13_raw, void* v14_raw) {
  List* v13 = (List*)v13_raw;
  List* v14 = (List*)v14_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Env_v20* env20 = malloc(sizeof(Env_v20));
  return v15(env15, (void*)(v20(env20, box_int(((Env_v205*)env205)->v10), box_int(((Env_v205*)env205)->v11), (void*)(v13), box_int(0))), (void*)(v9(env9, box_int(((Env_v205*)env205)->v10), box_int(((Env_v205*)env205)->v11), (void*)(v14))));
}

List* v9(void* env9, void* v10_raw, void* v11_raw, void* v12_raw) {
  int v10 = *(int*)v10_raw;
  int v11 = *(int*)v11_raw;
  List* v12 = (List*)v12_raw;
  Env_v205* env205 = malloc(sizeof(Env_v205));
  env205->v10 = v10;
  env205->v11 = v11;
  if (((v12) == NULL)) return NULL;
  return v205(env205, (void*)((v12)->head), (void*)((v12)->tail));
}

List* v6(void* env6, void* v7_raw, void* v8_raw) {
  int v7 = *(int*)v7_raw;
  List* v8 = (List*)v8_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  if ((v7 == ((Env_v6*)env6)->v5)) return v8;
  return v6(env6, box_int((v7 + 1)), (void*)(v9(env9, box_int(((Env_v6*)env6)->v5), box_int(v7), (void*)(v8))));
}

List* v216(int v5) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = v5;
  return v6(env6, box_int(0), (void*)(cons(NULL, NULL)));
}

// main
int main(void) {
  printInt(v0(v216(4)));
  return 0;
}

