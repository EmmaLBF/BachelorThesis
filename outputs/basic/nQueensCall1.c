
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
int v45(void* env45, void* v3_raw);
Closure* v46(void* env46, void* v2_raw);
int v0(List* v1);
List* v135(void* env135, void* v20_raw);
Closure* v136(void* env136, void* v19_raw);
List* v138(void* env138, void* v18_raw);
Closure* v16(void* env16, void* v17_raw);
bool v167(void* env167, void* v43_raw);
bool v169(void* env169, void* v40_raw);
Closure* v170(void* env170, void* v39_raw);
Closure* v172(void* env172, void* v36_raw);
bool v178(void* env178, void* v34_raw);
Closure* v179(void* env179, void* v33_raw);
bool v181(void* env181, void* v32_raw);
Closure* v30(void* env30, void* v31_raw);
List* v185(void* env185, void* v29_raw);
List* v191(void* env191, void* v27_raw);
List* v193(void* env193, void* v25_raw);
Closure* v194(void* env194, void* v24_raw);
Closure* v195(void* env195, void* v23_raw);
Closure* v21(void* env21, void* v22_raw);
List* v205(void* env205, void* v15_raw);
Closure* v206(void* env206, void* v14_raw);
List* v208(void* env208, void* v13_raw);
Closure* v209(void* env209, void* v12_raw);
Closure* v10(void* env10, void* v11_raw);
List* v214(void* env214, void* v9_raw);
Closure* v7(void* env7, void* v8_raw);
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
} Env_v45;

typedef struct {
} Env_v46;

typedef struct {
    List* v18;
    List* v19;
} Env_v135;

typedef struct {
    List* v18;
} Env_v136;

typedef struct {
    List* v17;
} Env_v138;

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
    Pair_Int_Int *v33;
} Env_v178;

typedef struct {
    Pair_Int_Int *v31;
} Env_v179;

typedef struct {
    Pair_Int_Int *v31;
} Env_v181;

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
    int v22;
    int v23;
    List* v24;
} Env_v193;

typedef struct {
    int v22;
    int v23;
} Env_v194;

typedef struct {
    int v22;
} Env_v195;

typedef struct {
    int v11;
    int v12;
    List* v14;
} Env_v205;

typedef struct {
    int v11;
    int v12;
} Env_v206;

typedef struct {
    int v11;
    int v12;
} Env_v208;

typedef struct {
    int v11;
} Env_v209;

typedef struct {
    int v5;
    int v8;
} Env_v214;

typedef struct {
} Env_v217;

// function implementations
int v45(void* env45, void* v3_raw) {
  List* v3 = (List*)v3_raw;
  return (1 + v0(v3));
}

Closure* v46(void* env46, void* v2_raw) {
  List* v2 = (List*)v2_raw;
  Env_v45* env45 = malloc(sizeof(Env_v45));
  Closure* c45 = malloc(sizeof(Closure));
  c45->env = env45;
  c45->fn = (void* (*)(void*, void*))v45;
  return c45;
}

int v0(List* v1) {
  Env_v46* env46 = malloc(sizeof(Env_v46));
  if (((v1) == NULL)) return 0;
  Closure* c46 = v46(env46, (void*)((v1)->head));
  return (int)(intptr_t)(c46)->fn((c46)->env, (v1)->tail);
}

List* v135(void* env135, void* v20_raw) {
  List* v20 = (List*)v20_raw;
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Closure* c16 = v16(env16, (void*)(v20));
  return cons(((Env_v135*)env135)->v19, (List*)(c16)->fn((c16)->env, ((Env_v135*)env135)->v18));
}

Closure* v136(void* env136, void* v19_raw) {
  List* v19 = (List*)v19_raw;
  Env_v135* env135 = malloc(sizeof(Env_v135));
  env135->v18 = ((Env_v136*)env136)->v18;
  env135->v19 = v19;
  Closure* c135 = malloc(sizeof(Closure));
  c135->env = env135;
  c135->fn = (void* (*)(void*, void*))v135;
  return c135;
}

List* v138(void* env138, void* v18_raw) {
  List* v18 = (List*)v18_raw;
  Env_v136* env136 = malloc(sizeof(Env_v136));
  env136->v18 = v18;
  if (((((Env_v138*)env138)->v17) == NULL)) return v18;
  Closure* c136 = v136(env136, (void*)((((Env_v138*)env138)->v17)->head));
  return (List*)(c136)->fn((c136)->env, (((Env_v138*)env138)->v17)->tail);
}

Closure* v16(void* env16, void* v17_raw) {
  List* v17 = (List*)v17_raw;
  Env_v138* env138 = malloc(sizeof(Env_v138));
  env138->v17 = v17;
  Closure* c138 = malloc(sizeof(Closure));
  c138->env = env138;
  c138->fn = (void* (*)(void*, void*))v138;
  return c138;
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

bool v178(void* env178, void* v34_raw) {
  List* v34 = (List*)v34_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Env_v172* env172 = malloc(sizeof(Env_v172));
  Closure* c30 = v30(env30, (void*)(((Env_v178*)env178)->v31));
  Closure* c172 = v172(env172, (void*)(((Env_v178*)env178)->v31));
  return (!((Closure*)(c172)->fn((c172)->env, ((Env_v178*)env178)->v33)) && (bool)(intptr_t)(c30)->fn((c30)->env, v34));
}

Closure* v179(void* env179, void* v33_raw) {
  Pair_Int_Int *v33 = (Pair_Int_Int*)v33_raw;
  Env_v178* env178 = malloc(sizeof(Env_v178));
  env178->v31 = ((Env_v179*)env179)->v31;
  env178->v33 = v33;
  Closure* c178 = malloc(sizeof(Closure));
  c178->env = env178;
  c178->fn = (void* (*)(void*, void*))v178;
  return c178;
}

bool v181(void* env181, void* v32_raw) {
  List* v32 = (List*)v32_raw;
  Env_v179* env179 = malloc(sizeof(Env_v179));
  env179->v31 = ((Env_v181*)env181)->v31;
  if (((v32) == NULL)) return true;
  Closure* c179 = v179(env179, (void*)((v32)->head));
  return (bool)(intptr_t)(c179)->fn((c179)->env, (v32)->tail);
}

Closure* v30(void* env30, void* v31_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  Env_v181* env181 = malloc(sizeof(Env_v181));
  env181->v31 = v31;
  Closure* c181 = malloc(sizeof(Closure));
  c181->env = env181;
  c181->fn = (void* (*)(void*, void*))v181;
  return c181;
}

List* v185(void* env185, void* v29_raw) {
  List* v29 = (List*)v29_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  Closure* c30 = v30(env30, (void*)(((Env_v185*)env185)->v27));
  return (((bool)(intptr_t)(c30)->fn((c30)->env, ((Env_v185*)env185)->v24)) ? (cons(cons(((Env_v185*)env185)->v27, ((Env_v185*)env185)->v24), v29)) : (v29));
}

List* v191(void* env191, void* v27_raw) {
  Pair_Int_Int *v27 = (Pair_Int_Int*)v27_raw;
  Env_v21* env21 = malloc(sizeof(Env_v21));
  Env_v185* env185 = malloc(sizeof(Env_v185));
  env185->v24 = ((Env_v191*)env191)->v24;
  env185->v27 = v27;
  Closure* c21 = v21(env21, box_int(((Env_v191*)env191)->v22));
  Closure* c220 = (c21)->fn((c21)->env, box_int(((Env_v191*)env191)->v23));
  Closure* c221 = (c220)->fn((c220)->env, ((Env_v191*)env191)->v24);
  return v185(env185, (void*)((List*)(c221)->fn((c221)->env, box_int((((Env_v191*)env191)->v25 + 1)))));
}

List* v193(void* env193, void* v25_raw) {
  int v25 = *(int*)v25_raw;
  Env_v191* env191 = malloc(sizeof(Env_v191));
  env191->v22 = ((Env_v193*)env193)->v22;
  env191->v23 = ((Env_v193*)env193)->v23;
  env191->v24 = ((Env_v193*)env193)->v24;
  env191->v25 = v25;
  if ((v25 == ((Env_v193*)env193)->v22)) return NULL;
  return v191(env191, (void*)(makePair_Int_Int(((Env_v193*)env193)->v23, v25)));
}

Closure* v194(void* env194, void* v24_raw) {
  List* v24 = (List*)v24_raw;
  Env_v193* env193 = malloc(sizeof(Env_v193));
  env193->v22 = ((Env_v194*)env194)->v22;
  env193->v23 = ((Env_v194*)env194)->v23;
  env193->v24 = v24;
  Closure* c193 = malloc(sizeof(Closure));
  c193->env = env193;
  c193->fn = (void* (*)(void*, void*))v193;
  return c193;
}

Closure* v195(void* env195, void* v23_raw) {
  int v23 = *(int*)v23_raw;
  Env_v194* env194 = malloc(sizeof(Env_v194));
  env194->v22 = ((Env_v195*)env195)->v22;
  env194->v23 = v23;
  Closure* c194 = malloc(sizeof(Closure));
  c194->env = env194;
  c194->fn = (void* (*)(void*, void*))v194;
  return c194;
}

Closure* v21(void* env21, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v195* env195 = malloc(sizeof(Env_v195));
  env195->v22 = v22;
  Closure* c195 = malloc(sizeof(Closure));
  c195->env = env195;
  c195->fn = (void* (*)(void*, void*))v195;
  return c195;
}

List* v205(void* env205, void* v15_raw) {
  List* v15 = (List*)v15_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  Env_v16* env16 = malloc(sizeof(Env_v16));
  Env_v21* env21 = malloc(sizeof(Env_v21));
  Closure* c21 = v21(env21, box_int(((Env_v205*)env205)->v11));
  Closure* c222 = (c21)->fn((c21)->env, box_int(((Env_v205*)env205)->v12));
  Closure* c223 = (c222)->fn((c222)->env, ((Env_v205*)env205)->v14);
  Closure* c10 = v10(env10, box_int(((Env_v205*)env205)->v11));
  Closure* c224 = (c10)->fn((c10)->env, box_int(((Env_v205*)env205)->v12));
  Closure* c16 = v16(env16, (void*)((List*)(c223)->fn((c223)->env, box_int(0))));
  return (List*)(c16)->fn((c16)->env, (List*)(c224)->fn((c224)->env, v15));
}

Closure* v206(void* env206, void* v14_raw) {
  List* v14 = (List*)v14_raw;
  Env_v205* env205 = malloc(sizeof(Env_v205));
  env205->v11 = ((Env_v206*)env206)->v11;
  env205->v12 = ((Env_v206*)env206)->v12;
  env205->v14 = v14;
  Closure* c205 = malloc(sizeof(Closure));
  c205->env = env205;
  c205->fn = (void* (*)(void*, void*))v205;
  return c205;
}

List* v208(void* env208, void* v13_raw) {
  List* v13 = (List*)v13_raw;
  Env_v206* env206 = malloc(sizeof(Env_v206));
  env206->v11 = ((Env_v208*)env208)->v11;
  env206->v12 = ((Env_v208*)env208)->v12;
  if (((v13) == NULL)) return NULL;
  Closure* c206 = v206(env206, (void*)((v13)->head));
  return (List*)(c206)->fn((c206)->env, (v13)->tail);
}

Closure* v209(void* env209, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v208* env208 = malloc(sizeof(Env_v208));
  env208->v11 = ((Env_v209*)env209)->v11;
  env208->v12 = v12;
  Closure* c208 = malloc(sizeof(Closure));
  c208->env = env208;
  c208->fn = (void* (*)(void*, void*))v208;
  return c208;
}

Closure* v10(void* env10, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v209* env209 = malloc(sizeof(Env_v209));
  env209->v11 = v11;
  Closure* c209 = malloc(sizeof(Closure));
  c209->env = env209;
  c209->fn = (void* (*)(void*, void*))v209;
  return c209;
}

List* v214(void* env214, void* v9_raw) {
  List* v9 = (List*)v9_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = ((Env_v214*)env214)->v5;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  if ((((Env_v214*)env214)->v8 == ((Env_v214*)env214)->v5)) return v9;
  Closure* c10 = v10(env10, box_int(((Env_v214*)env214)->v5));
  Closure* c225 = (c10)->fn((c10)->env, box_int(((Env_v214*)env214)->v8));
  Closure* c7 = v7(env7, box_int((((Env_v214*)env214)->v8 + 1)));
  return (List*)(c7)->fn((c7)->env, (List*)(c225)->fn((c225)->env, v9));
}

Closure* v7(void* env7, void* v8_raw) {
  int v8 = *(int*)v8_raw;
  Env_v214* env214 = malloc(sizeof(Env_v214));
  env214->v5 = ((Env_v7*)env7)->v5;
  env214->v8 = v8;
  Closure* c214 = malloc(sizeof(Closure));
  c214->env = env214;
  c214->fn = (void* (*)(void*, void*))v214;
  return c214;
}

List* v217(int v5) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = v5;
  Closure* c7 = v7(env7, box_int(0));
  return (List*)(c7)->fn((c7)->env, cons(NULL, NULL));
}

// main
int main(void) {
  printInt(v0(v217(4)));
  return 0;
}

