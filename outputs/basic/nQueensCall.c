
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
int v44(void* env44, void* v3_raw);
Closure* v45(void* env45, void* v2_raw);
int v0(List* v1);
List* v134(void* env134, void* v19_raw);
Closure* v135(void* env135, void* v18_raw);
List* v137(void* env137, void* v17_raw);
Closure* v15(void* env15, void* v16_raw);
bool v166(void* env166, void* v42_raw);
bool v168(void* env168, void* v39_raw);
Closure* v169(void* env169, void* v38_raw);
Closure* v171(void* env171, void* v35_raw);
bool v177(void* env177, void* v33_raw);
Closure* v178(void* env178, void* v32_raw);
bool v180(void* env180, void* v31_raw);
Closure* v29(void* env29, void* v30_raw);
List* v184(void* env184, void* v28_raw);
List* v190(void* env190, void* v26_raw);
List* v192(void* env192, void* v24_raw);
Closure* v193(void* env193, void* v23_raw);
Closure* v194(void* env194, void* v22_raw);
Closure* v20(void* env20, void* v21_raw);
List* v204(void* env204, void* v14_raw);
Closure* v205(void* env205, void* v13_raw);
List* v207(void* env207, void* v12_raw);
Closure* v208(void* env208, void* v11_raw);
Closure* v9(void* env9, void* v10_raw);
List* v213(void* env213, void* v8_raw);
Closure* v6(void* env6, void* v7_raw);
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
} Env_v44;

typedef struct {
} Env_v45;

typedef struct {
    List* v17;
    List* v18;
} Env_v134;

typedef struct {
    List* v17;
} Env_v135;

typedef struct {
    List* v16;
} Env_v137;

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
    Pair_Int_Int *v32;
} Env_v177;

typedef struct {
    Pair_Int_Int *v30;
} Env_v178;

typedef struct {
    Pair_Int_Int *v30;
} Env_v180;

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
    int v21;
    int v22;
    List* v23;
} Env_v192;

typedef struct {
    int v21;
    int v22;
} Env_v193;

typedef struct {
    int v21;
} Env_v194;

typedef struct {
    int v10;
    int v11;
    List* v13;
} Env_v204;

typedef struct {
    int v10;
    int v11;
} Env_v205;

typedef struct {
    int v10;
    int v11;
} Env_v207;

typedef struct {
    int v10;
} Env_v208;

typedef struct {
    int v5;
    int v7;
} Env_v213;

typedef struct {
} Env_v216;

// function implementations
int v44(void* env44, void* v3_raw) {
  List* v3 = (List*)v3_raw;
  return (1 + v0(v3));
}

Closure* v45(void* env45, void* v2_raw) {
  List* v2 = (List*)v2_raw;
  Env_v44* env44 = malloc(sizeof(Env_v44));
  Closure* c44 = malloc(sizeof(Closure));
  c44->env = env44;
  c44->fn = (void* (*)(void*, void*))v44;
  return c44;
}

int v0(List* v1) {
  Env_v45* env45 = malloc(sizeof(Env_v45));
  if (((v1) == NULL)) return 0;
  Closure* c45 = v45(env45, (void*)((v1)->head));
  return (int)(intptr_t)(c45)->fn((c45)->env, (v1)->tail);
}

List* v134(void* env134, void* v19_raw) {
  List* v19 = (List*)v19_raw;
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Closure* c15 = v15(env15, (void*)(v19));
  return cons(((Env_v134*)env134)->v18, (List*)(c15)->fn((c15)->env, ((Env_v134*)env134)->v17));
}

Closure* v135(void* env135, void* v18_raw) {
  List* v18 = (List*)v18_raw;
  Env_v134* env134 = malloc(sizeof(Env_v134));
  env134->v17 = ((Env_v135*)env135)->v17;
  env134->v18 = v18;
  Closure* c134 = malloc(sizeof(Closure));
  c134->env = env134;
  c134->fn = (void* (*)(void*, void*))v134;
  return c134;
}

List* v137(void* env137, void* v17_raw) {
  List* v17 = (List*)v17_raw;
  Env_v135* env135 = malloc(sizeof(Env_v135));
  env135->v17 = v17;
  if (((((Env_v137*)env137)->v16) == NULL)) return v17;
  Closure* c135 = v135(env135, (void*)((((Env_v137*)env137)->v16)->head));
  return (List*)(c135)->fn((c135)->env, (((Env_v137*)env137)->v16)->tail);
}

Closure* v15(void* env15, void* v16_raw) {
  List* v16 = (List*)v16_raw;
  Env_v137* env137 = malloc(sizeof(Env_v137));
  env137->v16 = v16;
  Closure* c137 = malloc(sizeof(Closure));
  c137->env = env137;
  c137->fn = (void* (*)(void*, void*))v137;
  return c137;
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

bool v177(void* env177, void* v33_raw) {
  List* v33 = (List*)v33_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Env_v171* env171 = malloc(sizeof(Env_v171));
  Closure* c29 = v29(env29, (void*)(((Env_v177*)env177)->v30));
  Closure* c171 = v171(env171, (void*)(((Env_v177*)env177)->v30));
  return (!((Closure*)(c171)->fn((c171)->env, ((Env_v177*)env177)->v32)) && (bool)(intptr_t)(c29)->fn((c29)->env, v33));
}

Closure* v178(void* env178, void* v32_raw) {
  Pair_Int_Int *v32 = (Pair_Int_Int*)v32_raw;
  Env_v177* env177 = malloc(sizeof(Env_v177));
  env177->v30 = ((Env_v178*)env178)->v30;
  env177->v32 = v32;
  Closure* c177 = malloc(sizeof(Closure));
  c177->env = env177;
  c177->fn = (void* (*)(void*, void*))v177;
  return c177;
}

bool v180(void* env180, void* v31_raw) {
  List* v31 = (List*)v31_raw;
  Env_v178* env178 = malloc(sizeof(Env_v178));
  env178->v30 = ((Env_v180*)env180)->v30;
  if (((v31) == NULL)) return true;
  Closure* c178 = v178(env178, (void*)((v31)->head));
  return (bool)(intptr_t)(c178)->fn((c178)->env, (v31)->tail);
}

Closure* v29(void* env29, void* v30_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Env_v180* env180 = malloc(sizeof(Env_v180));
  env180->v30 = v30;
  Closure* c180 = malloc(sizeof(Closure));
  c180->env = env180;
  c180->fn = (void* (*)(void*, void*))v180;
  return c180;
}

List* v184(void* env184, void* v28_raw) {
  List* v28 = (List*)v28_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  Closure* c29 = v29(env29, (void*)(((Env_v184*)env184)->v26));
  return (((bool)(intptr_t)(c29)->fn((c29)->env, ((Env_v184*)env184)->v23)) ? (cons(cons(((Env_v184*)env184)->v26, ((Env_v184*)env184)->v23), v28)) : (v28));
}

List* v190(void* env190, void* v26_raw) {
  Pair_Int_Int *v26 = (Pair_Int_Int*)v26_raw;
  Env_v20* env20 = malloc(sizeof(Env_v20));
  Env_v184* env184 = malloc(sizeof(Env_v184));
  env184->v23 = ((Env_v190*)env190)->v23;
  env184->v26 = v26;
  Closure* c20 = v20(env20, box_int(((Env_v190*)env190)->v21));
  Closure* c219 = (c20)->fn((c20)->env, box_int(((Env_v190*)env190)->v22));
  Closure* c220 = (c219)->fn((c219)->env, ((Env_v190*)env190)->v23);
  return v184(env184, (void*)((List*)(c220)->fn((c220)->env, box_int((((Env_v190*)env190)->v24 + 1)))));
}

List* v192(void* env192, void* v24_raw) {
  int v24 = *(int*)v24_raw;
  Env_v190* env190 = malloc(sizeof(Env_v190));
  env190->v21 = ((Env_v192*)env192)->v21;
  env190->v22 = ((Env_v192*)env192)->v22;
  env190->v23 = ((Env_v192*)env192)->v23;
  env190->v24 = v24;
  if ((v24 == ((Env_v192*)env192)->v21)) return NULL;
  return v190(env190, (void*)(makePair_Int_Int(((Env_v192*)env192)->v22, v24)));
}

Closure* v193(void* env193, void* v23_raw) {
  List* v23 = (List*)v23_raw;
  Env_v192* env192 = malloc(sizeof(Env_v192));
  env192->v21 = ((Env_v193*)env193)->v21;
  env192->v22 = ((Env_v193*)env193)->v22;
  env192->v23 = v23;
  Closure* c192 = malloc(sizeof(Closure));
  c192->env = env192;
  c192->fn = (void* (*)(void*, void*))v192;
  return c192;
}

Closure* v194(void* env194, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v193* env193 = malloc(sizeof(Env_v193));
  env193->v21 = ((Env_v194*)env194)->v21;
  env193->v22 = v22;
  Closure* c193 = malloc(sizeof(Closure));
  c193->env = env193;
  c193->fn = (void* (*)(void*, void*))v193;
  return c193;
}

Closure* v20(void* env20, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v194* env194 = malloc(sizeof(Env_v194));
  env194->v21 = v21;
  Closure* c194 = malloc(sizeof(Closure));
  c194->env = env194;
  c194->fn = (void* (*)(void*, void*))v194;
  return c194;
}

List* v204(void* env204, void* v14_raw) {
  List* v14 = (List*)v14_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  Env_v15* env15 = malloc(sizeof(Env_v15));
  Env_v20* env20 = malloc(sizeof(Env_v20));
  Closure* c20 = v20(env20, box_int(((Env_v204*)env204)->v10));
  Closure* c221 = (c20)->fn((c20)->env, box_int(((Env_v204*)env204)->v11));
  Closure* c222 = (c221)->fn((c221)->env, ((Env_v204*)env204)->v13);
  Closure* c9 = v9(env9, box_int(((Env_v204*)env204)->v10));
  Closure* c223 = (c9)->fn((c9)->env, box_int(((Env_v204*)env204)->v11));
  Closure* c15 = v15(env15, (void*)((List*)(c222)->fn((c222)->env, box_int(0))));
  return (List*)(c15)->fn((c15)->env, (List*)(c223)->fn((c223)->env, v14));
}

Closure* v205(void* env205, void* v13_raw) {
  List* v13 = (List*)v13_raw;
  Env_v204* env204 = malloc(sizeof(Env_v204));
  env204->v10 = ((Env_v205*)env205)->v10;
  env204->v11 = ((Env_v205*)env205)->v11;
  env204->v13 = v13;
  Closure* c204 = malloc(sizeof(Closure));
  c204->env = env204;
  c204->fn = (void* (*)(void*, void*))v204;
  return c204;
}

List* v207(void* env207, void* v12_raw) {
  List* v12 = (List*)v12_raw;
  Env_v205* env205 = malloc(sizeof(Env_v205));
  env205->v10 = ((Env_v207*)env207)->v10;
  env205->v11 = ((Env_v207*)env207)->v11;
  if (((v12) == NULL)) return NULL;
  Closure* c205 = v205(env205, (void*)((v12)->head));
  return (List*)(c205)->fn((c205)->env, (v12)->tail);
}

Closure* v208(void* env208, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v207* env207 = malloc(sizeof(Env_v207));
  env207->v10 = ((Env_v208*)env208)->v10;
  env207->v11 = v11;
  Closure* c207 = malloc(sizeof(Closure));
  c207->env = env207;
  c207->fn = (void* (*)(void*, void*))v207;
  return c207;
}

Closure* v9(void* env9, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v208* env208 = malloc(sizeof(Env_v208));
  env208->v10 = v10;
  Closure* c208 = malloc(sizeof(Closure));
  c208->env = env208;
  c208->fn = (void* (*)(void*, void*))v208;
  return c208;
}

List* v213(void* env213, void* v8_raw) {
  List* v8 = (List*)v8_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = ((Env_v213*)env213)->v5;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  if ((((Env_v213*)env213)->v7 == ((Env_v213*)env213)->v5)) return v8;
  Closure* c9 = v9(env9, box_int(((Env_v213*)env213)->v5));
  Closure* c224 = (c9)->fn((c9)->env, box_int(((Env_v213*)env213)->v7));
  Closure* c6 = v6(env6, box_int((((Env_v213*)env213)->v7 + 1)));
  return (List*)(c6)->fn((c6)->env, (List*)(c224)->fn((c224)->env, v8));
}

Closure* v6(void* env6, void* v7_raw) {
  int v7 = *(int*)v7_raw;
  Env_v213* env213 = malloc(sizeof(Env_v213));
  env213->v5 = ((Env_v6*)env6)->v5;
  env213->v7 = v7;
  Closure* c213 = malloc(sizeof(Closure));
  c213->env = env213;
  c213->fn = (void* (*)(void*, void*))v213;
  return c213;
}

List* v216(int v5) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v5 = v5;
  Closure* c6 = v6(env6, box_int(0));
  return (List*)(c6)->fn((c6)->env, cons(NULL, NULL));
}

// main
int main(void) {
  printInt(v0(v216(4)));
  return 0;
}

