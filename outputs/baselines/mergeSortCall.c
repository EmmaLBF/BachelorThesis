
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
typedef struct Pair_Int_NodeInt {
  int fst;
  NodeInt* snd;
} Pair_Int_NodeInt;

Pair_Int_NodeInt* makePair_Int_NodeInt(int fst, NodeInt* snd) {
  Pair_Int_NodeInt* p = malloc(sizeof(Pair_Int_NodeInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

typedef struct Pair_NodeInt_NodeInt {
  NodeInt* fst;
  NodeInt* snd;
} Pair_NodeInt_NodeInt;

Pair_NodeInt_NodeInt* makePair_NodeInt_NodeInt(NodeInt* fst, NodeInt* snd) {
  Pair_NodeInt_NodeInt* p = malloc(sizeof(Pair_NodeInt_NodeInt));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
NodeInt* v32(void* env32, void* v13_raw);
Closure* v33(void* env33, void* v12_raw);
NodeInt* v35(void* env35, void* v11_raw);
Closure* v36(void* env36, void* v10_raw);
NodeInt* v38(void* env38, void* v9_raw);
Closure* v7(void* env7, void* v8_raw);
NodeInt* v43(void* env43, void* v6_raw);
Pair_NodeInt_NodeInt* v45(void* env45, void* v23_raw);
Pair_NodeInt_NodeInt* v48(void* env48, void* v22_raw);
Closure* v49(void* env49, void* v21_raw);
Pair_NodeInt_NodeInt* v51(void* env51, void* v20_raw);
Pair_NodeInt_NodeInt* v53(void* env53, void* v19_raw);
Pair_NodeInt_NodeInt* v17(void* env17, void* v18_raw);
Pair_NodeInt_NodeInt* v56(void* env56, void* v16_raw);
Pair_NodeInt_NodeInt* v58(void* env58, void* v15_raw);
int v61(void* env61, void* v27_raw);
Closure* v62(void* env62, void* v26_raw);
int v24(void* env24, void* v25_raw);
Pair_NodeInt_NodeInt* v66(void* env66, void* v14_raw);
NodeInt* v69(void* env69, void* v5_raw);
Closure* v70(void* env70, void* v4_raw);
NodeInt* v72(void* env72, void* v3_raw);
Closure* v73(void* env73, void* v2_raw);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
} Env_v7;

typedef struct {
} Env_v17;

typedef struct {
} Env_v24;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
    int v12;
} Env_v32;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
} Env_v33;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
    int v10;
} Env_v35;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
} Env_v36;

typedef struct {
    NodeInt* v8;
} Env_v38;

typedef struct {
} Env_v43;

typedef struct {
    int v21;
} Env_v45;

typedef struct {
    int v19;
    int v21;
} Env_v48;

typedef struct {
    int v19;
} Env_v49;

typedef struct {
    int v19;
} Env_v51;

typedef struct {
    Pair_Int_NodeInt *v18;
} Env_v53;

typedef struct {
    NodeInt* v14;
} Env_v56;

typedef struct {
    NodeInt* v14;
} Env_v58;

typedef struct {
} Env_v61;

typedef struct {
} Env_v62;

typedef struct {
} Env_v66;

typedef struct {
    int v2;
    int v4;
} Env_v69;

typedef struct {
    int v2;
} Env_v70;

typedef struct {
    int v2;
} Env_v72;

typedef struct {
} Env_v73;

// function implementations
NodeInt* v32(void* env32, void* v13_raw) {
  NodeInt* v13 = (NodeInt*)v13_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  if ((((Env_v32*)env32)->v10 < ((Env_v32*)env32)->v12)) {
    Closure* c7 = v7(env7, (void*)(((Env_v32*)env32)->v11));
    return consInt(((Env_v32*)env32)->v10, (NodeInt*)(c7)->fn((c7)->env, ((Env_v32*)env32)->v9));
  } else {
    Closure* c7 = v7(env7, (void*)(v13));
    return consInt(((Env_v32*)env32)->v12, (NodeInt*)(c7)->fn((c7)->env, ((Env_v32*)env32)->v8));
  }
}

Closure* v33(void* env33, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v12 = v12;
  env32->v8 = ((Env_v33*)env33)->v8;
  env32->v9 = ((Env_v33*)env33)->v9;
  env32->v10 = ((Env_v33*)env33)->v10;
  env32->v11 = ((Env_v33*)env33)->v11;
  Closure* c32 = malloc(sizeof(Closure));
  c32->env = env32;
  c32->fn = (void* (*)(void*, void*))v32;
  return c32;
}

NodeInt* v35(void* env35, void* v11_raw) {
  NodeInt* v11 = (NodeInt*)v11_raw;
  Env_v33* env33 = malloc(sizeof(Env_v33));
  env33->v11 = v11;
  env33->v8 = ((Env_v35*)env35)->v8;
  env33->v9 = ((Env_v35*)env35)->v9;
  env33->v10 = ((Env_v35*)env35)->v10;
  if (((((Env_v35*)env35)->v9) == NULL)) return ((Env_v35*)env35)->v8;
  Closure* c33 = v33(env33, box_int((((Env_v35*)env35)->v9)->head));
  return (NodeInt*)(c33)->fn((c33)->env, (((Env_v35*)env35)->v9)->tail);
}

Closure* v36(void* env36, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v35* env35 = malloc(sizeof(Env_v35));
  env35->v10 = v10;
  env35->v8 = ((Env_v36*)env36)->v8;
  env35->v9 = ((Env_v36*)env36)->v9;
  Closure* c35 = malloc(sizeof(Closure));
  c35->env = env35;
  c35->fn = (void* (*)(void*, void*))v35;
  return c35;
}

NodeInt* v38(void* env38, void* v9_raw) {
  NodeInt* v9 = (NodeInt*)v9_raw;
  Env_v36* env36 = malloc(sizeof(Env_v36));
  env36->v9 = v9;
  env36->v8 = ((Env_v38*)env38)->v8;
  if (((((Env_v38*)env38)->v8) == NULL)) return v9;
  Closure* c36 = v36(env36, box_int((((Env_v38*)env38)->v8)->head));
  return (NodeInt*)(c36)->fn((c36)->env, (((Env_v38*)env38)->v8)->tail);
}

Closure* v7(void* env7, void* v8_raw) {
  NodeInt* v8 = (NodeInt*)v8_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v8 = v8;
  Closure* c38 = malloc(sizeof(Closure));
  c38->env = env38;
  c38->fn = (void* (*)(void*, void*))v38;
  return c38;
}

NodeInt* v43(void* env43, void* v6_raw) {
  Pair_NodeInt_NodeInt *v6 = (Pair_NodeInt_NodeInt*)v6_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  Closure* c7 = v7(env7, (void*)(v0((v6)->fst)));
  return (NodeInt*)(c7)->fn((c7)->env, v0((v6)->snd));
}

Pair_NodeInt_NodeInt* v45(void* env45, void* v23_raw) {
  Pair_NodeInt_NodeInt *v23 = (Pair_NodeInt_NodeInt*)v23_raw;
  return makePair_NodeInt_NodeInt(consInt(((Env_v45*)env45)->v21, (v23)->fst), (v23)->snd);
}

Pair_NodeInt_NodeInt* v48(void* env48, void* v22_raw) {
  NodeInt* v22 = (NodeInt*)v22_raw;
  Env_v17* env17 = malloc(sizeof(Env_v17));
  Env_v45* env45 = malloc(sizeof(Env_v45));
  env45->v21 = ((Env_v48*)env48)->v21;
  return v45(env45, (void*)(v17(env17, (void*)(makePair_Int_NodeInt((((Env_v48*)env48)->v19 - 1), v22)))));
}

Closure* v49(void* env49, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  env48->v21 = v21;
  env48->v19 = ((Env_v49*)env49)->v19;
  Closure* c48 = malloc(sizeof(Closure));
  c48->env = env48;
  c48->fn = (void* (*)(void*, void*))v48;
  return c48;
}

Pair_NodeInt_NodeInt* v51(void* env51, void* v20_raw) {
  NodeInt* v20 = (NodeInt*)v20_raw;
  Env_v49* env49 = malloc(sizeof(Env_v49));
  env49->v19 = ((Env_v51*)env51)->v19;
  if ((((Env_v51*)env51)->v19 == 0)) {
    return makePair_NodeInt_NodeInt(NULL, v20);
  } else {
    if (((v20) == NULL)) {
      return makePair_NodeInt_NodeInt(NULL, NULL);
    } else {
      Closure* c49 = v49(env49, box_int((v20)->head));
      return (Pair_NodeInt_NodeInt*)(c49)->fn((c49)->env, (v20)->tail);
    }
  }
}

Pair_NodeInt_NodeInt* v53(void* env53, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v51* env51 = malloc(sizeof(Env_v51));
  env51->v19 = v19;
  return v51(env51, (void*)((((Env_v53*)env53)->v18)->snd));
}

Pair_NodeInt_NodeInt* v17(void* env17, void* v18_raw) {
  Pair_Int_NodeInt *v18 = (Pair_Int_NodeInt*)v18_raw;
  Env_v53* env53 = malloc(sizeof(Env_v53));
  env53->v18 = v18;
  return v53(env53, box_int((v18)->fst));
}

Pair_NodeInt_NodeInt* v56(void* env56, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  Env_v17* env17 = malloc(sizeof(Env_v17));
  return v17(env17, (void*)(makePair_Int_NodeInt(v16, ((Env_v56*)env56)->v14)));
}

Pair_NodeInt_NodeInt* v58(void* env58, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v56* env56 = malloc(sizeof(Env_v56));
  env56->v14 = ((Env_v58*)env58)->v14;
  return v56(env56, box_int((v15 / 2)));
}

int v61(void* env61, void* v27_raw) {
  NodeInt* v27 = (NodeInt*)v27_raw;
  Env_v24* env24 = malloc(sizeof(Env_v24));
  return (1 + v24(env24, (void*)(v27)));
}

Closure* v62(void* env62, void* v26_raw) {
  int v26 = *(int*)v26_raw;
  Env_v61* env61 = malloc(sizeof(Env_v61));
  Closure* c61 = malloc(sizeof(Closure));
  c61->env = env61;
  c61->fn = (void* (*)(void*, void*))v61;
  return c61;
}

int v24(void* env24, void* v25_raw) {
  NodeInt* v25 = (NodeInt*)v25_raw;
  Env_v62* env62 = malloc(sizeof(Env_v62));
  if (((v25) == NULL)) return 0;
  Closure* c62 = v62(env62, box_int((v25)->head));
  return (int)(intptr_t)(c62)->fn((c62)->env, (v25)->tail);
}

Pair_NodeInt_NodeInt* v66(void* env66, void* v14_raw) {
  NodeInt* v14 = (NodeInt*)v14_raw;
  Env_v24* env24 = malloc(sizeof(Env_v24));
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v14 = v14;
  return v58(env58, box_int(v24(env24, (void*)(v14))));
}

NodeInt* v69(void* env69, void* v5_raw) {
  NodeInt* v5 = (NodeInt*)v5_raw;
  Env_v43* env43 = malloc(sizeof(Env_v43));
  Env_v66* env66 = malloc(sizeof(Env_v66));
  return v43(env43, (void*)(v66(env66, (void*)(consInt(((Env_v69*)env69)->v2, consInt(((Env_v69*)env69)->v4, v5))))));
}

Closure* v70(void* env70, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v4 = v4;
  env69->v2 = ((Env_v70*)env70)->v2;
  Closure* c69 = malloc(sizeof(Closure));
  c69->env = env69;
  c69->fn = (void* (*)(void*, void*))v69;
  return c69;
}

NodeInt* v72(void* env72, void* v3_raw) {
  NodeInt* v3 = (NodeInt*)v3_raw;
  Env_v70* env70 = malloc(sizeof(Env_v70));
  env70->v2 = ((Env_v72*)env72)->v2;
  if (((v3) == NULL)) {
    return consInt(((Env_v72*)env72)->v2, NULL);
  } else {
    Closure* c70 = v70(env70, box_int((v3)->head));
    return (NodeInt*)(c70)->fn((c70)->env, (v3)->tail);
  }
}

Closure* v73(void* env73, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v2 = v2;
  Closure* c72 = malloc(sizeof(Closure));
  c72->env = env72;
  c72->fn = (void* (*)(void*, void*))v72;
  return c72;
}

NodeInt* v0(NodeInt* v1) {
  Env_v73* env73 = malloc(sizeof(Env_v73));
  if (((v1) == NULL)) return NULL;
  Closure* c73 = v73(env73, box_int((v1)->head));
  return (NodeInt*)(c73)->fn((c73)->env, (v1)->tail);
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

