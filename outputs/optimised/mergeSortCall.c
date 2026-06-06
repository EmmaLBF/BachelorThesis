
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
NodeInt* v33(void* env33, void* v12_raw, void* v13_raw);
NodeInt* v36(void* env36, void* v10_raw, void* v11_raw);
NodeInt* v7(void* env7, void* v8_raw, void* v9_raw);
NodeInt* v43(void* env43, void* v6_raw);
Pair_NodeInt_NodeInt* v45(void* env45, void* v23_raw);
Pair_NodeInt_NodeInt* v49(void* env49, void* v21_raw, void* v22_raw);
Pair_NodeInt_NodeInt* v51(void* env51, void* v20_raw);
Pair_NodeInt_NodeInt* v53(void* env53, void* v19_raw);
Pair_NodeInt_NodeInt* v17(void* env17, void* v18_raw);
Pair_NodeInt_NodeInt* v56(void* env56, void* v16_raw);
Pair_NodeInt_NodeInt* v58(void* env58, void* v15_raw);
int v62(void* env62, void* v26_raw, void* v27_raw);
int v24(void* env24, void* v25_raw);
Pair_NodeInt_NodeInt* v66(void* env66, void* v14_raw);
NodeInt* v70(void* env70, void* v4_raw, void* v5_raw);
NodeInt* v73(void* env73, void* v2_raw, void* v3_raw);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
    int v12;
    NodeInt* v13;
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
} Env_v33;

typedef struct {
    int v10;
    NodeInt* v11;
    NodeInt* v8;
    NodeInt* v9;
} Env_v36;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
} Env_v7;

typedef struct {
    Pair_NodeInt_NodeInt *v6;
} Env_v43;

typedef struct {
    Pair_NodeInt_NodeInt *v23;
    int v21;
} Env_v45;

typedef struct {
    int v21;
    NodeInt* v22;
    int v19;
} Env_v49;

typedef struct {
    NodeInt* v20;
    int v19;
} Env_v51;

typedef struct {
    int v19;
    Pair_Int_NodeInt *v18;
} Env_v53;

typedef struct {
    Pair_Int_NodeInt *v18;
} Env_v17;

typedef struct {
    int v16;
    NodeInt* v14;
} Env_v56;

typedef struct {
    int v15;
    NodeInt* v14;
} Env_v58;

typedef struct {
    int v26;
    NodeInt* v27;
} Env_v62;

typedef struct {
    NodeInt* v25;
} Env_v24;

typedef struct {
    NodeInt* v14;
} Env_v66;

typedef struct {
    int v4;
    NodeInt* v5;
    int v2;
} Env_v70;

typedef struct {
    int v2;
    NodeInt* v3;
} Env_v73;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
NodeInt* v33(void* env33, void* v12_raw, void* v13_raw) {
  int v12 = *(int*)v12_raw;
  NodeInt* v13 = (NodeInt*)v13_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  if ((((Env_v33*)env33)->v10 < v12)) {
    return consInt(((Env_v33*)env33)->v10, v7(env7, (void*)(((Env_v33*)env33)->v11), (void*)(((Env_v33*)env33)->v9)));
  } else {
    return consInt(v12, v7(env7, (void*)(v13), (void*)(((Env_v33*)env33)->v8)));
  }
}

NodeInt* v36(void* env36, void* v10_raw, void* v11_raw) {
  int v10 = *(int*)v10_raw;
  NodeInt* v11 = (NodeInt*)v11_raw;
  Env_v33* env33 = malloc(sizeof(Env_v33));
  env33->v10 = v10;
  env33->v11 = v11;
  env33->v8 = ((Env_v36*)env36)->v8;
  env33->v9 = ((Env_v36*)env36)->v9;
  if (((((Env_v36*)env36)->v9) == NULL)) {
    return ((Env_v36*)env36)->v8;
  } else {
    return v33(env33, box_int((((Env_v36*)env36)->v9)->head), (void*)((((Env_v36*)env36)->v9)->tail));
  }
}

NodeInt* v7(void* env7, void* v8_raw, void* v9_raw) {
  NodeInt* v8 = (NodeInt*)v8_raw;
  NodeInt* v9 = (NodeInt*)v9_raw;
  Env_v36* env36 = malloc(sizeof(Env_v36));
  env36->v8 = v8;
  env36->v9 = v9;
  if (((v8) == NULL)) {
    return v9;
  } else {
    return v36(env36, box_int((v8)->head), (void*)((v8)->tail));
  }
}

NodeInt* v43(void* env43, void* v6_raw) {
  Pair_NodeInt_NodeInt *v6 = (Pair_NodeInt_NodeInt*)v6_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  return v7(env7, (void*)(v0((v6)->fst)), (void*)(v0((v6)->snd)));
}

Pair_NodeInt_NodeInt* v45(void* env45, void* v23_raw) {
  Pair_NodeInt_NodeInt *v23 = (Pair_NodeInt_NodeInt*)v23_raw;
  return makePair_NodeInt_NodeInt(consInt(((Env_v45*)env45)->v21, (v23)->fst), (v23)->snd);
}

Pair_NodeInt_NodeInt* v49(void* env49, void* v21_raw, void* v22_raw) {
  int v21 = *(int*)v21_raw;
  NodeInt* v22 = (NodeInt*)v22_raw;
  Env_v17* env17 = malloc(sizeof(Env_v17));
  Env_v45* env45 = malloc(sizeof(Env_v45));
  env45->v21 = v21;
  return v45(env45, (void*)(v17(env17, (void*)(makePair_Int_NodeInt((((Env_v49*)env49)->v19 - 1), v22)))));
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
      return v49(env49, box_int((v20)->head), (void*)((v20)->tail));
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

int v62(void* env62, void* v26_raw, void* v27_raw) {
  int v26 = *(int*)v26_raw;
  NodeInt* v27 = (NodeInt*)v27_raw;
  Env_v24* env24 = malloc(sizeof(Env_v24));
  return (1 + v24(env24, (void*)(v27)));
}

int v24(void* env24, void* v25_raw) {
  NodeInt* v25 = (NodeInt*)v25_raw;
  Env_v62* env62 = malloc(sizeof(Env_v62));
  if (((v25) == NULL)) {
    return 0;
  } else {
    return v62(env62, box_int((v25)->head), (void*)((v25)->tail));
  }
}

Pair_NodeInt_NodeInt* v66(void* env66, void* v14_raw) {
  NodeInt* v14 = (NodeInt*)v14_raw;
  Env_v24* env24 = malloc(sizeof(Env_v24));
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v14 = v14;
  return v58(env58, box_int(v24(env24, (void*)(v14))));
}

NodeInt* v70(void* env70, void* v4_raw, void* v5_raw) {
  int v4 = *(int*)v4_raw;
  NodeInt* v5 = (NodeInt*)v5_raw;
  Env_v43* env43 = malloc(sizeof(Env_v43));
  Env_v66* env66 = malloc(sizeof(Env_v66));
  return v43(env43, (void*)(v66(env66, (void*)(consInt(((Env_v70*)env70)->v2, consInt(v4, v5))))));
}

NodeInt* v73(void* env73, void* v2_raw, void* v3_raw) {
  int v2 = *(int*)v2_raw;
  NodeInt* v3 = (NodeInt*)v3_raw;
  Env_v70* env70 = malloc(sizeof(Env_v70));
  env70->v2 = v2;
  if (((v3) == NULL)) {
    return consInt(v2, NULL);
  } else {
    return v70(env70, box_int((v3)->head), (void*)((v3)->tail));
  }
}

NodeInt* v0(NodeInt* v1) {
  Env_v73* env73 = malloc(sizeof(Env_v73));
  if (((v1) == NULL)) {
    return NULL;
  } else {
    return v73(env73, box_int((v1)->head), (void*)((v1)->tail));
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

