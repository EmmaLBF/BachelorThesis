
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
    NodeInt* v13;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
    int v12;
} Env_v32;

typedef struct {
    int v12;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
} Env_v33;

typedef struct {
    NodeInt* v11;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v8;
    NodeInt* v9;
    int v10;
} Env_v35;

typedef struct {
    int v10;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v8;
    NodeInt* v9;
} Env_v36;

typedef struct {
    NodeInt* v9;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v8;
} Env_v38;

typedef struct {
    NodeInt* v8;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
} Env_v7;

typedef struct {
    Pair_NodeInt_NodeInt *v6;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
} Env_v43;

typedef struct {
    Pair_NodeInt_NodeInt *v23;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
    Pair_Int_NodeInt *v18;
    int v19;
    NodeInt* v20;
    int v21;
    NodeInt* v22;
} Env_v45;

typedef struct {
    NodeInt* v22;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
    Pair_Int_NodeInt *v18;
    int v19;
    NodeInt* v20;
    int v21;
} Env_v48;

typedef struct {
    int v21;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
    Pair_Int_NodeInt *v18;
    int v19;
    NodeInt* v20;
} Env_v49;

typedef struct {
    NodeInt* v20;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
    Pair_Int_NodeInt *v18;
    int v19;
} Env_v51;

typedef struct {
    int v19;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
    Pair_Int_NodeInt *v18;
} Env_v53;

typedef struct {
    Pair_Int_NodeInt *v18;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
    int v16;
} Env_v17;

typedef struct {
    int v16;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    int v15;
} Env_v56;

typedef struct {
    int v15;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
} Env_v58;

typedef struct {
    NodeInt* v27;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    NodeInt* v25;
    int v26;
} Env_v61;

typedef struct {
    int v26;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
    NodeInt* v25;
} Env_v62;

typedef struct {
    NodeInt* v25;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
} Env_v24;

typedef struct {
    NodeInt* v14;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
} Env_v66;

typedef struct {
    NodeInt* v5;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
} Env_v69;

typedef struct {
    int v4;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
} Env_v70;

typedef struct {
    NodeInt* v3;
    NodeInt* v1;
    int v2;
} Env_v72;

typedef struct {
    int v2;
    NodeInt* v1;
} Env_v73;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
NodeInt* v32(void* env32, void* v13_raw) {
  NodeInt* v13 = (NodeInt*)v13_raw;
  if ((((Env_v32*)env32)->v10 < ((Env_v32*)env32)->v12)) {
    return consInt(((Env_v32*)env32)->v10, v7(env7, (void*)(((Env_v32*)env32)->v11))(((Env_v32*)env32)->v9));
  } else {
    return consInt(((Env_v32*)env32)->v12, v7(env7, (void*)(v13))(((Env_v32*)env32)->v8));
  }
}

Closure* v33(void* env33, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v12 = v12;
  env32->v1 = ((Env_v32*)env32)->v1;
  env32->v2 = ((Env_v32*)env32)->v2;
  env32->v3 = ((Env_v32*)env32)->v3;
  env32->v4 = ((Env_v32*)env32)->v4;
  env32->v5 = ((Env_v32*)env32)->v5;
  env32->v6 = ((Env_v32*)env32)->v6;
  env32->v8 = ((Env_v32*)env32)->v8;
  env32->v9 = ((Env_v32*)env32)->v9;
  env32->v10 = ((Env_v32*)env32)->v10;
  env32->v11 = ((Env_v32*)env32)->v11;
  env32->v12 = ((Env_v32*)env32)->v12;
  Closure* c32 = malloc(sizeof(Closure));
  c32->env = env32;
  c32->fn = (void* (*)(void*, void*))v32;
  return c32;
}

NodeInt* v35(void* env35, void* v11_raw) {
  NodeInt* v11 = (NodeInt*)v11_raw;
  if (((((Env_v35*)env35)->v9) == NULL)) {
    return ((Env_v35*)env35)->v8;
  } else {
    return v33(env33, box_int((((Env_v35*)env35)->v9)->head))((((Env_v35*)env35)->v9)->tail);
  }
}

Closure* v36(void* env36, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v35* env35 = malloc(sizeof(Env_v35));
  env35->v10 = v10;
  env35->v1 = ((Env_v35*)env35)->v1;
  env35->v2 = ((Env_v35*)env35)->v2;
  env35->v3 = ((Env_v35*)env35)->v3;
  env35->v4 = ((Env_v35*)env35)->v4;
  env35->v5 = ((Env_v35*)env35)->v5;
  env35->v6 = ((Env_v35*)env35)->v6;
  env35->v8 = ((Env_v35*)env35)->v8;
  env35->v9 = ((Env_v35*)env35)->v9;
  env35->v10 = ((Env_v35*)env35)->v10;
  Closure* c35 = malloc(sizeof(Closure));
  c35->env = env35;
  c35->fn = (void* (*)(void*, void*))v35;
  return c35;
}

NodeInt* v38(void* env38, void* v9_raw) {
  NodeInt* v9 = (NodeInt*)v9_raw;
  if (((((Env_v38*)env38)->v8) == NULL)) {
    return v9;
  } else {
    return v36(env36, box_int((((Env_v38*)env38)->v8)->head))((((Env_v38*)env38)->v8)->tail);
  }
}

Closure* v7(void* env7, void* v8_raw) {
  NodeInt* v8 = (NodeInt*)v8_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v8 = v8;
  env38->v1 = ((Env_v38*)env38)->v1;
  env38->v2 = ((Env_v38*)env38)->v2;
  env38->v3 = ((Env_v38*)env38)->v3;
  env38->v4 = ((Env_v38*)env38)->v4;
  env38->v5 = ((Env_v38*)env38)->v5;
  env38->v6 = ((Env_v38*)env38)->v6;
  env38->v8 = ((Env_v38*)env38)->v8;
  Closure* c38 = malloc(sizeof(Closure));
  c38->env = env38;
  c38->fn = (void* (*)(void*, void*))v38;
  return c38;
}

NodeInt* v43(void* env43, void* v6_raw) {
  Pair_NodeInt_NodeInt *v6 = (Pair_NodeInt_NodeInt*)v6_raw;
  return v7(env7, (void*)(v0((v6)->fst)))(v0((v6)->snd));
}

Pair_NodeInt_NodeInt* v45(void* env45, void* v23_raw) {
  Pair_NodeInt_NodeInt *v23 = (Pair_NodeInt_NodeInt*)v23_raw;
  return makePair_NodeInt_NodeInt(consInt(((Env_v45*)env45)->v21, (v23)->fst), (v23)->snd);
}

Pair_NodeInt_NodeInt* v48(void* env48, void* v22_raw) {
  NodeInt* v22 = (NodeInt*)v22_raw;
  return v45(env45, (void*)(v17(makePair_Int_NodeInt((((Env_v48*)env48)->v19 - 1), v22))));
}

Closure* v49(void* env49, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  env48->v21 = v21;
  env48->v1 = ((Env_v48*)env48)->v1;
  env48->v2 = ((Env_v48*)env48)->v2;
  env48->v3 = ((Env_v48*)env48)->v3;
  env48->v4 = ((Env_v48*)env48)->v4;
  env48->v5 = ((Env_v48*)env48)->v5;
  env48->v14 = ((Env_v48*)env48)->v14;
  env48->v15 = ((Env_v48*)env48)->v15;
  env48->v16 = ((Env_v48*)env48)->v16;
  env48->v18 = ((Env_v48*)env48)->v18;
  env48->v19 = ((Env_v48*)env48)->v19;
  env48->v20 = ((Env_v48*)env48)->v20;
  env48->v21 = ((Env_v48*)env48)->v21;
  Closure* c48 = malloc(sizeof(Closure));
  c48->env = env48;
  c48->fn = (void* (*)(void*, void*))v48;
  return c48;
}

Pair_NodeInt_NodeInt* v51(void* env51, void* v20_raw) {
  NodeInt* v20 = (NodeInt*)v20_raw;
  if ((((Env_v51*)env51)->v19 == 0)) {
    return makePair_NodeInt_NodeInt(NULL, v20);
  } else {
    if (((v20) == NULL)) {
      return makePair_NodeInt_NodeInt(NULL, NULL);
    } else {
      return v49(env49, box_int((v20)->head))((v20)->tail);
    }
  }
}

Pair_NodeInt_NodeInt* v53(void* env53, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  return v51(env51, (void*)((((Env_v53*)env53)->v18)->snd));
}

Pair_NodeInt_NodeInt* v17(void* env17, void* v18_raw) {
  Pair_Int_NodeInt *v18 = (Pair_Int_NodeInt*)v18_raw;
  return v53(env53, box_int((v18)->fst));
}

Pair_NodeInt_NodeInt* v56(void* env56, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(env17, (void*)(makePair_Int_NodeInt(v16, ((Env_v56*)env56)->v14)));
}

Pair_NodeInt_NodeInt* v58(void* env58, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  return v56(env56, box_int((v15 / 2)));
}

int v61(void* env61, void* v27_raw) {
  NodeInt* v27 = (NodeInt*)v27_raw;
  return (1 + v24(env24, (void*)(v27)));
}

Closure* v62(void* env62, void* v26_raw) {
  int v26 = *(int*)v26_raw;
  Env_v61* env61 = malloc(sizeof(Env_v61));
  env61->v26 = v26;
  env61->v1 = ((Env_v61*)env61)->v1;
  env61->v2 = ((Env_v61*)env61)->v2;
  env61->v3 = ((Env_v61*)env61)->v3;
  env61->v4 = ((Env_v61*)env61)->v4;
  env61->v5 = ((Env_v61*)env61)->v5;
  env61->v14 = ((Env_v61*)env61)->v14;
  env61->v25 = ((Env_v61*)env61)->v25;
  env61->v26 = ((Env_v61*)env61)->v26;
  Closure* c61 = malloc(sizeof(Closure));
  c61->env = env61;
  c61->fn = (void* (*)(void*, void*))v61;
  return c61;
}

int v24(void* env24, void* v25_raw) {
  NodeInt* v25 = (NodeInt*)v25_raw;
  if (((v25) == NULL)) {
    return 0;
  } else {
    return v62(env62, box_int((v25)->head))((v25)->tail);
  }
}

Pair_NodeInt_NodeInt* v66(void* env66, void* v14_raw) {
  NodeInt* v14 = (NodeInt*)v14_raw;
  return v58(env58, box_int(v24(v14)));
}

NodeInt* v69(void* env69, void* v5_raw) {
  NodeInt* v5 = (NodeInt*)v5_raw;
  return v43(env43, (void*)(v66(consInt(((Env_v69*)env69)->v2, consInt(((Env_v69*)env69)->v4, v5)))));
}

Closure* v70(void* env70, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v4 = v4;
  env69->v1 = ((Env_v69*)env69)->v1;
  env69->v2 = ((Env_v69*)env69)->v2;
  env69->v3 = ((Env_v69*)env69)->v3;
  env69->v4 = ((Env_v69*)env69)->v4;
  Closure* c69 = malloc(sizeof(Closure));
  c69->env = env69;
  c69->fn = (void* (*)(void*, void*))v69;
  return c69;
}

NodeInt* v72(void* env72, void* v3_raw) {
  NodeInt* v3 = (NodeInt*)v3_raw;
  if (((v3) == NULL)) {
    return consInt(((Env_v72*)env72)->v2, NULL);
  } else {
    return v70(env70, box_int((v3)->head))((v3)->tail);
  }
}

Closure* v73(void* env73, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v2 = v2;
  env72->v1 = ((Env_v72*)env72)->v1;
  env72->v2 = ((Env_v72*)env72)->v2;
  Closure* c72 = malloc(sizeof(Closure));
  c72->env = env72;
  c72->fn = (void* (*)(void*, void*))v72;
  return c72;
}

NodeInt* v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return NULL;
  } else {
    return v73(env73, box_int((v1)->head))((v1)->tail);
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

