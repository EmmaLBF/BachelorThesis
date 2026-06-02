
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
NodeInt* v29(void* env29, void* v12_raw, void* v13_raw);
NodeInt* v33(void* env33, void* v10_raw, void* v11_raw);
NodeInt* v7(NodeInt* v8, NodeInt* v9);
NodeInt* v45(Pair_NodeInt_NodeInt *v6);
Pair_NodeInt_NodeInt* v46(void* env46, void* v23_raw);
Pair_NodeInt_NodeInt* v52(void* env52, void* v21_raw, void* v22_raw);
Pair_NodeInt_NodeInt* v55(void* env55, void* v20_raw);
Pair_NodeInt_NodeInt* v58(void* env58, void* v19_raw);
Pair_NodeInt_NodeInt* v17(Pair_Int_NodeInt *v18);
Pair_NodeInt_NodeInt* v63(void* env63, void* v16_raw);
Pair_NodeInt_NodeInt* v66(void* env66, void* v15_raw);
int v68(int v26, NodeInt* v27);
int v24(NodeInt* v25);
Pair_NodeInt_NodeInt* v75(NodeInt* v14);
NodeInt* v81(void* env81, void* v4_raw, void* v5_raw);
NodeInt* v85(int v2, NodeInt* v3);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
    int v12;
    NodeInt* v13;
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
} Env_v29;

typedef struct {
    int v10;
    NodeInt* v11;
    NodeInt* v8;
    NodeInt* v9;
} Env_v33;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
} Env_v7;

typedef struct {
    Pair_NodeInt_NodeInt *v6;
} Env_v45;

typedef struct {
    Pair_NodeInt_NodeInt *v23;
    int v21;
} Env_v46;

typedef struct {
    int v21;
    NodeInt* v22;
    int v19;
} Env_v52;

typedef struct {
    NodeInt* v20;
    int v19;
} Env_v55;

typedef struct {
    int v19;
    Pair_Int_NodeInt *v18;
} Env_v58;

typedef struct {
    Pair_Int_NodeInt *v18;
} Env_v17;

typedef struct {
    int v16;
    NodeInt* v14;
} Env_v63;

typedef struct {
    int v15;
    NodeInt* v14;
} Env_v66;

typedef struct {
    int v26;
    NodeInt* v27;
} Env_v68;

typedef struct {
    NodeInt* v25;
} Env_v24;

typedef struct {
    NodeInt* v14;
} Env_v75;

typedef struct {
    int v4;
    NodeInt* v5;
    int v2;
} Env_v81;

typedef struct {
    int v2;
    NodeInt* v3;
} Env_v85;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
NodeInt* v29(void* env29, void* v12_raw, void* v13_raw) {
  int v12 = *(int*)v12_raw;
  NodeInt* v13 = (NodeInt*)v13_raw;
  if ((((Env_v29*)env29)->v10 < v12)) {
    return consInt(((Env_v29*)env29)->v10, v7(((Env_v29*)env29)->v11, ((Env_v29*)env29)->v9));
  } else {
    return consInt(v12, v7(v13, ((Env_v29*)env29)->v8));
  }
}

NodeInt* v33(void* env33, void* v10_raw, void* v11_raw) {
  int v10 = *(int*)v10_raw;
  NodeInt* v11 = (NodeInt*)v11_raw;
  if (((((Env_v33*)env33)->v9) == NULL)) {
    return ((Env_v33*)env33)->v8;
  } else {
    Env_v29* env29 = malloc(sizeof(Env_v29));
    env29->v10 = v10;
    env29->v11 = v11;
    env29->v8 = ((Env_v33*)env33)->v8;
    env29->v9 = ((Env_v33*)env33)->v9;
    Closure* c29 = malloc(sizeof(Closure));
    c29->env = env29;
    c29->fn = (void* (*)(void*, void*))v29;
    return (NodeInt*)((Closure*)c29)->fn(((Closure*)c29)->env, box_int((((Env_v33*)env33)->v9)->head), (((Env_v33*)env33)->v9)->tail);
  }
}

NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (((v8) == NULL)) {
    return v9;
  } else {
    Env_v33* env33 = malloc(sizeof(Env_v33));
    env33->v8 = v8;
    env33->v9 = v9;
    Closure* c33 = malloc(sizeof(Closure));
    c33->env = env33;
    c33->fn = (void* (*)(void*, void*))v33;
    return (NodeInt*)((Closure*)c33)->fn(((Closure*)c33)->env, box_int((v8)->head), (v8)->tail);
  }
}

NodeInt* v45(Pair_NodeInt_NodeInt *v6) {
  return v7(v0((v6)->fst), v0((v6)->snd));
}

Pair_NodeInt_NodeInt* v46(void* env46, void* v23_raw) {
  Pair_NodeInt_NodeInt *v23 = (Pair_NodeInt_NodeInt*)v23_raw;
  return makePair_NodeInt_NodeInt(consInt(((Env_v46*)env46)->v21, (v23)->fst), (v23)->snd);
}

Pair_NodeInt_NodeInt* v52(void* env52, void* v21_raw, void* v22_raw) {
  int v21 = *(int*)v21_raw;
  NodeInt* v22 = (NodeInt*)v22_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v21 = v21;
  Closure* c46 = malloc(sizeof(Closure));
  c46->env = env46;
  c46->fn = (void* (*)(void*, void*))v46;
  return (Pair_NodeInt_NodeInt*)((Closure*)c46)->fn(((Closure*)c46)->env, v17(makePair_Int_NodeInt((((Env_v52*)env52)->v19 - 1), v22)));
}

Pair_NodeInt_NodeInt* v55(void* env55, void* v20_raw) {
  NodeInt* v20 = (NodeInt*)v20_raw;
  if ((((Env_v55*)env55)->v19 == 0)) {
    return makePair_NodeInt_NodeInt(NULL, v20);
  } else {
    if (((v20) == NULL)) {
      return makePair_NodeInt_NodeInt(NULL, NULL);
    } else {
      Env_v52* env52 = malloc(sizeof(Env_v52));
      env52->v19 = ((Env_v55*)env55)->v19;
      Closure* c52 = malloc(sizeof(Closure));
      c52->env = env52;
      c52->fn = (void* (*)(void*, void*))v52;
      return (Pair_NodeInt_NodeInt*)((Closure*)c52)->fn(((Closure*)c52)->env, box_int((v20)->head), (v20)->tail);
    }
  }
}

Pair_NodeInt_NodeInt* v58(void* env58, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  Closure* c55 = malloc(sizeof(Closure));
  c55->env = env55;
  c55->fn = (void* (*)(void*, void*))v55;
  return (Pair_NodeInt_NodeInt*)((Closure*)c55)->fn(((Closure*)c55)->env, (((Env_v58*)env58)->v18)->snd);
}

Pair_NodeInt_NodeInt* v17(Pair_Int_NodeInt *v18) {
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v18 = v18;
  Closure* c58 = malloc(sizeof(Closure));
  c58->env = env58;
  c58->fn = (void* (*)(void*, void*))v58;
  return (Pair_NodeInt_NodeInt*)((Closure*)c58)->fn(((Closure*)c58)->env, box_int((v18)->fst));
}

Pair_NodeInt_NodeInt* v63(void* env63, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(makePair_Int_NodeInt(v16, ((Env_v63*)env63)->v14));
}

Pair_NodeInt_NodeInt* v66(void* env66, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  env63->v14 = ((Env_v66*)env66)->v14;
  Closure* c63 = malloc(sizeof(Closure));
  c63->env = env63;
  c63->fn = (void* (*)(void*, void*))v63;
  return (Pair_NodeInt_NodeInt*)((Closure*)c63)->fn(((Closure*)c63)->env, box_int((v15 / 2)));
}

int v68(int v26, NodeInt* v27) {
  return (1 + v24(v27));
}

int v24(NodeInt* v25) {
  if (((v25) == NULL)) {
    return 0;
  } else {
    return v68((v25)->head, (v25)->tail);
  }
}

Pair_NodeInt_NodeInt* v75(NodeInt* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  Closure* c66 = malloc(sizeof(Closure));
  c66->env = env66;
  c66->fn = (void* (*)(void*, void*))v66;
  return (Pair_NodeInt_NodeInt*)((Closure*)c66)->fn(((Closure*)c66)->env, box_int(v24(v14)));
}

NodeInt* v81(void* env81, void* v4_raw, void* v5_raw) {
  int v4 = *(int*)v4_raw;
  NodeInt* v5 = (NodeInt*)v5_raw;
  return v45(v75(consInt(((Env_v81*)env81)->v2, consInt(v4, v5))));
}

NodeInt* v85(int v2, NodeInt* v3) {
  if (((v3) == NULL)) {
    return consInt(v2, NULL);
  } else {
    Env_v81* env81 = malloc(sizeof(Env_v81));
    env81->v2 = v2;
    Closure* c81 = malloc(sizeof(Closure));
    c81->env = env81;
    c81->fn = (void* (*)(void*, void*))v81;
    return (NodeInt*)((Closure*)c81)->fn(((Closure*)c81)->env, box_int((v3)->head), (v3)->tail);
  }
}

NodeInt* v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return NULL;
  } else {
    return v85((v1)->head, (v1)->tail);
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

