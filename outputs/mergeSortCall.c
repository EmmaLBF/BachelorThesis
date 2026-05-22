
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
NodeInt* v29(void* env29, void* v12_raw, void* v13_raw);
NodeInt* v7(NodeInt* v8, NodeInt* v9);
NodeInt* v45(Pair* v6);
Pair* v46(void* env46, void* v23_raw);
Pair* v55(void* env55, void* v20_raw);
Pair* v17(Pair* v18);
Pair* v63(void* env63, void* v16_raw);
int v24(NodeInt* v25);
Pair* v75(NodeInt* v14);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v8;
    NodeInt* v9;
    int v10;
    NodeInt* v11;
} Env_v29;

typedef struct {
    NodeInt* v8;
    NodeInt* v9;
} Env_v33;

typedef struct {
    int v21;
} Env_v46;

typedef struct {
    int v19;
} Env_v52;

typedef struct {
    int v19;
} Env_v55;

typedef struct {
    Pair* v18;
} Env_v58;

typedef struct {
    NodeInt* v14;
} Env_v63;

typedef struct {
    NodeInt* v14;
} Env_v66;

typedef struct {
    int v2;
} Env_v81;

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

NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (isEmptyInt(v8)) {
    return v9;
  } else {
    int v10 = (headInt(v8));
    NodeInt* v11 = tailInt(v8);
    Env_v29* env29 = malloc(sizeof(Env_v29));
    env29->v10 = v10;
    env29->v11 = v11;
    env29->v8 = v8;
    env29->v9 = v9;
    return (NodeInt*)((isEmptyInt(v9)) ? (v8) : ((NodeInt*)v29(env29, box_int((headInt(v9))), (void*)(tailInt(v9)))));
  }
}

NodeInt* v45(Pair* v6) {
  return v7(v0((NodeInt*)(fst(v6))), v0((NodeInt*)(snd(v6))));
}

Pair* v46(void* env46, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(consInt(((Env_v46*)env46)->v21, (NodeInt*)(fst(v23))), (NodeInt*)(snd(v23)));
}

Pair* v55(void* env55, void* v20_raw) {
  NodeInt* v20 = (NodeInt*)v20_raw;
  if ((((Env_v55*)env55)->v19 == 0)) {
    return mk_pair(NULL, v20);
  } else {
    if (isEmptyInt(v20)) {
      return mk_pair(NULL, NULL);
    } else {
      void* env52 = env55;
      int v21 = (headInt(v20));
      NodeInt* v22 = tailInt(v20);
      Env_v46* env46 = malloc(sizeof(Env_v46));
      env46->v21 = v21;
      return (Pair*)(Pair*)v46(env46, (void*)(v17(mk_pair(box_int((((Env_v52*)env52)->v19 - 1)), v22))));
    }
  }
}

Pair* v17(Pair* v18) {
  int v19 = *(int*)(fst(v18));
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  return (Pair*)(Pair*)v55(env55, (void*)((NodeInt*)(snd(v18))));
}

Pair* v63(void* env63, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v63*)env63)->v14));
}

int v24(NodeInt* v25) {
  if (isEmptyInt(v25)) {
    return 0;
  } else {
    int v26 = (headInt(v25));
    NodeInt* v27 = tailInt(v25);
    return (1 + v24(v27));
  }
}

Pair* v75(NodeInt* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  int v15 = v24(v14);
  return (Pair*)(Pair*)v63(env66, box_int((v15 / 2)));
}

NodeInt* v0(NodeInt* v1) {
  if (isEmptyInt(v1)) {
    return NULL;
  } else {
    int v2 = (headInt(v1));
    NodeInt* v3 = tailInt(v1);
    if (isEmptyInt(v3)) {
      return consInt(v2, NULL);
    } else {
      int v4 = (headInt(v3));
      NodeInt* v5 = tailInt(v3);
      return (NodeInt*)v45(v75(consInt(v2, consInt(v4, v5))));
    }
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

