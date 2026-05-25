
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
NodeInt* v7(NodeInt* v8, NodeInt* v9);
Pair* v17(Pair* v18);
int v24(NodeInt* v25);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v8;
    NodeInt* v9;
} Env_v7;

typedef struct {
    Pair* v18;
} Env_v17;

typedef struct {
    NodeInt* v25;
} Env_v24;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (isEmptyInt(v8)) {
    return v9;
  } else {
    int v10 = (headInt(v8));
    NodeInt* v11 = tailInt(v8);
    int v12 = (headInt(v9));
    NodeInt* v13 = tailInt(v9);
    NodeInt* v29 = NULL;
    if ((v10 < v12)) {
      v29 = consInt(v10, v7(v11, v9));
    } else {
      v29 = consInt(v12, v7(v13, v8));
    }
    return (NodeInt*)((isEmptyInt(v9)) ? (v8) : ((NodeInt*)v29));
  }
}

Pair* v17(Pair* v18) {
  int v19 = *(int*)(fst(v18));
  NodeInt* v20 = (NodeInt*)(snd(v18));
  Pair* v55 = NULL;
  if ((v19 == 0)) {
    v55 = mk_pair(NULL, v20);
  } else {
    int v21 = (headInt(v20));
    NodeInt* v22 = tailInt(v20);
    Pair* v23 = v17(mk_pair(box_int((v19 - 1)), v22));
    v55 = ((isEmptyInt(v20)) ? (mk_pair(NULL, NULL)) : ((Pair*)(Pair*)mk_pair(consInt(v21, (NodeInt*)(fst(v23))), (NodeInt*)(snd(v23)))));
  }
  return (Pair*)(Pair*)v55;
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
      NodeInt* v14 = consInt(v2, consInt(v4, v5));
      int v15 = v24(v14);
      int v16 = (v15 / 2);
      Pair* v6 = (Pair*)(Pair*)v17(mk_pair(box_int(v16), v14));
      return (NodeInt*)v7(v0((NodeInt*)(fst(v6))), v0((NodeInt*)(snd(v6))));
    }
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

