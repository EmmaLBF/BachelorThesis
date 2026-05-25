
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
// function implementations
NodeInt* v7(NodeInt* v8, NodeInt* v9) {
  if (((v8) == NULL)) {
    return v9;
  } else {
    int v10 = (v8)->head;
    int v12 = (v9)->head;
    NodeInt* v29 = NULL;
    if ((v10 < v12)) {
      v29 = consInt(v10, v7((v8)->tail, v9));
    } else {
      v29 = consInt(v12, v7((v9)->tail, v8));
    }
    return (NodeInt*)((((v9) == NULL)) ? (v8) : ((NodeInt*)v29));
  }
}

Pair* v17(Pair* v18) {
  int v19 = *(int*)((v18)->fst);
  NodeInt* v20 = (NodeInt*)((v18)->snd);
  Pair* v55 = NULL;
  if ((v19 == 0)) {
    v55 = mk_pair(NULL, v20);
  } else {
    Pair* v23 = v17(mk_pair(box_int((v19 - 1)), (v20)->tail));
    v55 = ((((v20) == NULL)) ? (mk_pair(NULL, NULL)) : ((Pair*)(Pair*)mk_pair(consInt((v20)->head, (NodeInt*)((v23)->fst)), (NodeInt*)((v23)->snd))));
  }
  return (Pair*)(Pair*)v55;
}

int v24(NodeInt* v25) {
  if (((v25) == NULL)) {
    return 0;
  } else {
    return (1 + v24((v25)->tail));
  }
}

NodeInt* v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return NULL;
  } else {
    NodeInt* v3 = (v1)->tail;
    if (((v3) == NULL)) {
      return consInt((v1)->head, NULL);
    } else {
      NodeInt* v14 = consInt((v1)->head, consInt((v3)->head, (v3)->tail));
      Pair* v6 = (Pair*)(Pair*)v17(mk_pair(box_int((v24(v14) / 2)), v14));
      return (NodeInt*)v7(v0((NodeInt*)((v6)->fst)), v0((NodeInt*)((v6)->snd)));
    }
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

