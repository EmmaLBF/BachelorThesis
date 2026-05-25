
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
  if (isEmptyInt(v8)) {
    return v9;
  } else {
    int v10 = (headInt(v8));
    int v12 = (headInt(v9));
    NodeInt* v29 = NULL;
    if ((v10 < v12)) {
      v29 = consInt(v10, v7(tailInt(v8), v9));
    } else {
      v29 = consInt(v12, v7(tailInt(v9), v8));
    }
    return ((isEmptyInt(v9)) ? (v8) : (v29));
  }
}

Pair* v17(Pair* v18) {
  int v19 = *(int*)(fst(v18));
  NodeInt* v20 = (NodeInt*)(snd(v18));
  Pair* v55 = NULL;
  if ((v19 == 0)) {
    v55 = mk_pair(NULL, v20);
  } else {
    Pair* v23 = v17(mk_pair(box_int((v19 - 1)), tailInt(v20)));
    v55 = ((isEmptyInt(v20)) ? (mk_pair(NULL, NULL)) : (mk_pair(consInt((headInt(v20)), (NodeInt*)(fst(v23))), (NodeInt*)(snd(v23)))));
  }
  return v55;
}

int v24(NodeInt* v25) {
  if (isEmptyInt(v25)) {
    return 0;
  } else {
    return (1 + v24(tailInt(v25)));
  }
}

NodeInt* v0(NodeInt* v1) {
  if (isEmptyInt(v1)) {
    return NULL;
  } else {
    NodeInt* v3 = tailInt(v1);
    if (isEmptyInt(v3)) {
      return consInt((headInt(v1)), NULL);
    } else {
      NodeInt* v14 = consInt((headInt(v1)), v3);
      Pair* v6 = v17(mk_pair(box_int((v24(v14) / 2)), v14));
      return v7(v0((NodeInt*)(fst(v6))), v0((NodeInt*)(snd(v6))));
    }
  }
}

// main
int main(void) {
  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

