
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
NodeInt* v7(void* env7, void* v8_raw, void* v9_raw);
Pair_NodeInt_NodeInt v17(void* env17, void* v18_raw);
int v24(void* env24, void* v25_raw);
NodeInt* v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v8;
    NodeInt* v9;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    Pair_NodeInt_NodeInt *v6;
} Env_v7;

typedef struct {
    Pair_Int_NodeInt v18;
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
    NodeInt* v25;
    NodeInt* v1;
    int v2;
    NodeInt* v3;
    int v4;
    NodeInt* v5;
    NodeInt* v14;
} Env_v24;

// function implementations
// main
int main(void) {
  NodeInt* v7(void* env7, void* v8_raw, void* v9_raw) {
    NodeInt* v8 = (NodeInt*)v8_raw;
    NodeInt* v9 = (NodeInt*)v9_raw;
    if (((v8) == NULL)) {
      return v9;
    } else {
      NodeInt* v36 = NULL;
      if (((v9) == NULL)) {
        v36 = v8;
      } else {
        NodeInt* v33 = NULL;
        if (((v8)->head < (v9)->head)) {
          v33 = consInt((v8)->head, v7(env7, (void*)((v8)->tail), (void*)(v9)));
        } else {
          v33 = consInt((v9)->head, v7(env7, (void*)((v9)->tail), (void*)(v8)));
        }
        v36 = v33;
      }
      return v36;
    }
  }

  Pair_NodeInt_NodeInt v17(void* env17, void* v18_raw) {
    Pair_Int_NodeInt v18 = (Pair_Int_NodeInt)v18_raw;
    Pair_NodeInt_NodeInt v51 = { .fst = NULL, .snd = NULL};
    if (((v18).fst == 0)) {
      v51 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = (v18).snd };
    } else {
      if ((((v18).snd) == NULL)) {
        v51 = (Pair_NodeInt_NodeInt){ .fst = NULL, .snd = NULL };
      } else {
        Pair_NodeInt_NodeInt v23 = v17(env17, (void*)(makePair_Int_NodeInt(((v18).fst - 1), ((v18).snd)->tail)));
        v51 = (Pair_NodeInt_NodeInt){ .fst = consInt(((v18).snd)->head, (v23).fst), .snd = (v23).snd };
      }
    }
    return v51;
  }

  int v24(void* env24, void* v25_raw) {
    NodeInt* v25 = (NodeInt*)v25_raw;
    if (((v25) == NULL)) {
      return 0;
    } else {
      Env_v24* env24 = malloc(sizeof(Env_v24));
      env24->v1 = v1;
      env24->v2 = v2;
      env24->v3 = v3;
      env24->v4 = v4;
      env24->v5 = v5;
      env24->v14 = v14;
      return (1 + v24(env24, (void*)((v25)->tail)));
    }
  }

  NodeInt* v0(NodeInt* v1) {
    if (((v1) == NULL)) {
      return NULL;
    } else {
      NodeInt* v73 = NULL;
      if ((((v1)->tail) == NULL)) {
        v73 = consInt((v1)->head, NULL);
      } else {
        Pair_NodeInt_NodeInt v6 = v17(env17, (void*)(makePair_Int_NodeInt((v24(env24, (void*)(v1)) / 2), v1)));
        v73 = v7(env7, (void*)(v0((v6).fst)), (void*)(v0((v6).snd)));
      }
      return v73;
    }
  }

  printListInt(v0(consInt(4, consInt(6, consInt(3, NULL)))));
  return 0;
}

