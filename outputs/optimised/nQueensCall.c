
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

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
int v0(Node* v1);
Node* v14(Node* v15, Node* v16);
bool v54(void* env54, void* v34_raw);
bool v26(Pair_Int_Int *v27, Node* v28);
Node* v19(int v20, int v21, Node* v22, int v23);
Node* v8(int v9, int v10, Node* v11);
Node* v5(void* env5, void* v6_raw, void* v7_raw);

// closure defitions
typedef struct {
    int v4;
} Env_v5;

typedef struct {
    int v32;
    int v33;
    int v35;
} Env_v50;

typedef struct {
    int v32;
    int v33;
    Pair_Int_Int *v34;
} Env_v52;

typedef struct {
    int v32;
    int v33;
} Env_v54;

// function implementations
int v0(Node* v1) {
  if (((v1) == NULL)) return 0;
  return (1 + v0((v1)->tail));
}

Node* v14(Node* v15, Node* v16) {
  if (((v15) == NULL)) return v16;
  return cons((v15)->head, v14((v15)->tail, v16));
}

bool v54(void* env54, void* v34_raw) {
  Pair_Int_Int *v34 = (Pair_Int_Int*)v34_raw;
  return ((((Env_v50*)env54)->v33 == (v34)->snd) || (abs((((Env_v50*)env54)->v33 - (v34)->snd)) == abs((((Env_v50*)env54)->v32 - (v34)->fst))));
}

bool v26(Pair_Int_Int *v27, Node* v28) {
  if (((v28) == NULL)) return true;
  Closure* c54 = malloc(sizeof(Closure));
  c54->env = env54;
  c54->fn = (void* (*)(void*, void*))v54;
  return (!((Closure*)(c54)->fn((c54)->env, (v28)->head)) && v26(v27, (v28)->tail));
}

Node* v19(int v20, int v21, Node* v22, int v23) {
  if ((v23 == v20)) return NULL;
  Pair_Int_Int *v24 = makePair_Int_Int(v21, v23);
  Node* v72 = NULL;
  if (v26(v24, v22)) {
    v72 = cons(cons(v24, v22), v19(v20, v21, v22, (v23 + 1)));
  } else {
    v72 = v19(v20, v21, v22, (v23 + 1));
  }
  return v72;
}

Node* v8(int v9, int v10, Node* v11) {
  if (((v11) == NULL)) return NULL;
  return v14(v19(v9)(v10)((v11)->head)(0), v8(v9)(v10)((v11)->tail));
}

Node* v5(void* env5, void* v6_raw, void* v7_raw) {
  int v6 = *(int*)v6_raw;
  Node* v7 = (Node*)v7_raw;
  if ((v6 == ((Env_v5*)env5)->v4)) return v7;
  return v5(env5, box_int((v6 + 1)), (void*)(v8(((Env_v5*)env5)->v4, v6, v7)));
}

// main
int main(void) {
  int v4 = 4;
  printInt(v0(v5(env5)(0)(cons(NULL, NULL))));
  return 0;
}

