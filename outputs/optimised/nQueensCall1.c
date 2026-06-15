
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
Node* v16(Node* v17, Node* v18);
bool v30(Pair_Int_Int *v31, Node* v32);
Node* v21(int v22, int v23, Node* v24, int v25);
Node* v10(int v11, int v12, Node* v13);
Node* v7(void* env7, void* v8_raw, void* v9_raw);

// closure defitions
typedef struct {
    int v5;
} Env_v7;

typedef struct {
} Env_v107;

// function implementations
int v0(Node* v1) {
  if (((v1) == NULL)) return 0;
  return (1 + v0((v1)->tail));
}

Node* v16(Node* v17, Node* v18) {
  if (((v17) == NULL)) return v18;
  return cons((v17)->head, v16((v17)->tail, v18));
}

bool v30(Pair_Int_Int *v31, Node* v32) {
  if (((v32) == NULL)) return true;
  Pair_Int_Int v40 = *(Pair_Int_Int*)((v32)->head);
  return (!((((v31)->snd == (v40).snd) || (abs(((v31)->snd - (v40).snd)) == abs(((v31)->fst - (v40).fst))))) && v30(v31, (v32)->tail));
}

Node* v21(int v22, int v23, Node* v24, int v25) {
  if ((v25 == v22)) return NULL;
  Pair_Int_Int *v27 = makePair_Int_Int(v23, v25);
  Node* v75 = NULL;
  if (v30(v27, v24)) {
    v75 = cons(cons(v27, v24), v21(v22, v23, v24, (v25 + 1)));
  } else {
    v75 = v21(v22, v23, v24, (v25 + 1));
  }
  return v75;
}

Node* v10(int v11, int v12, Node* v13) {
  if (((v13) == NULL)) return NULL;
  return v16(v21(v11, v12, (v13)->head, 0), v10(v11, v12, (v13)->tail));
}

Node* v7(void* env7, void* v8_raw, void* v9_raw) {
  int v8 = *(int*)v8_raw;
  Node* v9 = (Node*)v9_raw;
  if ((v8 == ((Env_v7*)env7)->v5)) return v9;
  return v7(env7, box_int((v8 + 1)), (void*)(v10(((Env_v7*)env7)->v5, v8, v9)));
}

// main
int main(void) {
  int v5 = 4;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v5 = v5;
  printInt(v0(v7(env7, box_int(0), (void*)(cons(NULL, NULL)))));
  return 0;
}

