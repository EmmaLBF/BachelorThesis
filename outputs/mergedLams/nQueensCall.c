
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
int v40(Node* v2, Node* v3);
int v0(Node* v1);
Node* v44(Node* (*(*v5)(int))(Node*));
Node* v50(void* env50, void* v18_raw, void* v19_raw);
Node* v15(Node* v16, Node* v17);
bool v54(void* env54, void* v37_raw);
bool v56(void* env56, void* v36_raw);
bool v58(void* env58, void* v35_raw);
Closure* v59(void* env59, void* v34_raw);
Closure* v61(void* env61, void* v33_raw);
Closure* v63(Pair_Int_Int *v32);
bool v70(void* env70, void* v30_raw, void* v31_raw);
bool v27(Pair_Int_Int *v28, Node* v29);
Node* v76(void* env76, void* v26_raw);
Node* v82(void* env82, void* v25_raw);
Node* v20(int v21, int v22, Node* v23, int v24);
Node* v97(void* env97, void* v13_raw, void* v14_raw);
Node* v9(int v10, int v11, Node* v12);
Node* v6(void* env6, void* v7_raw, void* v8_raw);
Node* v107(int v4);

// closure defitions
typedef struct {
    Node* v2;
    Node* v3;
} Env_v40;

typedef struct {
    Node* v1;
} Env_v0;

typedef struct {
    Node* (*(*v5)(int))(Node*);
} Env_v44;

typedef struct {
    Node* v18;
    Node* v19;
    Node* v17;
} Env_v50;

typedef struct {
    Node* v16;
    Node* v17;
} Env_v15;

typedef struct {
    int v37;
    int v33;
    int v34;
    int v36;
} Env_v54;

typedef struct {
    int v36;
    int v33;
    int v34;
    Pair_Int_Int *v35;
} Env_v56;

typedef struct {
    Pair_Int_Int *v35;
    int v33;
    int v34;
} Env_v58;

typedef struct {
    int v34;
    int v33;
} Env_v59;

typedef struct {
    int v33;
    Pair_Int_Int *v32;
} Env_v61;

typedef struct {
    Pair_Int_Int *v32;
} Env_v63;

typedef struct {
    Pair_Int_Int *v30;
    Node* v31;
    Pair_Int_Int *v28;
} Env_v70;

typedef struct {
    Pair_Int_Int *v28;
    Node* v29;
} Env_v27;

typedef struct {
    Node* v26;
    Node* v23;
    Pair_Int_Int *v25;
} Env_v76;

typedef struct {
    Pair_Int_Int *v25;
    int v21;
    int v22;
    Node* v23;
    int v24;
} Env_v82;

typedef struct {
    int v21;
    int v22;
    Node* v23;
    int v24;
} Env_v20;

typedef struct {
    Node* v13;
    Node* v14;
    int v10;
    int v11;
} Env_v97;

typedef struct {
    int v10;
    int v11;
    Node* v12;
} Env_v9;

typedef struct {
    int v7;
    Node* v8;
    int v4;
} Env_v6;

typedef struct {
    int v4;
} Env_v107;

// function implementations
int v40(Node* v2, Node* v3) {
  return (1 + v0(v3));
}

int v0(Node* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return v40((v1)->head, (v1)->tail);
  }
}

Node* v44(Node* (*(*v5)(int))(Node*)) {
  return v5(0)(cons(NULL, NULL));
}

Node* v50(void* env50, void* v18_raw, void* v19_raw) {
  Node* v18 = (Node*)v18_raw;
  Node* v19 = (Node*)v19_raw;
  return cons(v18, v15(v19, ((Env_v50*)env50)->v17));
}

Node* v15(Node* v16, Node* v17) {
  if (((v16) == NULL)) {
    return v17;
  } else {
    Env_v50* env50 = malloc(sizeof(Env_v50));
    env50->v17 = v17;
    Closure* c50 = malloc(sizeof(Closure));
    c50->env = env50;
    c50->fn = (void* (*)(void*, void*))v50;
    return (Node*)((Closure*)c50)->fn(((Closure*)c50)->env, (v16)->head, (v16)->tail);
  }
}

bool v54(void* env54, void* v37_raw) {
  int v37 = *(int*)v37_raw;
  return ((((Env_v54*)env54)->v34 == v37) || (abs((((Env_v54*)env54)->v34 - v37)) == abs((((Env_v54*)env54)->v33 - ((Env_v54*)env54)->v36))));
}

bool v56(void* env56, void* v36_raw) {
  int v36 = *(int*)v36_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v36 = v36;
  env54->v33 = ((Env_v56*)env56)->v33;
  env54->v34 = ((Env_v56*)env56)->v34;
  Closure* c54 = malloc(sizeof(Closure));
  c54->env = env54;
  c54->fn = (void* (*)(void*, void*))v54;
  return (bool)(intptr_t)((Closure*)c54)->fn(((Closure*)c54)->env, box_int((((Env_v56*)env56)->v35)->snd));
}

bool v58(void* env58, void* v35_raw) {
  Pair_Int_Int *v35 = (Pair_Int_Int*)v35_raw;
  Env_v56* env56 = malloc(sizeof(Env_v56));
  env56->v35 = v35;
  env56->v33 = ((Env_v58*)env58)->v33;
  env56->v34 = ((Env_v58*)env58)->v34;
  Closure* c56 = malloc(sizeof(Closure));
  c56->env = env56;
  c56->fn = (void* (*)(void*, void*))v56;
  return (bool)(intptr_t)((Closure*)c56)->fn(((Closure*)c56)->env, box_int((v35)->fst));
}

Closure* v59(void* env59, void* v34_raw) {
  int v34 = *(int*)v34_raw;
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v34 = v34;
  env58->v33 = ((Env_v59*)env59)->v33;
  Closure* c58 = malloc(sizeof(Closure));
  c58->env = env58;
  c58->fn = (void* (*)(void*, void*))v58;
  return c58;
}

Closure* v61(void* env61, void* v33_raw) {
  int v33 = *(int*)v33_raw;
  Env_v59* env59 = malloc(sizeof(Env_v59));
  env59->v33 = v33;
  Closure* c59 = malloc(sizeof(Closure));
  c59->env = env59;
  c59->fn = (void* (*)(void*, void*))v59;
  return (Closure*)((Closure*)c59)->fn(((Closure*)c59)->env, box_int((((Env_v61*)env61)->v32)->snd));
}

Closure* v63(Pair_Int_Int *v32) {
  Env_v61* env61 = malloc(sizeof(Env_v61));
  env61->v32 = v32;
  Closure* c61 = malloc(sizeof(Closure));
  c61->env = env61;
  c61->fn = (void* (*)(void*, void*))v61;
  return (Closure*)((Closure*)c61)->fn(((Closure*)c61)->env, box_int((v32)->fst));
}

bool v70(void* env70, void* v30_raw, void* v31_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Node* v31 = (Node*)v31_raw;
  return (!((Closure*)((Closure*)v63(((Env_v70*)env70)->v28))->fn(((Closure*)v63(((Env_v70*)env70)->v28))->env, v30)) && v27(((Env_v70*)env70)->v28, v31));
}

bool v27(Pair_Int_Int *v28, Node* v29) {
  if (((v29) == NULL)) {
    return true;
  } else {
    Env_v70* env70 = malloc(sizeof(Env_v70));
    env70->v28 = v28;
    Closure* c70 = malloc(sizeof(Closure));
    c70->env = env70;
    c70->fn = (void* (*)(void*, void*))v70;
    return (bool)(intptr_t)((Closure*)c70)->fn(((Closure*)c70)->env, (v29)->head, (v29)->tail);
  }
}

Node* v76(void* env76, void* v26_raw) {
  Node* v26 = (Node*)v26_raw;
  if (v27(((Env_v76*)env76)->v25, ((Env_v76*)env76)->v23)) {
    return cons(cons(((Env_v76*)env76)->v25, ((Env_v76*)env76)->v23), v26);
  } else {
    return v26;
  }
}

Node* v82(void* env82, void* v25_raw) {
  Pair_Int_Int *v25 = (Pair_Int_Int*)v25_raw;
  Env_v76* env76 = malloc(sizeof(Env_v76));
  env76->v25 = v25;
  env76->v23 = ((Env_v82*)env82)->v23;
  Closure* c76 = malloc(sizeof(Closure));
  c76->env = env76;
  c76->fn = (void* (*)(void*, void*))v76;
  return (Node*)((Closure*)c76)->fn(((Closure*)c76)->env, v20(((Env_v82*)env82)->v21, ((Env_v82*)env82)->v22, ((Env_v82*)env82)->v23, (((Env_v82*)env82)->v24 + 1)));
}

Node* v20(int v21, int v22, Node* v23, int v24) {
  if ((v24 == v21)) {
    return NULL;
  } else {
    Env_v82* env82 = malloc(sizeof(Env_v82));
    env82->v21 = v21;
    env82->v22 = v22;
    env82->v23 = v23;
    env82->v24 = v24;
    Closure* c82 = malloc(sizeof(Closure));
    c82->env = env82;
    c82->fn = (void* (*)(void*, void*))v82;
    return (Node*)((Closure*)c82)->fn(((Closure*)c82)->env, makePair_Int_Int(v22, v24));
  }
}

Node* v97(void* env97, void* v13_raw, void* v14_raw) {
  Node* v13 = (Node*)v13_raw;
  Node* v14 = (Node*)v14_raw;
  return v15(v20(((Env_v97*)env97)->v10, ((Env_v97*)env97)->v11, v13, 0), v9(((Env_v97*)env97)->v10, ((Env_v97*)env97)->v11, v14));
}

Node* v9(int v10, int v11, Node* v12) {
  if (((v12) == NULL)) {
    return NULL;
  } else {
    Env_v97* env97 = malloc(sizeof(Env_v97));
    env97->v10 = v10;
    env97->v11 = v11;
    Closure* c97 = malloc(sizeof(Closure));
    c97->env = env97;
    c97->fn = (void* (*)(void*, void*))v97;
    return (Node*)((Closure*)c97)->fn(((Closure*)c97)->env, (v12)->head, (v12)->tail);
  }
}

Node* v6(void* env6, void* v7_raw, void* v8_raw) {
  int v7 = *(int*)v7_raw;
  Node* v8 = (Node*)v8_raw;
  if ((v7 == ((Env_v6*)env6)->v4)) {
    return v8;
  } else {
    Env_v6* env6 = malloc(sizeof(Env_v6));
    env6->v4 = ((Env_v6*)env6)->v4;
    Closure* c6 = malloc(sizeof(Closure));
    c6->env = env6;
    c6->fn = (void* (*)(void*, void*))v6;
    return (Node*)((Closure*)c6)->fn(((Closure*)c6)->env, box_int((v7 + 1)), v9(((Env_v6*)env6)->v4, v7, v8));
  }
}

Node* v107(int v4) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v4 = v4;
  Closure* c6 = malloc(sizeof(Closure));
  c6->env = env6;
  c6->fn = (void* (*)(void*, void*))v6;
  return v44(c6);
}

// main
int main(void) {
  printInt(v0(v107(4)));
  return 0;
}

