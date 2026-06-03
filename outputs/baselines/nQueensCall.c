
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
int v39(Node* v3);
int (*v40(Node* v2))(Node*);
int v0(Node* v1);
Node* v44(Node* (*(*v5)(int))(Node*));
Node* v49(void* env49, void* v19_raw);
Closure* v50(void* env50, void* v18_raw);
Node* v52(void* env52, void* v17_raw);
Closure* v15(Node* v16);
bool v54(void* env54, void* v37_raw);
bool v56(void* env56, void* v36_raw);
bool v58(void* env58, void* v35_raw);
Closure* v59(void* env59, void* v34_raw);
Closure* v61(void* env61, void* v33_raw);
Closure* v63(Pair_Int_Int *v32);
bool v69(void* env69, void* v31_raw);
Closure* v70(void* env70, void* v30_raw);
bool v72(void* env72, void* v29_raw);
Closure* v27(Pair_Int_Int *v28);
Node* v76(void* env76, void* v26_raw);
Node* v82(void* env82, void* v25_raw);
Node* v84(void* env84, void* v24_raw);
Closure* v85(void* env85, void* v23_raw);
Closure* v86(void* env86, void* v22_raw);
Closure* v20(int v21);
Closure* v96(void* env96, void* v14_raw);
Closure* v97(void* env97, void* v13_raw);
Node* v99(void* env99, void* v12_raw);
Closure* v100(void* env100, void* v11_raw);
Closure* v9(int v10);
Node* v105(void* env105, void* v8_raw);
Closure* v6(void* env6, void* v7_raw);
Node* v107(int v4);

// closure defitions
typedef struct {
    Node* v3;
} Env_v39;

typedef struct {
    Node* v2;
} Env_v40;

typedef struct {
    Node* v1;
} Env_v0;

typedef struct {
    Node* (*(*v5)(int))(Node*);
} Env_v44;

typedef struct {
    Node* v19;
    Node* v17;
    Node* v18;
} Env_v49;

typedef struct {
    Node* v18;
    Node* v17;
} Env_v50;

typedef struct {
    Node* v17;
    Node* v16;
} Env_v52;

typedef struct {
    Node* v16;
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
    Node* v31;
    Pair_Int_Int *v28;
    Pair_Int_Int *v30;
} Env_v69;

typedef struct {
    Pair_Int_Int *v30;
    Pair_Int_Int *v28;
} Env_v70;

typedef struct {
    Node* v29;
    Pair_Int_Int *v28;
} Env_v72;

typedef struct {
    Pair_Int_Int *v28;
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
    int v24;
    int v21;
    int v22;
    Node* v23;
} Env_v84;

typedef struct {
    Node* v23;
    int v21;
    int v22;
} Env_v85;

typedef struct {
    int v22;
    int v21;
} Env_v86;

typedef struct {
    int v21;
} Env_v20;

typedef struct {
    Node* v14;
    int v10;
    int v11;
    Node* v13;
} Env_v96;

typedef struct {
    Node* v13;
    int v10;
    int v11;
} Env_v97;

typedef struct {
    Node* v12;
    int v10;
    int v11;
} Env_v99;

typedef struct {
    int v11;
    int v10;
} Env_v100;

typedef struct {
    int v10;
} Env_v9;

typedef struct {
    Node* v8;
    int v4;
    int v7;
} Env_v105;

typedef struct {
    int v7;
    int v4;
} Env_v6;

typedef struct {
    int v4;
} Env_v107;

// function implementations
int v39(Node* v3) {
  return (1 + v0(v3));
}

int (*v40(Node* v2))(Node*) {
  return v39;
}

int v0(Node* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return v40((v1)->head)((v1)->tail);
  }
}

Node* v44(Node* (*(*v5)(int))(Node*)) {
  return v5(0)(cons(NULL, NULL));
}

Node* v49(void* env49, void* v19_raw) {
  Node* v19 = (Node*)v19_raw;
  return cons(((Env_v49*)env49)->v18, (Node*)((Closure*)v15(v19))->fn(((Closure*)v15(v19))->env, ((Env_v49*)env49)->v17));
}

Closure* v50(void* env50, void* v18_raw) {
  Node* v18 = (Node*)v18_raw;
  Env_v49* env49 = malloc(sizeof(Env_v49));
  env49->v18 = v18;
  env49->v17 = ((Env_v50*)env50)->v17;
  Closure* c49 = malloc(sizeof(Closure));
  c49->env = env49;
  c49->fn = (void* (*)(void*, void*))v49;
  return c49;
}

Node* v52(void* env52, void* v17_raw) {
  Node* v17 = (Node*)v17_raw;
  if (((((Env_v52*)env52)->v16) == NULL)) {
    return v17;
  } else {
    Env_v50* env50 = malloc(sizeof(Env_v50));
    env50->v17 = v17;
    Closure* c50 = malloc(sizeof(Closure));
    c50->env = env50;
    c50->fn = (void* (*)(void*, void*))v50;
    return (Node*)((Closure*)(Closure*)((Closure*)c50)->fn(((Closure*)c50)->env, (((Env_v52*)env52)->v16)->head))->fn(((Closure*)(Closure*)((Closure*)c50)->fn(((Closure*)c50)->env, (((Env_v52*)env52)->v16)->head))->env, (((Env_v52*)env52)->v16)->tail);
  }
}

Closure* v15(Node* v16) {
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v16 = v16;
  Closure* c52 = malloc(sizeof(Closure));
  c52->env = env52;
  c52->fn = (void* (*)(void*, void*))v52;
  return c52;
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

bool v69(void* env69, void* v31_raw) {
  Node* v31 = (Node*)v31_raw;
  return (!((Closure*)((Closure*)v63(((Env_v69*)env69)->v28))->fn(((Closure*)v63(((Env_v69*)env69)->v28))->env, ((Env_v69*)env69)->v30)) && (bool)(intptr_t)((Closure*)v27(((Env_v69*)env69)->v28))->fn(((Closure*)v27(((Env_v69*)env69)->v28))->env, v31));
}

Closure* v70(void* env70, void* v30_raw) {
  Pair_Int_Int *v30 = (Pair_Int_Int*)v30_raw;
  Env_v69* env69 = malloc(sizeof(Env_v69));
  env69->v30 = v30;
  env69->v28 = ((Env_v70*)env70)->v28;
  Closure* c69 = malloc(sizeof(Closure));
  c69->env = env69;
  c69->fn = (void* (*)(void*, void*))v69;
  return c69;
}

bool v72(void* env72, void* v29_raw) {
  Node* v29 = (Node*)v29_raw;
  if (((v29) == NULL)) {
    return true;
  } else {
    Env_v70* env70 = malloc(sizeof(Env_v70));
    env70->v28 = ((Env_v72*)env72)->v28;
    Closure* c70 = malloc(sizeof(Closure));
    c70->env = env70;
    c70->fn = (void* (*)(void*, void*))v70;
    return (bool)(intptr_t)((Closure*)(Closure*)((Closure*)c70)->fn(((Closure*)c70)->env, (v29)->head))->fn(((Closure*)(Closure*)((Closure*)c70)->fn(((Closure*)c70)->env, (v29)->head))->env, (v29)->tail);
  }
}

Closure* v27(Pair_Int_Int *v28) {
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v28 = v28;
  Closure* c72 = malloc(sizeof(Closure));
  c72->env = env72;
  c72->fn = (void* (*)(void*, void*))v72;
  return c72;
}

Node* v76(void* env76, void* v26_raw) {
  Node* v26 = (Node*)v26_raw;
  if ((bool)(intptr_t)((Closure*)v27(((Env_v76*)env76)->v25))->fn(((Closure*)v27(((Env_v76*)env76)->v25))->env, ((Env_v76*)env76)->v23)) {
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
  return (Node*)((Closure*)c76)->fn(((Closure*)c76)->env, (Node* (*)(int))((Closure*)(Node* (*)(int))((Closure*)(Node* (*)(int))((Closure*)v20(((Env_v82*)env82)->v21))->fn(((Closure*)v20(((Env_v82*)env82)->v21))->env, box_int(((Env_v82*)env82)->v22)))->fn(((Closure*)(Node* (*)(int))((Closure*)v20(((Env_v82*)env82)->v21))->fn(((Closure*)v20(((Env_v82*)env82)->v21))->env, box_int(((Env_v82*)env82)->v22)))->env, ((Env_v82*)env82)->v23))->fn(((Closure*)(Node* (*)(int))((Closure*)(Node* (*)(int))((Closure*)v20(((Env_v82*)env82)->v21))->fn(((Closure*)v20(((Env_v82*)env82)->v21))->env, box_int(((Env_v82*)env82)->v22)))->fn(((Closure*)(Node* (*)(int))((Closure*)v20(((Env_v82*)env82)->v21))->fn(((Closure*)v20(((Env_v82*)env82)->v21))->env, box_int(((Env_v82*)env82)->v22)))->env, ((Env_v82*)env82)->v23))->env, box_int((((Env_v82*)env82)->v24 + 1))));
}

Node* v84(void* env84, void* v24_raw) {
  int v24 = *(int*)v24_raw;
  if ((v24 == ((Env_v84*)env84)->v21)) {
    return NULL;
  } else {
    Env_v82* env82 = malloc(sizeof(Env_v82));
    env82->v24 = v24;
    env82->v21 = ((Env_v84*)env84)->v21;
    env82->v22 = ((Env_v84*)env84)->v22;
    env82->v23 = ((Env_v84*)env84)->v23;
    Closure* c82 = malloc(sizeof(Closure));
    c82->env = env82;
    c82->fn = (void* (*)(void*, void*))v82;
    return (Node*)((Closure*)c82)->fn(((Closure*)c82)->env, makePair_Int_Int(((Env_v84*)env84)->v22, v24));
  }
}

Closure* v85(void* env85, void* v23_raw) {
  Node* v23 = (Node*)v23_raw;
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v23 = v23;
  env84->v21 = ((Env_v85*)env85)->v21;
  env84->v22 = ((Env_v85*)env85)->v22;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

Closure* v86(void* env86, void* v22_raw) {
  int v22 = *(int*)v22_raw;
  Env_v85* env85 = malloc(sizeof(Env_v85));
  env85->v22 = v22;
  env85->v21 = ((Env_v86*)env86)->v21;
  Closure* c85 = malloc(sizeof(Closure));
  c85->env = env85;
  c85->fn = (void* (*)(void*, void*))v85;
  return c85;
}

Closure* v20(int v21) {
  Env_v86* env86 = malloc(sizeof(Env_v86));
  env86->v21 = v21;
  Closure* c86 = malloc(sizeof(Closure));
  c86->env = env86;
  c86->fn = (void* (*)(void*, void*))v86;
  return c86;
}

Closure* v96(void* env96, void* v14_raw) {
  Node* v14 = (Node*)v14_raw;
  return (Node*)((Closure*)v15((Node*)((Closure*)(Node*)((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->fn(((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->env, ((Env_v96*)env96)->v13))->fn(((Closure*)(Node*)((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->fn(((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->env, ((Env_v96*)env96)->v13))->env, box_int(0))))->fn(((Closure*)v15((Node*)((Closure*)(Node*)((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->fn(((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->env, ((Env_v96*)env96)->v13))->fn(((Closure*)(Node*)((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->fn(((Closure*)(Node*)((Closure*)v20(((Env_v96*)env96)->v10))->fn(((Closure*)v20(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->env, ((Env_v96*)env96)->v13))->env, box_int(0))))->env, (Node*)((Closure*)(Node*)((Closure*)v9(((Env_v96*)env96)->v10))->fn(((Closure*)v9(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->fn(((Closure*)(Node*)((Closure*)v9(((Env_v96*)env96)->v10))->fn(((Closure*)v9(((Env_v96*)env96)->v10))->env, box_int(((Env_v96*)env96)->v11)))->env, v14));
}

Closure* v97(void* env97, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  Env_v96* env96 = malloc(sizeof(Env_v96));
  env96->v13 = v13;
  env96->v10 = ((Env_v97*)env97)->v10;
  env96->v11 = ((Env_v97*)env97)->v11;
  Closure* c96 = malloc(sizeof(Closure));
  c96->env = env96;
  c96->fn = (void* (*)(void*, void*))v96;
  return c96;
}

Node* v99(void* env99, void* v12_raw) {
  Node* v12 = (Node*)v12_raw;
  if (((v12) == NULL)) {
    return NULL;
  } else {
    Env_v97* env97 = malloc(sizeof(Env_v97));
    env97->v10 = ((Env_v99*)env99)->v10;
    env97->v11 = ((Env_v99*)env99)->v11;
    Closure* c97 = malloc(sizeof(Closure));
    c97->env = env97;
    c97->fn = (void* (*)(void*, void*))v97;
    return (Closure*)((Closure*)(Closure*)((Closure*)c97)->fn(((Closure*)c97)->env, (v12)->head))->fn(((Closure*)(Closure*)((Closure*)c97)->fn(((Closure*)c97)->env, (v12)->head))->env, (v12)->tail);
  }
}

Closure* v100(void* env100, void* v11_raw) {
  int v11 = *(int*)v11_raw;
  Env_v99* env99 = malloc(sizeof(Env_v99));
  env99->v11 = v11;
  env99->v10 = ((Env_v100*)env100)->v10;
  Closure* c99 = malloc(sizeof(Closure));
  c99->env = env99;
  c99->fn = (void* (*)(void*, void*))v99;
  return c99;
}

Closure* v9(int v10) {
  Env_v100* env100 = malloc(sizeof(Env_v100));
  env100->v10 = v10;
  Closure* c100 = malloc(sizeof(Closure));
  c100->env = env100;
  c100->fn = (void* (*)(void*, void*))v100;
  return c100;
}

Node* v105(void* env105, void* v8_raw) {
  Node* v8 = (Node*)v8_raw;
  if ((((Env_v105*)env105)->v7 == ((Env_v105*)env105)->v4)) {
    return v8;
  } else {
    Env_v6* env6 = malloc(sizeof(Env_v6));
    env6->v4 = ((Env_v105*)env105)->v4;
    Closure* c6 = malloc(sizeof(Closure));
    c6->env = env6;
    c6->fn = (void* (*)(void*, void*))v6;
    return (Node*)((Closure*)(Closure*)((Closure*)c6)->fn(((Closure*)c6)->env, box_int((((Env_v105*)env105)->v7 + 1))))->fn(((Closure*)(Closure*)((Closure*)c6)->fn(((Closure*)c6)->env, box_int((((Env_v105*)env105)->v7 + 1))))->env, (Node*)((Closure*)(Node*)((Closure*)v9(((Env_v105*)env105)->v4))->fn(((Closure*)v9(((Env_v105*)env105)->v4))->env, box_int(((Env_v105*)env105)->v7)))->fn(((Closure*)(Node*)((Closure*)v9(((Env_v105*)env105)->v4))->fn(((Closure*)v9(((Env_v105*)env105)->v4))->env, box_int(((Env_v105*)env105)->v7)))->env, v8));
  }
}

Closure* v6(void* env6, void* v7_raw) {
  int v7 = *(int*)v7_raw;
  Env_v105* env105 = malloc(sizeof(Env_v105));
  env105->v7 = v7;
  env105->v4 = ((Env_v6*)env6)->v4;
  Closure* c105 = malloc(sizeof(Closure));
  c105->env = env105;
  c105->fn = (void* (*)(void*, void*))v105;
  return c105;
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

