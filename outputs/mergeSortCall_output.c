
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v28(void* env, void* v13_raw);
Closure* v29(void* env, void* v12_raw);
Node* v31(void* env, void* v11_raw);
Closure* v32(void* env, void* v10_raw);
Node* v34(void* env, void* v9_raw);
Closure* v7(Node* v8);
Node* v36(Pair* v6);
Pair* v37(void* env, void* v23_raw);
Pair* v38(void* env, void* v22_raw);
Closure* v39(void* env, void* v21_raw);
Pair* v41(void* env, void* v20_raw);
Pair* v42(void* env, void* v19_raw);
Pair* v17(Pair* v18);
Pair* v43(void* env, void* v16_raw);
Pair* v44(void* env, void* v15_raw);
int v45(Node* v27);
int (*v46(int v26))(Node*);
int v24(Node* v25);
Pair* v49(Node* v14);
Node* v51(void* env, void* v5_raw);
Closure* v52(void* env, void* v4_raw);
Node* v54(void* env, void* v3_raw);
Closure* v55(void* env, void* v2_raw);
Node* v0(Node* v1);

// closure defitions
typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
    int v12;
} Env_v28;

typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
} Env_v29;

typedef struct {
    Node* v8;
    Node* v9;
    int v10;
} Env_v31;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v32;

typedef struct {
    Node* v8;
} Env_v34;

typedef struct {
    int v21;
} Env_v37;

typedef struct {
    int v19;
    int v21;
} Env_v38;

typedef struct {
    int v19;
} Env_v39;

typedef struct {
    int v19;
} Env_v41;

typedef struct {
    Pair* v18;
} Env_v42;

typedef struct {
    Node* v14;
} Env_v43;

typedef struct {
    Node* v14;
} Env_v44;

typedef struct {
    Node* v1;
} Env_v51;

typedef struct {
    Node* v1;
} Env_v52;

typedef struct {
    Node* v1;
    int v2;
} Env_v54;

typedef struct {
    Node* v1;
} Env_v55;

// function implementations
Node* v28(void* env, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  if (((Env_v28*)env)->v10 < ((Env_v28*)env)->v12) {
    return cons(box_int(((Env_v28*)env)->v10), apply((Closure*)v7(((Env_v28*)env)->v11), (void*)(((Env_v28*)env)->v9)));
  } else {
    return cons(box_int(((Env_v28*)env)->v12), apply((Closure*)v7(v13), (void*)(((Env_v28*)env)->v8)));
  }
}

Closure* v29(void* env, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v28* env28 = malloc(sizeof(Env_v28));
  env28->v12 = v12;
  env28->v8 = ((Env_v29*)env)->v8;
  env28->v9 = ((Env_v29*)env)->v9;
  env28->v10 = ((Env_v29*)env)->v10;
  env28->v11 = ((Env_v29*)env)->v11;
  Closure* c = malloc(sizeof(Closure));
  c->env = env28;
  c->fn = (void* (*)(void*, void*))v28;
  return c;
}

Node* v31(void* env, void* v11_raw) {
  Node* v11 = (Node*)v11_raw;
  Node* v30 = ((Env_v31*)env)->v9;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v11 = v11;
  env29->v8 = ((Env_v31*)env)->v8;
  env29->v9 = ((Env_v31*)env)->v9;
  env29->v10 = ((Env_v31*)env)->v10;
  Closure* c = malloc(sizeof(Closure));
  c->env = env29;
  c->fn = (void* (*)(void*, void*))v29;
  return (isEmpty(v30)) ? (((Env_v31*)env)->v8) : (apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v30)))), (void*)(tail(v30))));
}

Closure* v32(void* env, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v31* env31 = malloc(sizeof(Env_v31));
  env31->v10 = v10;
  env31->v8 = ((Env_v32*)env)->v8;
  env31->v9 = ((Env_v32*)env)->v9;
  Closure* c = malloc(sizeof(Closure));
  c->env = env31;
  c->fn = (void* (*)(void*, void*))v31;
  return c;
}

Node* v34(void* env, void* v9_raw) {
  Node* v9 = (Node*)v9_raw;
  Node* v33 = ((Env_v34*)env)->v8;
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v9 = v9;
  env32->v8 = ((Env_v34*)env)->v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env32;
  c->fn = (void* (*)(void*, void*))v32;
  return (isEmpty(v33)) ? (v9) : (apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v33)))), (void*)(tail(v33))));
}

Closure* v7(Node* v8) {
  Env_v34* env34 = malloc(sizeof(Env_v34));
  env34->v8 = v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env34;
  c->fn = (void* (*)(void*, void*))v34;
  return c;
}

Node* v36(Pair* v6) {
  Closure* v35 = v7(v0((Node*)fst(v6)));
  return apply((Closure*)v35, (void*)(v0((Node*)snd(v6))));
}

Pair* v37(void* env, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v37*)env)->v21), (Node*)fst(v23)), (Node*)snd(v23));
}

Pair* v38(void* env, void* v22_raw) {
  Node* v22 = (Node*)v22_raw;
  Env_v37* env37 = malloc(sizeof(Env_v37));
  env37->v21 = ((Env_v38*)env)->v21;
  Closure* c = malloc(sizeof(Closure));
  c->env = env37;
  c->fn = (void* (*)(void*, void*))v37;
  return (Pair*)apply((Closure*)c, (void*)(v17(mk_pair(box_int((((Env_v38*)env)->v19 - 1)), v22))));
}

Closure* v39(void* env, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v21 = v21;
  env38->v19 = ((Env_v39*)env)->v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env38;
  c->fn = (void* (*)(void*, void*))v38;
  return c;
}

Pair* v41(void* env, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if (((Env_v41*)env)->v19 == 0) {
    return mk_pair(NULL, v20);
  } else {
    Node* v40 = v20;
    Env_v39* env39 = malloc(sizeof(Env_v39));
    env39->v19 = ((Env_v41*)env)->v19;
    Closure* c = malloc(sizeof(Closure));
    c->env = env39;
    c->fn = (void* (*)(void*, void*))v39;
    return (isEmpty(v40)) ? (mk_pair(NULL, NULL)) : (apply((Closure*)(Pair*)apply((Closure*)c, box_int(*(int*)(head(v40)))), (void*)(tail(v40))));
  }
}

Pair* v42(void* env, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v41* env41 = malloc(sizeof(Env_v41));
  env41->v19 = v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env41;
  c->fn = (void* (*)(void*, void*))v41;
  return (Pair*)apply((Closure*)c, (void*)((Node*)snd(((Env_v42*)env)->v18)));
}

Pair* v17(Pair* v18) {
  Env_v42* env42 = malloc(sizeof(Env_v42));
  env42->v18 = v18;
  Closure* c = malloc(sizeof(Closure));
  c->env = env42;
  c->fn = (void* (*)(void*, void*))v42;
  return (Pair*)apply((Closure*)c, box_int(*(int*)fst(v18)));
}

Pair* v43(void* env, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v43*)env)->v14));
}

Pair* v44(void* env, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v43* env43 = malloc(sizeof(Env_v43));
  env43->v14 = ((Env_v44*)env)->v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env43;
  c->fn = (void* (*)(void*, void*))v43;
  return (Pair*)apply((Closure*)c, box_int((v15 / 2)));
}

int v45(Node* v27) {
  return (1 + v24(v27));
}

int (*v46(int v26))(Node*) {
  return v45;
}

int v24(Node* v25) {
  Node* v47 = v25;
  return (isEmpty(v47)) ? (0) : (v46(*(int*)(head(v47)))(tail(v47)));
}

Pair* v49(Node* v14) {
  int v48 = v24(v14);
  Env_v44* env44 = malloc(sizeof(Env_v44));
  env44->v14 = v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env44;
  c->fn = (void* (*)(void*, void*))v44;
  return (Pair*)apply((Closure*)c, box_int(v48));
}

Node* v51(void* env, void* v5_raw) {
  Node* v5 = (Node*)v5_raw;
  Pair* v50 = v49(((Env_v51*)env)->v1);
  return v36(v50);
}

Closure* v52(void* env, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v51* env51 = malloc(sizeof(Env_v51));
  env51->v1 = ((Env_v52*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env51;
  c->fn = (void* (*)(void*, void*))v51;
  return c;
}

Node* v54(void* env, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  Node* v53 = v3;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v1 = ((Env_v54*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env52;
  c->fn = (void* (*)(void*, void*))v52;
  return (isEmpty(v53)) ? (cons(box_int(((Env_v54*)env)->v2), NULL)) : (apply((Closure*)(Node*)apply((Closure*)c, box_int(*(int*)(head(v53)))), (void*)(tail(v53))));
}

Closure* v55(void* env, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v2 = v2;
  env54->v1 = ((Env_v55*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env54;
  c->fn = (void* (*)(void*, void*))v54;
  return c;
}

Node* v0(Node* v1) {
  Node* v56 = v1;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env55;
  c->fn = (void* (*)(void*, void*))v55;
  return (isEmpty(v56)) ? (NULL) : (apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v56)))), (void*)(tail(v56))));
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

