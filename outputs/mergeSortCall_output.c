
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v28(void* env, void* v13_raw);
Closure* v29(void* env, void* v12_raw);
Node* v32(void* env, void* v11_raw);
Closure* v33(void* env, void* v10_raw);
Node* v36(void* env, void* v9_raw);
Closure* v7(Node* v8);
Node* v45(Pair* v6);
Pair* v46(void* env, void* v23_raw);
Pair* v51(void* env, void* v22_raw);
Closure* v52(void* env, void* v21_raw);
Pair* v55(void* env, void* v20_raw);
Pair* v58(void* env, void* v19_raw);
Pair* v17(Pair* v18);
Pair* v63(void* env, void* v16_raw);
Pair* v66(void* env, void* v15_raw);
int v67(Node* v27);
int (*v68(int v26))(Node*);
int v24(Node* v25);
Pair* v75(Node* v14);
Node* v80(void* env, void* v5_raw);
Closure* v81(void* env, void* v4_raw);
Node* v84(void* env, void* v3_raw);
Closure* v85(int v2);
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
} Env_v32;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v33;

typedef struct {
    Node* v8;
} Env_v36;

typedef struct {
    int v21;
} Env_v46;

typedef struct {
    int v19;
    int v21;
} Env_v51;

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
    Node* v14;
} Env_v63;

typedef struct {
    Node* v14;
} Env_v66;

typedef struct {
    int v2;
    int v4;
} Env_v80;

typedef struct {
    int v2;
} Env_v81;

typedef struct {
    int v2;
} Env_v84;

// function implementations
Node* v28(void* env, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  if (((Env_v28*)env)->v10 < ((Env_v28*)env)->v12) {
    return cons(box_int(((Env_v28*)env)->v10), (Node*)apply((Closure*)v7(((Env_v28*)env)->v11), (void*)(((Env_v28*)env)->v9)));
  } else {
    return cons(box_int(((Env_v28*)env)->v12), (Node*)apply((Closure*)v7(v13), (void*)(((Env_v28*)env)->v8)));
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

Node* v32(void* env, void* v11_raw) {
  Node* v11 = (Node*)v11_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v11 = v11;
  env29->v8 = ((Env_v32*)env)->v8;
  env29->v9 = ((Env_v32*)env)->v9;
  env29->v10 = ((Env_v32*)env)->v10;
  Closure* c = malloc(sizeof(Closure));
  c->env = env29;
  c->fn = (void* (*)(void*, void*))v29;
  return (isEmpty(((Env_v32*)env)->v9)) ? (((Env_v32*)env)->v8) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(((Env_v32*)env)->v9)))), (void*)(tail(((Env_v32*)env)->v9))));
}

Closure* v33(void* env, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v10 = v10;
  env32->v8 = ((Env_v33*)env)->v8;
  env32->v9 = ((Env_v33*)env)->v9;
  Closure* c = malloc(sizeof(Closure));
  c->env = env32;
  c->fn = (void* (*)(void*, void*))v32;
  return c;
}

Node* v36(void* env, void* v9_raw) {
  Node* v9 = (Node*)v9_raw;
  Env_v33* env33 = malloc(sizeof(Env_v33));
  env33->v9 = v9;
  env33->v8 = ((Env_v36*)env)->v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env33;
  c->fn = (void* (*)(void*, void*))v33;
  return (isEmpty(((Env_v36*)env)->v8)) ? (v9) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(((Env_v36*)env)->v8)))), (void*)(tail(((Env_v36*)env)->v8))));
}

Closure* v7(Node* v8) {
  Env_v36* env36 = malloc(sizeof(Env_v36));
  env36->v8 = v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env36;
  c->fn = (void* (*)(void*, void*))v36;
  return c;
}

Node* v45(Pair* v6) {
  return (Node*)apply((Closure*)v7(v0((Node*)fst(v6))), (void*)(v0((Node*)snd(v6))));
}

Pair* v46(void* env, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v46*)env)->v21), (Node*)fst(v23)), (Node*)snd(v23));
}

Pair* v51(void* env, void* v22_raw) {
  Node* v22 = (Node*)v22_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v21 = ((Env_v51*)env)->v21;
  Closure* c = malloc(sizeof(Closure));
  c->env = env46;
  c->fn = (void* (*)(void*, void*))v46;
  return (Pair*)apply((Closure*)c, (void*)(v17(mk_pair(box_int((((Env_v51*)env)->v19 - 1)), v22))));
}

Closure* v52(void* env, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v51* env51 = malloc(sizeof(Env_v51));
  env51->v21 = v21;
  env51->v19 = ((Env_v52*)env)->v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env51;
  c->fn = (void* (*)(void*, void*))v51;
  return c;
}

Pair* v55(void* env, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if (((Env_v55*)env)->v19 == 0) {
    return mk_pair(NULL, v20);
  } else {
    Env_v52* env52 = malloc(sizeof(Env_v52));
    env52->v19 = ((Env_v55*)env)->v19;
    Closure* c = malloc(sizeof(Closure));
    c->env = env52;
    c->fn = (void* (*)(void*, void*))v52;
    return (isEmpty(v20)) ? (mk_pair(NULL, NULL)) : ((Pair*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v20)))), (void*)(tail(v20))));
  }
}

Pair* v58(void* env, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env55;
  c->fn = (void* (*)(void*, void*))v55;
  return (Pair*)apply((Closure*)c, (void*)((Node*)snd(((Env_v58*)env)->v18)));
}

Pair* v17(Pair* v18) {
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v18 = v18;
  Closure* c = malloc(sizeof(Closure));
  c->env = env58;
  c->fn = (void* (*)(void*, void*))v58;
  return (Pair*)apply((Closure*)c, box_int(*(int*)fst(v18)));
}

Pair* v63(void* env, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v63*)env)->v14));
}

Pair* v66(void* env, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  env63->v14 = ((Env_v66*)env)->v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env63;
  c->fn = (void* (*)(void*, void*))v63;
  return (Pair*)apply((Closure*)c, box_int((v15 / 2)));
}

int v67(Node* v27) {
  return (1 + v24(v27));
}

int (*v68(int v26))(Node*) {
  return v67;
}

int v24(Node* v25) {
  return (isEmpty(v25)) ? (0) : (v68(*(int*)(head(v25)))(tail(v25)));
}

Pair* v75(Node* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env66;
  c->fn = (void* (*)(void*, void*))v66;
  return (Pair*)apply((Closure*)c, box_int(v24(v14)));
}

Node* v80(void* env, void* v5_raw) {
  Node* v5 = (Node*)v5_raw;
  return v45(v75(cons(box_int(((Env_v80*)env)->v2), cons(box_int(((Env_v80*)env)->v4), v5))));
}

Closure* v81(void* env, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v80* env80 = malloc(sizeof(Env_v80));
  env80->v4 = v4;
  env80->v2 = ((Env_v81*)env)->v2;
  Closure* c = malloc(sizeof(Closure));
  c->env = env80;
  c->fn = (void* (*)(void*, void*))v80;
  return c;
}

Node* v84(void* env, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v2 = ((Env_v84*)env)->v2;
  Closure* c = malloc(sizeof(Closure));
  c->env = env81;
  c->fn = (void* (*)(void*, void*))v81;
  return (isEmpty(v3)) ? (cons(box_int(((Env_v84*)env)->v2), NULL)) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v3)))), (void*)(tail(v3))));
}

Closure* v85(int v2) {
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v2 = v2;
  Closure* c = malloc(sizeof(Closure));
  c->env = env84;
  c->fn = (void* (*)(void*, void*))v84;
  return c;
}

Node* v0(Node* v1) {
  return (isEmpty(v1)) ? (NULL) : ((Node*)apply((Closure*)v85(*(int*)(head(v1))), (void*)(tail(v1))));
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

