
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
Node* v28(void* env28, void* v13_raw);
Closure* v29(void* env29, void* v12_raw);
Node* v32(void* env32, void* v11_raw);
Closure* v33(void* env33, void* v10_raw);
Node* v36(void* env36, void* v9_raw);
Closure* v7(Node* v8);
Node* v45(Pair* v6);
Pair* v46(void* env46, void* v23_raw);
Pair* v51(void* env51, void* v22_raw);
Closure* v52(void* env52, void* v21_raw);
Pair* v55(void* env55, void* v20_raw);
Pair* v58(void* env58, void* v19_raw);
Pair* v17(Pair* v18);
Pair* v63(void* env63, void* v16_raw);
Pair* v66(void* env66, void* v15_raw);
int v67(Node* v27);
int (*v68(int v26))(Node*);
int v24(Node* v25);
Pair* v75(Node* v14);
Node* v80(void* env80, void* v5_raw);
Closure* v81(void* env81, void* v4_raw);
Node* v84(void* env84, void* v3_raw);
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
Node* v28(void* env28, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  if ((((Env_v28*)env28)->v10 < ((Env_v28*)env28)->v12)) {
    return cons(box_int(((Env_v28*)env28)->v10), (Node*)apply((Closure*)v7(((Env_v28*)env28)->v11), ((Env_v28*)env28)->v9));
  } else {
    return cons(box_int(((Env_v28*)env28)->v12), (Node*)apply((Closure*)v7(v13), ((Env_v28*)env28)->v8));
  }
}

Closure* v29(void* env29, void* v12_raw) {
  int v12 = *(int*)v12_raw;
  Env_v28* env28 = malloc(sizeof(Env_v28));
  env28->v12 = v12;
  env28->v8 = ((Env_v29*)env29)->v8;
  env28->v9 = ((Env_v29*)env29)->v9;
  env28->v10 = ((Env_v29*)env29)->v10;
  env28->v11 = ((Env_v29*)env29)->v11;
  Closure* c28 = malloc(sizeof(Closure));
  c28->env = env28;
  c28->fn = (void* (*)(void*, void*))v28;
  return c28;
}

Node* v32(void* env32, void* v11_raw) {
  Node* v11 = (Node*)v11_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v11 = v11;
  env29->v8 = ((Env_v32*)env32)->v8;
  env29->v9 = ((Env_v32*)env32)->v9;
  env29->v10 = ((Env_v32*)env32)->v10;
  Closure* c29 = malloc(sizeof(Closure));
  c29->env = env29;
  c29->fn = (void* (*)(void*, void*))v29;
  return ((isEmpty(((Env_v32*)env32)->v9)) ? (((Env_v32*)env32)->v8) : ((Node*)apply((Closure*)apply((Closure*)c29, box_int(*(int*)((head(((Env_v32*)env32)->v9))))), tail(((Env_v32*)env32)->v9))));
}

Closure* v33(void* env33, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v10 = v10;
  env32->v8 = ((Env_v33*)env33)->v8;
  env32->v9 = ((Env_v33*)env33)->v9;
  Closure* c32 = malloc(sizeof(Closure));
  c32->env = env32;
  c32->fn = (void* (*)(void*, void*))v32;
  return c32;
}

Node* v36(void* env36, void* v9_raw) {
  Node* v9 = (Node*)v9_raw;
  Env_v33* env33 = malloc(sizeof(Env_v33));
  env33->v9 = v9;
  env33->v8 = ((Env_v36*)env36)->v8;
  Closure* c33 = malloc(sizeof(Closure));
  c33->env = env33;
  c33->fn = (void* (*)(void*, void*))v33;
  return ((isEmpty(((Env_v36*)env36)->v8)) ? (v9) : ((Node*)apply((Closure*)apply((Closure*)c33, box_int(*(int*)((head(((Env_v36*)env36)->v8))))), tail(((Env_v36*)env36)->v8))));
}

Closure* v7(Node* v8) {
  Env_v36* env36 = malloc(sizeof(Env_v36));
  env36->v8 = v8;
  Closure* c36 = malloc(sizeof(Closure));
  c36->env = env36;
  c36->fn = (void* (*)(void*, void*))v36;
  return c36;
}

Node* v45(Pair* v6) {
  return (Node*)apply((Closure*)v7(v0((Node*)(fst(v6)))), v0((Node*)(snd(v6))));
}

Pair* v46(void* env46, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v46*)env46)->v21), (Node*)(fst(v23))), (Node*)(snd(v23)));
}

Pair* v51(void* env51, void* v22_raw) {
  Node* v22 = (Node*)v22_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v21 = ((Env_v51*)env51)->v21;
  Closure* c46 = malloc(sizeof(Closure));
  c46->env = env46;
  c46->fn = (void* (*)(void*, void*))v46;
  return (Pair*)apply((Closure*)c46, v17(mk_pair(box_int((((Env_v51*)env51)->v19 - 1)), v22)));
}

Closure* v52(void* env52, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v51* env51 = malloc(sizeof(Env_v51));
  env51->v21 = v21;
  env51->v19 = ((Env_v52*)env52)->v19;
  Closure* c51 = malloc(sizeof(Closure));
  c51->env = env51;
  c51->fn = (void* (*)(void*, void*))v51;
  return c51;
}

Pair* v55(void* env55, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if ((((Env_v55*)env55)->v19 == 0)) {
    return mk_pair(NULL, v20);
  } else {
    Env_v52* env52 = malloc(sizeof(Env_v52));
    env52->v19 = ((Env_v55*)env55)->v19;
    Closure* c52 = malloc(sizeof(Closure));
    c52->env = env52;
    c52->fn = (void* (*)(void*, void*))v52;
    return ((isEmpty(v20)) ? (mk_pair(NULL, NULL)) : ((Pair*)apply((Closure*)apply((Closure*)c52, box_int(*(int*)((head(v20))))), tail(v20))));
  }
}

Pair* v58(void* env58, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  Closure* c55 = malloc(sizeof(Closure));
  c55->env = env55;
  c55->fn = (void* (*)(void*, void*))v55;
  return (Pair*)apply((Closure*)c55, (Node*)(snd(((Env_v58*)env58)->v18)));
}

Pair* v17(Pair* v18) {
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v18 = v18;
  Closure* c58 = malloc(sizeof(Closure));
  c58->env = env58;
  c58->fn = (void* (*)(void*, void*))v58;
  return (Pair*)apply((Closure*)c58, box_int(*(int*)(fst(v18))));
}

Pair* v63(void* env63, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v63*)env63)->v14));
}

Pair* v66(void* env66, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v63* env63 = malloc(sizeof(Env_v63));
  env63->v14 = ((Env_v66*)env66)->v14;
  Closure* c63 = malloc(sizeof(Closure));
  c63->env = env63;
  c63->fn = (void* (*)(void*, void*))v63;
  return (Pair*)apply((Closure*)c63, box_int((v15 / 2)));
}

int v67(Node* v27) {
  return (1 + v24(v27));
}

int (*v68(int v26))(Node*) {
  return v67;
}

int v24(Node* v25) {
  return ((isEmpty(v25)) ? (0) : (v68(*(int*)((head(v25))))(tail(v25))));
}

Pair* v75(Node* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  Closure* c66 = malloc(sizeof(Closure));
  c66->env = env66;
  c66->fn = (void* (*)(void*, void*))v66;
  return (Pair*)apply((Closure*)c66, box_int(v24(v14)));
}

Node* v80(void* env80, void* v5_raw) {
  Node* v5 = (Node*)v5_raw;
  return v45(v75(cons(box_int(((Env_v80*)env80)->v2), cons(box_int(((Env_v80*)env80)->v4), v5))));
}

Closure* v81(void* env81, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v80* env80 = malloc(sizeof(Env_v80));
  env80->v4 = v4;
  env80->v2 = ((Env_v81*)env81)->v2;
  Closure* c80 = malloc(sizeof(Closure));
  c80->env = env80;
  c80->fn = (void* (*)(void*, void*))v80;
  return c80;
}

Node* v84(void* env84, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v2 = ((Env_v84*)env84)->v2;
  Closure* c81 = malloc(sizeof(Closure));
  c81->env = env81;
  c81->fn = (void* (*)(void*, void*))v81;
  return ((isEmpty(v3)) ? (cons(box_int(((Env_v84*)env84)->v2), NULL)) : ((Node*)apply((Closure*)apply((Closure*)c81, box_int(*(int*)((head(v3))))), tail(v3))));
}

Closure* v85(int v2) {
  Env_v84* env84 = malloc(sizeof(Env_v84));
  env84->v2 = v2;
  Closure* c84 = malloc(sizeof(Closure));
  c84->env = env84;
  c84->fn = (void* (*)(void*, void*))v84;
  return c84;
}

Node* v0(Node* v1) {
  return ((isEmpty(v1)) ? (NULL) : ((Node*)apply((Closure*)v85(*(int*)((head(v1)))), tail(v1))));
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

