
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v29(void* env29, void* v12_raw, void* v13_raw);
Node* v33(void* env33, void* v10_raw, void* v11_raw);
Node* v7(Node* v8, Node* v9);
Node* v45(Pair* v6);
Pair* v46(void* env46, void* v23_raw);
Pair* v52(void* env52, void* v21_raw, void* v22_raw);
Pair* v55(void* env55, void* v20_raw);
Pair* v58(void* env58, void* v19_raw);
Pair* v17(Pair* v18);
Pair* v63(void* env63, void* v16_raw);
Pair* v66(void* env66, void* v15_raw);
int v68(int v26, Node* v27);
int v24(Node* v25);
Pair* v75(Node* v14);
Node* v81(void* env81, void* v4_raw, void* v5_raw);
Node* v85(int v2, Node* v3);
Node* v0(Node* v1);

// closure defitions
typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
} Env_v29;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v33;

typedef struct {
    int v21;
} Env_v46;

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
} Env_v81;

// function implementations
Node* v29(void* env29, void* v12_raw, void* v13_raw) {
  int v12 = *(int*)v12_raw;
  Node* v13 = (Node*)v13_raw;
  if ((((Env_v29*)env29)->v10 < v12)) {
    return cons(box_int(((Env_v29*)env29)->v10), v7(((Env_v29*)env29)->v11, ((Env_v29*)env29)->v9));
  } else {
    return cons(box_int(v12), v7(v13, ((Env_v29*)env29)->v8));
  }
}

Node* v33(void* env33, void* v10_raw, void* v11_raw) {
  int v10 = *(int*)v10_raw;
  Node* v11 = (Node*)v11_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v10 = v10;
  env29->v11 = v11;
  env29->v8 = ((Env_v33*)env33)->v8;
  env29->v9 = ((Env_v33*)env33)->v9;
  return ((isEmpty(((Env_v33*)env33)->v9)) ? (((Env_v33*)env33)->v8) : ((Node*)v29(env29, box_int(*(int*)((head(((Env_v33*)env33)->v9)))), (void*)(tail(((Env_v33*)env33)->v9)))));
}

Node* v7(Node* v8, Node* v9) {
  Env_v33* env33 = malloc(sizeof(Env_v33));
  env33->v8 = v8;
  env33->v9 = v9;
  return ((isEmpty(v8)) ? (v9) : ((Node*)v33(env33, box_int(*(int*)((head(v8)))), (void*)(tail(v8)))));
}

Node* v45(Pair* v6) {
  return v7(v0((Node*)(fst(v6))), v0((Node*)(snd(v6))));
}

Pair* v46(void* env46, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v46*)env46)->v21), (Node*)(fst(v23))), (Node*)(snd(v23)));
}

Pair* v52(void* env52, void* v21_raw, void* v22_raw) {
  int v21 = *(int*)v21_raw;
  Node* v22 = (Node*)v22_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v21 = v21;
  return (Pair*)v46(env46, (void*)(v17(mk_pair(box_int((((Env_v52*)env52)->v19 - 1)), v22))));
}

Pair* v55(void* env55, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if ((((Env_v55*)env55)->v19 == 0)) {
    return mk_pair(NULL, v20);
  } else {
    return ((isEmpty(v20)) ? (mk_pair(NULL, NULL)) : ((Pair*)v52(env55, box_int(*(int*)((head(v20)))), (void*)(tail(v20)))));
  }
}

Pair* v58(void* env58, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  return (Pair*)v55(env55, (void*)((Node*)(snd(((Env_v58*)env58)->v18))));
}

Pair* v17(Pair* v18) {
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v18 = v18;
  return (Pair*)v58(env58, box_int(*(int*)(fst(v18))));
}

Pair* v63(void* env63, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v63*)env63)->v14));
}

Pair* v66(void* env66, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  return (Pair*)v63(env66, box_int((v15 / 2)));
}

int v68(int v26, Node* v27) {
  return (1 + v24(v27));
}

int v24(Node* v25) {
  return ((isEmpty(v25)) ? (0) : (v68(*(int*)((head(v25))), tail(v25))));
}

Pair* v75(Node* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  return (Pair*)v66(env66, box_int(v24(v14)));
}

Node* v81(void* env81, void* v4_raw, void* v5_raw) {
  int v4 = *(int*)v4_raw;
  Node* v5 = (Node*)v5_raw;
  return v45(v75(cons(box_int(((Env_v81*)env81)->v2), cons(box_int(v4), v5))));
}

Node* v85(int v2, Node* v3) {
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v2 = v2;
  return ((isEmpty(v3)) ? (cons(box_int(v2), NULL)) : ((Node*)v81(env81, box_int(*(int*)((head(v3)))), (void*)(tail(v3)))));
}

Node* v0(Node* v1) {
  return ((isEmpty(v1)) ? (NULL) : (v85(*(int*)((head(v1))), tail(v1))));
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

