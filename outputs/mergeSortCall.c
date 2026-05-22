
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v29(void* env29, void* v12_raw, void* v13_raw);
Node* v7(Node* v8, Node* v9);
Node* v45(Pair* v6);
Pair* v46(void* env46, void* v23_raw);
Pair* v55(void* env55, void* v20_raw);
Pair* v17(Pair* v18);
Pair* v63(void* env63, void* v16_raw);
int v24(Node* v25);
Pair* v75(Node* v14);
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

Node* v7(Node* v8, Node* v9) {
  if (isEmpty(v8)) {
    return v9;
  } else {
    int v10 = *(int*)((head(v8)));
    Node* v11 = tail(v8);
    Env_v29* env29 = malloc(sizeof(Env_v29));
    env29->v10 = v10;
    env29->v11 = v11;
    env29->v8 = v8;
    env29->v9 = v9;
    return (Node*)((isEmpty(v9)) ? (v8) : ((Node*)v29(env29, box_int(*(int*)((head(v9)))), (void*)(tail(v9)))));
  }
}

Node* v45(Pair* v6) {
  return v7(v0((Node*)(fst(v6))), v0((Node*)(snd(v6))));
}

Pair* v46(void* env46, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v46*)env46)->v21), (Node*)(fst(v23))), (Node*)(snd(v23)));
}

Pair* v55(void* env55, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if ((((Env_v55*)env55)->v19 == 0)) {
    return mk_pair(NULL, v20);
  } else {
    if (isEmpty(v20)) {
      return mk_pair(NULL, NULL);
    } else {
      void* env52 = env55;
      int v21 = *(int*)((head(v20)));
      Node* v22 = tail(v20);
      Env_v46* env46 = malloc(sizeof(Env_v46));
      env46->v21 = v21;
      return (Pair*)(Pair*)v46(env46, (void*)(v17(mk_pair(box_int((((Env_v52*)env52)->v19 - 1)), v22))));
    }
  }
}

Pair* v17(Pair* v18) {
  int v19 = *(int*)(fst(v18));
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v19 = v19;
  return (Pair*)(Pair*)v55(env55, (void*)((Node*)(snd(v18))));
}

Pair* v63(void* env63, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v63*)env63)->v14));
}

int v24(Node* v25) {
  if (isEmpty(v25)) {
    return 0;
  } else {
    int v26 = *(int*)((head(v25)));
    Node* v27 = tail(v25);
    return (1 + v24(v27));
  }
}

Pair* v75(Node* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  int v15 = v24(v14);
  return (Pair*)(Pair*)v63(env66, box_int((v15 / 2)));
}

Node* v0(Node* v1) {
  if (isEmpty(v1)) {
    return NULL;
  } else {
    int v2 = *(int*)((head(v1)));
    Node* v3 = tail(v1);
    if (isEmpty(v3)) {
      return cons(box_int(v2), NULL);
    } else {
      int v4 = *(int*)((head(v3)));
      Node* v5 = tail(v3);
      return (Node*)v45(v75(cons(box_int(v2), cons(box_int(v4), v5))));
    }
  }
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

