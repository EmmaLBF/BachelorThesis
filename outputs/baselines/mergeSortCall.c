
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
Node* v28(void* env, void* v13_raw);
Closure* v29(void* env, void* v12_raw);
Node* v30(void* env, void* v11_raw);
Closure* v31(void* env, void* v10_raw);
Node* v32(void* env, void* v9_raw);
Closure* v7(Node* v8);
Node* v34(Pair* v6);
Pair* v35(void* env, void* v23_raw);
Pair* v36(void* env, void* v22_raw);
Closure* v37(void* env, void* v21_raw);
Pair* v38(void* env, void* v20_raw);
Pair* v39(void* env, void* v19_raw);
Pair* v17(Pair* v18);
Pair* v40(void* env, void* v16_raw);
Pair* v41(void* env, void* v15_raw);
int v42(Node* v27);
int (*v43(int v26))(Node*);
int v24(Node* v25);
Pair* v45(Node* v14);
Node* v47(void* env, void* v5_raw);
Closure* v48(void* env, void* v4_raw);
Node* v49(void* env, void* v3_raw);
Closure* v50(void* env, void* v2_raw);
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
} Env_v30;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v31;

typedef struct {
    Node* v8;
} Env_v32;

typedef struct {
    int v21;
} Env_v35;

typedef struct {
    int v19;
    int v21;
} Env_v36;

typedef struct {
    int v19;
} Env_v37;

typedef struct {
    int v19;
} Env_v38;

typedef struct {
    Pair* v18;
} Env_v39;

typedef struct {
    Node* v14;
} Env_v40;

typedef struct {
    Node* v14;
} Env_v41;

typedef struct {
    Node* v1;
} Env_v47;

typedef struct {
    Node* v1;
} Env_v48;

typedef struct {
    Node* v1;
    int v2;
} Env_v49;

typedef struct {
    Node* v1;
} Env_v50;

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

Node* v30(void* env, void* v11_raw) {
  Node* v11 = (Node*)v11_raw;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v11 = v11;
  env29->v8 = ((Env_v30*)env)->v8;
  env29->v9 = ((Env_v30*)env)->v9;
  env29->v10 = ((Env_v30*)env)->v10;
  Closure* c = malloc(sizeof(Closure));
  c->env = env29;
  c->fn = (void* (*)(void*, void*))v29;
  return (isEmpty(((Env_v30*)env)->v9)) ? (((Env_v30*)env)->v8) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(((Env_v30*)env)->v9)))), (void*)(tail(((Env_v30*)env)->v9))));
}

Closure* v31(void* env, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v30* env30 = malloc(sizeof(Env_v30));
  env30->v10 = v10;
  env30->v8 = ((Env_v31*)env)->v8;
  env30->v9 = ((Env_v31*)env)->v9;
  Closure* c = malloc(sizeof(Closure));
  c->env = env30;
  c->fn = (void* (*)(void*, void*))v30;
  return c;
}

Node* v32(void* env, void* v9_raw) {
  Node* v9 = (Node*)v9_raw;
  Env_v31* env31 = malloc(sizeof(Env_v31));
  env31->v9 = v9;
  env31->v8 = ((Env_v32*)env)->v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env31;
  c->fn = (void* (*)(void*, void*))v31;
  return (isEmpty(((Env_v32*)env)->v8)) ? (v9) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(((Env_v32*)env)->v8)))), (void*)(tail(((Env_v32*)env)->v8))));
}

Closure* v7(Node* v8) {
  Env_v32* env32 = malloc(sizeof(Env_v32));
  env32->v8 = v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env32;
  c->fn = (void* (*)(void*, void*))v32;
  return c;
}

Node* v34(Pair* v6) {
  Closure* v33 = v7(v0((Node*)fst(v6)));
  return (Node*)apply((Closure*)v33, (void*)(v0((Node*)snd(v6))));
}

Pair* v35(void* env, void* v23_raw) {
  Pair* v23 = (Pair*)v23_raw;
  return mk_pair(cons(box_int(((Env_v35*)env)->v21), (Node*)fst(v23)), (Node*)snd(v23));
}

Pair* v36(void* env, void* v22_raw) {
  Node* v22 = (Node*)v22_raw;
  Env_v35* env35 = malloc(sizeof(Env_v35));
  env35->v21 = ((Env_v36*)env)->v21;
  Closure* c = malloc(sizeof(Closure));
  c->env = env35;
  c->fn = (void* (*)(void*, void*))v35;
  return (Pair*)apply((Closure*)c, (void*)(v17(mk_pair(box_int((((Env_v36*)env)->v19 - 1)), v22))));
}

Closure* v37(void* env, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v36* env36 = malloc(sizeof(Env_v36));
  env36->v21 = v21;
  env36->v19 = ((Env_v37*)env)->v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env36;
  c->fn = (void* (*)(void*, void*))v36;
  return c;
}

Pair* v38(void* env, void* v20_raw) {
  Node* v20 = (Node*)v20_raw;
  if (((Env_v38*)env)->v19 == 0) {
    return mk_pair(NULL, v20);
  } else {
    Env_v37* env37 = malloc(sizeof(Env_v37));
    env37->v19 = ((Env_v38*)env)->v19;
    Closure* c = malloc(sizeof(Closure));
    c->env = env37;
    c->fn = (void* (*)(void*, void*))v37;
    return (isEmpty(v20)) ? (mk_pair(NULL, NULL)) : ((Pair*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v20)))), (void*)(tail(v20))));
  }
}

Pair* v39(void* env, void* v19_raw) {
  int v19 = *(int*)v19_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v19 = v19;
  Closure* c = malloc(sizeof(Closure));
  c->env = env38;
  c->fn = (void* (*)(void*, void*))v38;
  return (Pair*)apply((Closure*)c, (void*)((Node*)snd(((Env_v39*)env)->v18)));
}

Pair* v17(Pair* v18) {
  Env_v39* env39 = malloc(sizeof(Env_v39));
  env39->v18 = v18;
  Closure* c = malloc(sizeof(Closure));
  c->env = env39;
  c->fn = (void* (*)(void*, void*))v39;
  return (Pair*)apply((Closure*)c, box_int(*(int*)fst(v18)));
}

Pair* v40(void* env, void* v16_raw) {
  int v16 = *(int*)v16_raw;
  return v17(mk_pair(box_int(v16), ((Env_v40*)env)->v14));
}

Pair* v41(void* env, void* v15_raw) {
  int v15 = *(int*)v15_raw;
  Env_v40* env40 = malloc(sizeof(Env_v40));
  env40->v14 = ((Env_v41*)env)->v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env40;
  c->fn = (void* (*)(void*, void*))v40;
  return (Pair*)apply((Closure*)c, box_int((v15 / 2)));
}

int v42(Node* v27) {
  return (1 + v24(v27));
}

int (*v43(int v26))(Node*) {
  return v42;
}

int v24(Node* v25) {
  return (isEmpty(v25)) ? (0) : (v43(*(int*)(head(v25)))(tail(v25)));
}

Pair* v45(Node* v14) {
  int v44 = v24(v14);
  Env_v41* env41 = malloc(sizeof(Env_v41));
  env41->v14 = v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env41;
  c->fn = (void* (*)(void*, void*))v41;
  return (Pair*)apply((Closure*)c, box_int(v44));
}

Node* v47(void* env, void* v5_raw) {
  Node* v5 = (Node*)v5_raw;
  Pair* v46 = v45(((Env_v47*)env)->v1);
  return v34(v46);
}

Closure* v48(void* env, void* v4_raw) {
  int v4 = *(int*)v4_raw;
  Env_v47* env47 = malloc(sizeof(Env_v47));
  env47->v1 = ((Env_v48*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env47;
  c->fn = (void* (*)(void*, void*))v47;
  return c;
}

Node* v49(void* env, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  env48->v1 = ((Env_v49*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env48;
  c->fn = (void* (*)(void*, void*))v48;
  return (isEmpty(v3)) ? (cons(box_int(((Env_v49*)env)->v2), NULL)) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v3)))), (void*)(tail(v3))));
}

Closure* v50(void* env, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v49* env49 = malloc(sizeof(Env_v49));
  env49->v2 = v2;
  env49->v1 = ((Env_v50*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env49;
  c->fn = (void* (*)(void*, void*))v49;
  return c;
}

Node* v0(Node* v1) {
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env50;
  c->fn = (void* (*)(void*, void*))v50;
  return (isEmpty(v1)) ? (NULL) : ((Node*)apply((Closure*)apply((Closure*)c, box_int(*(int*)(head(v1)))), (void*)(tail(v1))));
}

// main
int main(void) {
  printList(v0(LIST900()));
  return 0;
}

