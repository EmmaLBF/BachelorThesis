
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v25(void* env, Node* v13);
Closure* v26(void* env, int v12);
Node* v28(void* env, Node* v11);
Closure* v29(void* env, int v10);
Node* v31(void* env, Node* v9);
Closure* v7(Node* v8);
Node* v33(Pair* v6);
Pair* v34(void* env, Node* v20);
Closure* v35(void* env, int v19);
Pair* v17(Pair* v18);
Pair* v37(void* env, int v16);
Pair* v38(void* env, int v15);
int v39(Node* v24);
int (*v40(int v23))(Node*);
int v21(Node* v22);
Pair* v43(Node* v14);
Node* v45(void* env, Node* v5);
Closure* v46(void* env, int v4);
Node* v48(void* env, Node* v3);
Closure* v49(void* env, int v2);
Node* v0(Node* v1);

// closure defitions
typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
    int v12;
} Env_v25;

typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
} Env_v26;

typedef struct {
    Node* v8;
    Node* v9;
    int v10;
} Env_v28;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v29;

typedef struct {
    Node* v8;
} Env_v31;

typedef struct {
    Pair* v18;
    int v19;
} Env_v34;

typedef struct {
    Pair* v18;
} Env_v35;

typedef struct {
    Node* v14;
} Env_v37;

typedef struct {
    Node* v14;
} Env_v38;

typedef struct {
    Node* v1;
} Env_v45;

typedef struct {
    Node* v1;
} Env_v46;

typedef struct {
    Node* v1;
    int v2;
} Env_v48;

typedef struct {
    Node* v1;
} Env_v49;

// function implementations
Node* v25(void* env, Node* v13) {
  if (((Env_v25*)env)->v10 < ((Env_v25*)env)->v12) {
    return cons(mk_int((int)(((Env_v25*)env)->v10)), apply(v7(((Env_v25*)env)->v11), ((Env_v25*)env)->v9));
  } else {
    return cons(mk_int((int)(((Env_v25*)env)->v12)), apply(v7(v13), ((Env_v25*)env)->v8));
  }
}

Closure* v26(void* env, int v12) {
  Env_v25* env25 = malloc(sizeof(Env_v25));
  env25->v12 = v12;
  env25->v8 = ((Env_v26*)env)->v8;
  env25->v9 = ((Env_v26*)env)->v9;
  env25->v10 = ((Env_v26*)env)->v10;
  env25->v11 = ((Env_v26*)env)->v11;
  Closure* c = malloc(sizeof(Closure));
  c->env = env25;
  c->fn = (void* (*)(void*, void*))v25;
  return c;
}

Node* v28(void* env, Node* v11) {
  Node* v27 = ((Env_v28*)env)->v9;
  Env_v26* env26 = malloc(sizeof(Env_v26));
  env26->v11 = v11;
  env26->v8 = ((Env_v28*)env)->v8;
  env26->v9 = ((Env_v28*)env)->v9;
  env26->v10 = ((Env_v28*)env)->v10;
  Closure* c = malloc(sizeof(Closure));
  c->env = env26;
  c->fn = (void* (*)(void*, void*))v26;
  return (isEmpty(v27)) ? (((Env_v28*)env)->v8) : (apply(c, tail(v27)));
}

Closure* v29(void* env, int v10) {
  Env_v28* env28 = malloc(sizeof(Env_v28));
  env28->v10 = v10;
  env28->v8 = ((Env_v29*)env)->v8;
  env28->v9 = ((Env_v29*)env)->v9;
  Closure* c = malloc(sizeof(Closure));
  c->env = env28;
  c->fn = (void* (*)(void*, void*))v28;
  return c;
}

Node* v31(void* env, Node* v9) {
  Node* v30 = ((Env_v31*)env)->v8;
  Env_v29* env29 = malloc(sizeof(Env_v29));
  env29->v9 = v9;
  env29->v8 = ((Env_v31*)env)->v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env29;
  c->fn = (void* (*)(void*, void*))v29;
  return (isEmpty(v30)) ? (v9) : (apply(c, tail(v30)));
}

Closure* v7(Node* v8) {
  Env_v31* env31 = malloc(sizeof(Env_v31));
  env31->v8 = v8;
  Closure* c = malloc(sizeof(Closure));
  c->env = env31;
  c->fn = (void* (*)(void*, void*))v31;
  return c;
}

Node* v33(Pair* v6) {
  Closure* v32 = v7(v0(*(Node**)fst((Pair*)v6)));
  return apply(v32, v0(*(Node**)snd((Pair*)v6)));
}

Pair* v34(void* env, Node* v20) {
  return mk_pair(mk_int(cons(mk_int((int)(((Env_v34*)env)->v19)), *(Node**)fst((Pair*)v17(mk_pair(mk_int((*(int*)fst((Pair*)((Env_v34*)env)->v18) - 1)), mk_int(v20)))))), mk_int(*(Node**)snd((Pair*)v17(mk_pair(mk_int((*(int*)fst((Pair*)((Env_v34*)env)->v18) - 1)), mk_int(v20))))));
}

Closure* v35(void* env, int v19) {
  Env_v34* env34 = malloc(sizeof(Env_v34));
  env34->v19 = v19;
  env34->v18 = ((Env_v35*)env)->v18;
  Closure* c = malloc(sizeof(Closure));
  c->env = env34;
  c->fn = (void* (*)(void*, void*))v34;
  return c;
}

Pair* v17(Pair* v18) {
  if (*(int*)fst((Pair*)v18) == 0) {
    return mk_pair(mk_int(NULL), mk_int(*(Node**)snd((Pair*)v18)));
  } else {
    Node* v36 = *(Node**)snd((Pair*)v18);
    Env_v35* env35 = malloc(sizeof(Env_v35));
    env35->v18 = v18;
    Closure* c = malloc(sizeof(Closure));
    c->env = env35;
    c->fn = (void* (*)(void*, void*))v35;
    return (isEmpty(v36)) ? (mk_pair(mk_int(NULL), mk_int(NULL))) : (apply(c, tail(v36)));
  }
}

Pair* v37(void* env, int v16) {
  return v17(mk_pair(mk_int(v16), mk_int(((Env_v37*)env)->v14)));
}

Pair* v38(void* env, int v15) {
  Env_v37* env37 = malloc(sizeof(Env_v37));
  env37->v14 = ((Env_v38*)env)->v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env37;
  c->fn = (void* (*)(void*, void*))v37;
  return apply(c, (v15 / 2));
}

int v39(Node* v24) {
  return (1 + v21(v24));
}

int (*v40(int v23))(Node*) {
  return v39;
}

int v21(Node* v22) {
  Node* v41 = v22;
  return (isEmpty(v41)) ? (0) : (v40(*(int*)head(v41))(tail(v41)));
}

Pair* v43(Node* v14) {
  int v42 = v21(v14);
  Env_v38* env38 = malloc(sizeof(Env_v38));
  env38->v14 = v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env38;
  c->fn = (void* (*)(void*, void*))v38;
  return apply(c, v42);
}

Node* v45(void* env, Node* v5) {
  Pair* v44 = v43(((Env_v45*)env)->v1);
  return v33(v44);
}

Closure* v46(void* env, int v4) {
  Env_v45* env45 = malloc(sizeof(Env_v45));
  env45->v1 = ((Env_v46*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env45;
  c->fn = (void* (*)(void*, void*))v45;
  return c;
}

Node* v48(void* env, Node* v3) {
  Node* v47 = v3;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v1 = ((Env_v48*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env46;
  c->fn = (void* (*)(void*, void*))v46;
  return (isEmpty(v47)) ? (cons(mk_int((int)(((Env_v48*)env)->v2)), NULL)) : (apply((Closure*)apply(c, *(int*)head(v47)), tail(v47)));
}

Closure* v49(void* env, int v2) {
  Env_v48* env48 = malloc(sizeof(Env_v48));
  env48->v2 = v2;
  env48->v1 = ((Env_v49*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env48;
  c->fn = (void* (*)(void*, void*))v48;
  return c;
}

Node* v0(Node* v1) {
  Node* v50 = v1;
  Env_v49* env49 = malloc(sizeof(Env_v49));
  env49->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env49;
  c->fn = (void* (*)(void*, void*))v49;
  return (isEmpty(v50)) ? (NULL) : (apply(c, tail(v50)));
}

// main
int main(void) {
  printList(v0(cons(mk_int((int)(4)), cons(mk_int((int)(6)), cons(mk_int((int)(3)), NULL)))));
  return 0;
}

