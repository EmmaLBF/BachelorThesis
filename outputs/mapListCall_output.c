
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v6(void* env, void* v4);
Closure* v7(void* env, void* v3);
Node* v9(void* env, void* v2);
Closure* v0(int (*v1)(int));
int v10(int v5);

// closure defitions
typedef struct {
    int (*v1)(int);
    int v3;
} Env_v6;

typedef struct {
    int (*v1)(int);
} Env_v7;

typedef struct {
    int (*v1)(int);
} Env_v9;

// function implementations
Node* v6(void* env, void* v4) {
  return cons(mk_int((int)(((Env_v6*)env)->v1(((Env_v6*)env)->v3))), (Node*)apply(v0(((Env_v6*)env)->v1), v4));
}

Closure* v7(void* env, void* v3) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  env6->v1 = ((Env_v7*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env6;
  c->fn = (void* (*)(void*, void*))v6;
  return c;
}

Node* v9(void* env, void* v2) {
  Node* v8 = v2;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = ((Env_v9*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env7;
  c->fn = (void* (*)(void*, void*))v7;
  return (isEmpty(v8)) ? (NULL) : ((Node*)apply((Closure*)apply(c, mk_int(*(int*)head(v8))), tail(v8)));
}

Closure* v0(int (*v1)(int)) {
  Env_v9* env9 = malloc(sizeof(Env_v9));
  env9->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env9;
  c->fn = (void* (*)(void*, void*))v9;
  return c;
}

int v10(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  Closure* v11 = v0(v10);
  printList((Node*)apply(v11, cons(mk_int((int)(1)), cons(mk_int((int)(2)), cons(mk_int((int)(3)), NULL)))));
  return 0;
}

