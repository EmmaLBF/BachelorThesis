
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v6(void* env, void* v4_raw);
Closure* v7(void* env, void* v3_raw);
Node* v8(void* env, void* v2_raw);
Closure* v0(int (*v1)(int));
int v9(int v5);

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
} Env_v8;

// function implementations
Node* v6(void* env, void* v4_raw) {
  Node* v4 = (Node*)v4_raw;
  return cons(box_int(((Env_v6*)env)->v1(((Env_v6*)env)->v3)), (Node*)((Closure*)v0(((Env_v6*)env)->v1))->fn(((Closure*)v0(((Env_v6*)env)->v1))->env, (void*)(v4)));
}

Closure* v7(void* env, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v6* env6 = malloc(sizeof(Env_v6));
  env6->v3 = v3;
  env6->v1 = ((Env_v7*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env6;
  c->fn = (void* (*)(void*, void*))v6;
  return c;
}

Node* v8(void* env, void* v2_raw) {
  Node* v2 = (Node*)v2_raw;
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = ((Env_v8*)env)->v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env7;
  c->fn = (void* (*)(void*, void*))v7;
  return (isEmpty(v2)) ? (NULL) : ((Node*)(Closure*)c->fn((Closure*)c->env, box_int(*(int*)(head(v2)))));
}

Closure* v0(int (*v1)(int)) {
  Env_v8* env8 = malloc(sizeof(Env_v8));
  env8->v1 = v1;
  Closure* c = malloc(sizeof(Closure));
  c->env = env8;
  c->fn = (void* (*)(void*, void*))v8;
  return c;
}

int v9(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  Closure* v10 = v0(v9);
  printList((Node*)(Closure*)v10->fn((Closure*)v10->env, (void*)(cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL))))));
  return 0;
}

