
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
// function defitions
ListInt* v9(void* env9, void* v4_raw);
Closure* v10(void* env10, void* v3_raw);
ListInt* v12(void* env12, void* v2_raw);
Closure* v0(int (*v1)(int));
int v14(int v5);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
    int (*v1)(int);
    int v3;
} Env_v9;

typedef struct {
    int (*v1)(int);
} Env_v10;

typedef struct {
    int (*v1)(int);
} Env_v12;

// function implementations
ListInt* v9(void* env9, void* v4_raw) {
  ListInt* v4 = (ListInt*)v4_raw;
  Closure* c0 = v0(((Env_v9*)env9)->v1);
  return consInt(((Env_v9*)env9)->v1(((Env_v9*)env9)->v3), (ListInt*)(c0)->fn((c0)->env, v4));
}

Closure* v10(void* env10, void* v3_raw) {
  int v3 = *(int*)v3_raw;
  Env_v9* env9 = malloc(sizeof(Env_v9));
  env9->v1 = ((Env_v10*)env10)->v1;
  env9->v3 = v3;
  Closure* c9 = malloc(sizeof(Closure));
  c9->env = env9;
  c9->fn = (void* (*)(void*, void*))v9;
  return c9;
}

ListInt* v12(void* env12, void* v2_raw) {
  ListInt* v2 = (ListInt*)v2_raw;
  Env_v10* env10 = malloc(sizeof(Env_v10));
  env10->v1 = ((Env_v12*)env12)->v1;
  if (((v2) == NULL)) return NULL;
  Closure* c10 = v10(env10, box_int((v2)->head));
  return (ListInt*)(c10)->fn((c10)->env, (v2)->tail);
}

Closure* v0(int (*v1)(int)) {
  Env_v12* env12 = malloc(sizeof(Env_v12));
  env12->v1 = v1;
  Closure* c12 = malloc(sizeof(Closure));
  c12->env = env12;
  c12->fn = (void* (*)(void*, void*))v12;
  return c12;
}

int v14(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  Closure* c0 = v0(v14);
  printListInt((ListInt*)(c0)->fn((c0)->env, consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

