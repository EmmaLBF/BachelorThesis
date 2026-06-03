
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
int v5(void* env5, void* v3_raw);
Closure* v6(int v2);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v3;
    int v2;
} Env_v5;

typedef struct {
    int v2;
} Env_v6;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v5(void* env5, void* v3_raw) {
  NodeInt* v3 = (NodeInt*)v3_raw;
  return (((Env_v5*)env5)->v2 + v0(v3));
}

Closure* v6(int v2) {
  Env_v5* env5 = malloc(sizeof(Env_v5));
  env5->v2 = v2;
  Closure* c5 = malloc(sizeof(Closure));
  c5->env = env5;
  c5->fn = (void* (*)(void*, void*))v5;
  return c5;
}

int v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return (int)(intptr_t)((Closure*)v6((v1)->head))->fn(((Closure*)v6((v1)->head))->env, (v1)->tail);
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

