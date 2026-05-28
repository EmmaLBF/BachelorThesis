
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
NodeInt* v7(void* env7, void* v3_raw, void* v4_raw);
NodeInt* v0(int (*v1)(int), NodeInt* v2);
int v11(int v5);

// closure defitions
typedef struct {
    int v3;
    NodeInt* v4;
    int (*v1)(int);
} Env_v7;

typedef struct {
    int (*v1)(int);
    NodeInt* v2;
} Env_v0;

typedef struct {
    int v5;
} Env_v11;

// function implementations
NodeInt* v7(void* env7, void* v3_raw, void* v4_raw) {
  int v3 = *(int*)v3_raw;
  NodeInt* v4 = (NodeInt*)v4_raw;
  return consInt(((Env_v7*)env7)->v1(v3), v0(((Env_v7*)env7)->v1, v4));
}

NodeInt* v0(int (*v1)(int), NodeInt* v2) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = v1;
  Closure* c7 = malloc(sizeof(Closure));
  c7->env = env7;
  c7->fn = (void* (*)(void*, void*))v7;
  return ((((v2) == NULL)) ? (NULL) : ((NodeInt*)((Closure*)c7)->fn(((Closure*)c7)->env, box_int((v2)->head), (v2)->tail)));
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printListInt(v0(v11, consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

