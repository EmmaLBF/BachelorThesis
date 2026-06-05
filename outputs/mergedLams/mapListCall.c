
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
NodeInt* v10(void* env10, void* v3_raw, void* v4_raw);
NodeInt* v0(int (*v1)(int), NodeInt* v2);
int v14(int v5);

// env defitions
typedef struct {
    int v3;
    NodeInt* v4;
    int (*v1)(int);
    NodeInt* v2;
} Env_v10;

typedef struct {
    int (*v1)(int);
    NodeInt* v2;
} Env_v0;

typedef struct {
    int v5;
} Env_v14;

// function implementations
NodeInt* v10(void* env10, void* v3_raw, void* v4_raw) {
  int v3 = *(int*)v3_raw;
  NodeInt* v4 = (NodeInt*)v4_raw;
  return consInt(((Env_v10*)env10)->v1(v3), v0(((Env_v10*)env10)->v1, v4));
}

NodeInt* v0(int (*v1)(int), NodeInt* v2) {
  Env_v10* env10 = malloc(sizeof(Env_v10));
  env10->v1 = v1;
  env10->v2 = v2;
  if (((v2) == NULL)) {
    return NULL;
  } else {
    return v10(env10, box_int((v2)->head), (void*)((v2)->tail));
  }
}

int v14(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printListInt(v0(v14, consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

