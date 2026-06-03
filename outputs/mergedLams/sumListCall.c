
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
int v6(void* env6, void* v2_raw, void* v3_raw);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    int v2;
    NodeInt* v3;
    NodeInt* v1;
} Env_v6;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v6(void* env6, void* v2_raw, void* v3_raw) {
  int v2 = *(int*)v2_raw;
  NodeInt* v3 = (NodeInt*)v3_raw;
  return (v2 + v0(v3));
}

int v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return v6((v1)->head, (v1)->tail);
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

