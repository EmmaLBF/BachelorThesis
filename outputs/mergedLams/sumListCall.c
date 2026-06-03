
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
int v6(int v2, NodeInt* v3);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    int v2;
    NodeInt* v3;
} Env_v6;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v6(int v2, NodeInt* v3) {
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

