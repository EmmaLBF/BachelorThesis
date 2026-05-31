
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
// function defitions
int v4(NodeInt* v3);
int (*v5(int v2))(NodeInt*);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    NodeInt* v3;
} Env_v4;

typedef struct {
    int v2;
} Env_v5;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v4(NodeInt* v3) {
  return (1 + v0(v3));
}

int (*v5(int v2))(NodeInt*) {
  return v4;
}

int v0(NodeInt* v1) {
  if (((v1) == NULL)) {
    return 0;
  } else {
    return v5((v1)->head)((v1)->tail);
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

