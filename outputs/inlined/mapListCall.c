
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
NodeInt* v0(int (*v1)(int), NodeInt* v2);
int v11(int v5);

// closure defitions
typedef struct {
    int (*v1)(int);
} Env_v7;

// function implementations
NodeInt* v0(int (*v1)(int), NodeInt* v2) {
  if (isEmptyInt(v2)) {
    return NULL;
  } else {
    int v3 = (headInt(v2));
    NodeInt* v4 = tailInt(v2);
    return (NodeInt*)consInt(v1(v3), v0(v1, v4));
  }
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printListInt(v0(v11, consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

