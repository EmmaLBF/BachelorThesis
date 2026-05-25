
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
int v5(int v2, NodeInt* v3);
int v0(NodeInt* v1);

// closure defitions
typedef struct {
    int v2;
    NodeInt* v3;
} Env_v5;

typedef struct {
    NodeInt* v1;
} Env_v0;

// function implementations
int v5(int v2, NodeInt* v3) {
  return (v2 + v0(v3));
}

int v0(NodeInt* v1) {
  return ((((v1) == NULL)) ? (0) : (v5((v1)->head, (v1)->tail)));
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

