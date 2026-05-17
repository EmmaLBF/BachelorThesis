
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
int v4(void* env4, void* v3_raw);
Closure* v5(int v2);
int v0(Node* v1);

// closure defitions
typedef struct {
    int v2;
} Env_v4;

// function implementations
int v4(void* env4, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (((Env_v4*)env4)->v2 + v0(v3));
}

Closure* v5(int v2) {
  Env_v4* env4 = malloc(sizeof(Env_v4));
  env4->v2 = v2;
  Closure* c4 = malloc(sizeof(Closure));
  c4->env = env4;
  c4->fn = (void* (*)(void*, void*))v4;
  return c4;
}

int v0(Node* v1) {
  return ((isEmpty(v1)) ? (0) : ((int)(intptr_t)apply((Closure*)v5(*(int*)(head(v1))), (void*)(tail(v1)))));
}

// main
int main(void) {
  printInt(v0(cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

