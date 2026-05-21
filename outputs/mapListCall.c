
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v7(void* env7, void* v3_raw, void* v4_raw);
Node* v0(int (*v1)(int), Node* v2);
int v11(int v5);

// closure defitions
typedef struct {
    int (*v1)(int);
} Env_v7;

// function implementations
Node* v7(void* env7, void* v3_raw, void* v4_raw) {
  int v3 = *(int*)v3_raw;
  Node* v4 = (Node*)v4_raw;
  return cons(box_int(((Env_v7*)env7)->v1(v3)), v0(((Env_v7*)env7)->v1, v4));
}

Node* v0(int (*v1)(int), Node* v2) {
  Env_v7* env7 = malloc(sizeof(Env_v7));
  env7->v1 = v1;
  Closure* c7 = malloc(sizeof(Closure));
  c7->env = env7;
  c7->fn = (void* (*)(void*, void*))v7;
  return ((isEmpty(v2)) ? (NULL) : ((Node*)((Closure*)c7)->fn(((Closure*)c7)->env, box_int(*(int*)((head(v2)))), tail(v2))));
}

int v11(int v5) {
  return (v5 * 2);
}

// main
int main(void) {
  printList(v0(v11, cons(box_int(1), cons(box_int(2), cons(box_int(3), NULL)))));
  return 0;
}

