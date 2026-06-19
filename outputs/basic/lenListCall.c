
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
// function defitions
int v5(void* env5, void* v3_raw);
Closure* v6(void* env6, void* v2_raw);
int v0(ListInt* v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
} Env_v5;

typedef struct {
} Env_v6;

// function implementations
int v5(void* env5, void* v3_raw) {
  ListInt* v3 = (ListInt*)v3_raw;
  return (1 + v0(v3));
}

Closure* v6(void* env6, void* v2_raw) {
  int v2 = *(int*)v2_raw;
  Env_v5* env5 = malloc(sizeof(Env_v5));
  Closure* c5 = malloc(sizeof(Closure));
  c5->env = env5;
  c5->fn = (void* (*)(void*, void*))v5;
  return c5;
}

int v0(ListInt* v1) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  if (((v1) == NULL)) return 0;
  Closure* c6 = v6(env6, box_int((v1)->head));
  return (int)(intptr_t)(c6)->fn((c6)->env, (v1)->tail);
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

