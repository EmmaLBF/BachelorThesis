
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
// function defitions
int v6(void* env6, void* v2_raw, void* v3_raw);
int v0(ListInt* v1);

// closure defitions
typedef struct {
} Env_v0;

typedef struct {
} Env_v6;

// function implementations
int v6(void* env6, void* v2_raw, void* v3_raw) {
  int v2 = *(int*)v2_raw;
  ListInt* v3 = (ListInt*)v3_raw;
  return (v2 + v0(v3));
}

int v0(ListInt* v1) {
  Env_v6* env6 = malloc(sizeof(Env_v6));
  if (((v1) == NULL)) return 0;
  return v6(env6, box_int((v1)->head), (void*)((v1)->tail));
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

