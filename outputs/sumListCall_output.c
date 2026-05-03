// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "listLib.c"

// Function Definitions
int v4(Node* v3, Closure_v4* env);
Closure_v4* v5(int v2);
int v0(Node* v1);

// Compiled Program
typedef struct {
    int v2;
} Closure_v4;

int v4(Node* v3, Closure_v4* env) {
  return (env->v2 + v0(v3));
}

Closure_v4* v5(int v2) {
  Closure_v4* env = malloc(sizeof(Closure_v4));
  env->v2= v2;
  return env;
}

int v0(Node* v1) {
  return (isEmpty(v1)) ? (0) : (v5(*(int*)head(v1))(tail(v1)));
}

int main(void) {
  printf("%d\n", v0(cons(&(int){1}, cons(&(int){2}, cons(&(int){3}, NULL)))));
  return 0;
}

