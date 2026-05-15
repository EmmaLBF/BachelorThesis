
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
Node* v45(Pair* v6);
Pair* v17(Pair* v18);
Pair* v75(Node* v14);
Node* v0(Node* v1);

// closure defitions
typedef struct {
    Node* v8;
    Node* v9;
    int v10;
    Node* v11;
} Env_v29;

typedef struct {
    Node* v8;
    Node* v9;
} Env_v33;

typedef struct {
    int v21;
} Env_v46;

typedef struct {
    int v19;
} Env_v52;

typedef struct {
    int v19;
} Env_v55;

typedef struct {
    Pair* v18;
} Env_v58;

typedef struct {
    Node* v14;
} Env_v63;

typedef struct {
    Node* v14;
} Env_v66;

typedef struct {
    int v2;
} Env_v81;

// function implementations
Node* v45(Pair* v6) {
  return v7(v0((Node*)fst(v6)), v0((Node*)snd(v6)));
}

Pair* v17(Pair* v18) {
  Env_v58* env58 = malloc(sizeof(Env_v58));
  env58->v18 = v18;
  Closure* c = malloc(sizeof(Closure));
  c->env = env58;
  c->fn = (void* (*)(void*, void*))v58;
  return (Pair*)apply((Closure*)c, box_int(*(int*)fst(v18)));
}

Pair* v75(Node* v14) {
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v14 = v14;
  Closure* c = malloc(sizeof(Closure));
  c->env = env66;
  c->fn = (void* (*)(void*, void*))v66;
  Node* v25 = v14;
  return (Pair*)apply((Closure*)c, box_int((isEmpty(v25)) ? (0) : (v68(*(int*)(head(v25)), tail(v25)))));
}

Node* v0(Node* v1) {
  return (isEmpty(v1)) ? (NULL) : (v85(*(int*)(head(v1)), tail(v1)));
}

// main
int main(void) {
  printList(v0(cons(box_int(4), cons(box_int(6), cons(box_int(3), NULL)))));
  return 0;
}

