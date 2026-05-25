
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
int v0(Pair* v1);

// closure defitions
typedef struct {
    Pair* v1;
} Env_v0;

// function implementations
int v0(Pair* v1) {
  if ((*(int*)((v1)->snd) == 0)) {
    return *(int*)((v1)->fst);
  } else {
    return v0(mk_pair(box_int(*(int*)((v1)->snd)), box_int((*(int*)((v1)->fst) % *(int*)((v1)->snd)))));
  }
}

// main
int main(void) {
  printInt(v0(mk_pair(box_int(30), box_int(10))));
  return 0;
}

