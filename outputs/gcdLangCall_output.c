
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v0(Pair* v1);

// closure defitions
// function implementations
int v0(Pair* v1) {
  if (*(int*)snd(v1) == 0) {
    return *(int*)fst(v1);
  } else {
    return v0(mk_pair(box_int(*(int*)snd(v1)), box_int((*(int*)fst(v1) % *(int*)snd(v1)))));
  }
}

// main
int main(void) {
  printf("%d\n", v0(mk_pair(box_int(30), box_int(10))));
  return 0;
}

