
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
  if (*(int*)snd((Pair*)v1) == 0) {
    return *(int*)fst((Pair*)v1);
  } else {
    return v0(mk_pair(mk_int(*(int*)snd((Pair*)v1)), mk_int((*(int*)fst((Pair*)v1) % *(int*)snd((Pair*)v1)))));
  }
}

// main
int main(void) {
  printf("%d\n", v0(mk_pair(mk_int(30), mk_int(10))));
  return 0;
}

