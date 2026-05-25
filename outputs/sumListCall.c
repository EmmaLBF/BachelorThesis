
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// function defitions
int v0(NodeInt* v1);

// closure defitions
// function implementations
int v0(NodeInt* v1) {
  if (isEmptyInt(v1)) {
    return 0;
  } else {
    return ((headInt(v1)) + v0(tailInt(v1)));
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

