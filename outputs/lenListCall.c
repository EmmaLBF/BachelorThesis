
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
  if (((v1) == NULL)) {
    return 0;
  } else {
    return (1 + v0((v1)->tail));
  }
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

