
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// function defitions
int v5(int v2, NodeInt* v3);
int v0(NodeInt* v1);

// closure defitions
// function implementations
int v5(int v2, NodeInt* v3) {
  return (1 + v0(v3));
}

int v0(NodeInt* v1) {
  return ((isEmptyInt(v1)) ? (0) : (v5((headInt(v1)), tailInt(v1))));
}

// main
int main(void) {
  printInt(v0(consInt(1, consInt(2, consInt(3, NULL)))));
  return 0;
}

