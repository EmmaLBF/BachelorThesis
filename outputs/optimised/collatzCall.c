
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../lib.c"

// pair type defitions
// function defitions
int v0(int v1);

// closure defitions
// function implementations
int v0(int v1) {
  if ((v1 == 1)) return 0;
  return (1 + v0(((((v1 % 2) == 0)) ? ((v1 / 2)) : (((3 * v1) + 1)))));
}

// main
int main(void) {
  printInt(v0(8));
  return 0;
}

