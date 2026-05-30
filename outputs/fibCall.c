
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "listLib.c"

// pair type defitions
// function defitions
int v0(int v1);

// closure defitions
// function implementations
int v0(int v1) {
  if ((v1 < 2)) {
    return v1;
  } else {
    return (v0((v1 - 1)) + v0((v1 - 2)));
  }
}

// main
int main(void) {
  printInt(v0(5));
  return 0;
}

