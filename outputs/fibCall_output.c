// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Function Definitions
int v0(int v1);

// Compiled Program
int v0(int v1) {
  if (v1 < 2) {
    return v1;
  } else {
    return (v0((v1 - 1)) + v0((v1 - 2)));
  }
}

int main(void) {
  printf("%d\n", v0(5));
  return 0;
}

