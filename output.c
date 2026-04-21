// imports
#include <stdbool.h>

// function ptr types
typedef int (*intToint)(int);

// function declarations
int v3(int v1, intToint v0);
intToint v4(intToint v0);

// function definitions
int v3(int v1, intToint v0) {
  int v2 = 1;
  if (v1 == 0) {
    v2 = 1;
  } else {
    v2 = (v1 * v0((v1 - 1)));
  }
  return v2;
}

intToint v4(intToint v0) {
  return v3;
}

int main(void) {
  intToint v5 = v4(v5);
  return v5;
}
