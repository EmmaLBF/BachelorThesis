
// imports
#include <stdbool.h>


int v3(int v1, int (*v0)(int)) {
  int v2 = 1;
  if (v1 == 0) {
    v2 = 1;
  } else {
    v2 = (v1 * v0((v1 - 1)));
  }
  return v2;
}

int (*v4(int (*v0)(int)))(int) {
  return v3;
}

int (*v5)(int) = v4(v5);
