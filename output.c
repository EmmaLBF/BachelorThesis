// imports
#include <stdbool.h>

int v0(int v1) {
  if (v1 == 0) {
    return 1;
  } else {
    return (v1 * v0((v1 - 1)));
  }
}

return v0(5);
