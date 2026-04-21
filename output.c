#include <stdbool.h>
typedef int (*intToint)(int);

intToint v4(intToint v0) {
  int v3(int v1) {
    int v2 = 1;
    if (v1 == 0) {
      v2 = 1;
    } else {
      v2 = (v1 * v0((v1 - 1)));
    }
    return v2;
  }
  return v3;
}
intToint v5 = v4(v5);
return v5;

