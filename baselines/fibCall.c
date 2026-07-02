// imports
#include <stdio.h>
#include <stdlib.h>
#include "../outputs/lib.c"

int fibonacci(int n) {
    int first = 1, second = 1, next = 1;
    for (int i = 3; i <= n; i++) {
        next = first + second;
        first = second;
        second = next;
    }
    return next;
}

int main() {
    int len = 5;
    printInt(fibonacci(len));
    return 0;
}