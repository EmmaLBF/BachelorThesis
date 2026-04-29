// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// List Definitions
typedef struct Node {
    void* head;
    struct Node* tail;
} Node;

Node* cons(void* head, Node* tail) {
    Node* node = malloc(sizeof(Node));
    node->head = head;
    node->tail = tail;
    return node;
}

int isEmpty(Node* xs) {
    return xs == NULL;
}

void* head(Node* xs) {
    return xs->head;
}

Node* tail(Node* xs) {
    return xs->tail;
}

// Function Definitions
int v5(int v2, Node* v3);
int v0(Node* v1);

// Compiled Program
int v5(int v2, Node* v3) {
  return (1 + v0(v3));
}

int v0(Node* v1) {
  return (isEmpty(v1)) ? (0) : (v5(*(int*)head(v1), tail(v1)));
}

int main(void) {
  printf("%d\n", v0(cons(&(int){1}, cons(&(int){2}, cons(&(int){3}, NULL)))));
  return 0;
}

