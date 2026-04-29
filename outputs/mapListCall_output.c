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
Node* v6(Node* v4, int (*v1)(int), int v3);
Node* (*v7(int v3, int (*v1)(int)))(Node*);
Node* v0(int (*v1)(int), Node* v2);
int v9(int v5);

// Compiled Program
Node* v6(Node* v4, int (*v1)(int), int v3) {
  return cons(&(int){v1(v3)}, v0(v1, v4));
}

Node* (*v7(int v3, int (*v1)(int)))(Node*) {
  return v6;
}

Node* v0(int (*v1)(int), Node* v2) {
  return (isEmpty(v2)) ? (NULL) : (v7(tail(v2)));
}

int v9(int v5) {
  return (v5 * 2);
}

int main(void) {
  printf("%d\n", v0(v9, cons(&(int){1}, cons(&(int){2}, cons(&(int){3}, NULL)))));
  return 0;
}

