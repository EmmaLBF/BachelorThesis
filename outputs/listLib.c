#include <stdlib.h>

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

typedef struct Closure {
    void* env;
    void* (*fn)(void*, void*);
} Closure;

void* apply(Closure* c, void* arg) {
    return c->fn(c->env, arg);
}

void printList(Node *list) {
  if (list == NULL) return;
  printf("%d\n", (int)(intptr_t)list->head);
  printList(list->tail);
}