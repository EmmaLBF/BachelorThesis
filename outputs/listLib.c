#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>


// Ptr list
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

// Int list
typedef struct NodeInt {
    int head;
    struct NodeInt* tail;
} NodeInt;
NodeInt* consInt(int head, NodeInt* tail) {
    NodeInt* node = malloc(sizeof(NodeInt));
    node->head = head;
    node->tail = tail;
    return node;
}

// Bool list
typedef struct NodeBool {
    bool head;
    struct NodeBool* tail;
} NodeBool;
NodeBool* consBool(int head, NodeBool* tail) {
    NodeBool* node = malloc(sizeof(NodeBool));
    node->head = head;
    node->tail = tail;
    return node;
}

// Closures

typedef struct Closure {
    void* env;
    void* (*fn)();
} Closure;

// printing

void printListInt(NodeInt *list) {
    while (list != NULL) {
        printf("%d ", list->head);
        list = list->tail;
    }
}

void printInt(int i) { printf("%d\n", i); }

// box / unbox

int* box_int(int v) { int* p = malloc(sizeof(int)); *p = v; return p; }
bool* box_bool(bool v) { bool* p = malloc(sizeof(bool)); *p = v; return p; }

// pairs

typedef struct Pair {
    void* fst;
    void* snd;
} Pair;

Pair* mk_pair(void* fst, void* snd) {
    Pair* p = malloc(sizeof(Pair));
    p->fst = fst;
    p->snd = snd;
    return p;
}

// generate list

NodeInt* LIST(size_t size, int seed) {
    srand(seed);
    NodeInt* list = NULL;
    for (int i = 0; i < size; i++) {
        list = consInt(rand(), list);
    }
    return list;
}