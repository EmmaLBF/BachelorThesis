#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>


// Ptr list
typedef struct List {
    void* head;
    struct List* tail;
} List;
List* cons(void* head, List* tail) {
    List* node = malloc(sizeof(List));
    node->head = head;
    node->tail = tail;
    return node;
}

// Int list
typedef struct ListInt {
    int head;
    struct ListInt* tail;
} ListInt;
ListInt* consInt(int head, ListInt* tail) {
    ListInt* node = malloc(sizeof(ListInt));
    node->head = head;
    node->tail = tail;
    return node;
}

// Bool list
typedef struct ListBool {
    bool head;
    struct ListBool* tail;
} ListBool;
ListBool* consBool(int head, ListBool* tail) {
    ListBool* node = malloc(sizeof(ListBool));
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

void printListInt(ListInt *list) {
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

ListInt* LIST(size_t size, int seed) {
    srand(seed);
    ListInt* list = NULL;
    for (int i = 0; i < size; i++) {
        list = consInt(rand(), list);
    }
    return list;
}