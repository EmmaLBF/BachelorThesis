#include <stdlib.h>
#include <stdbool.h>

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
    // void* (*fn)(void*, void*);
    void* (*fn)();
} Closure;

void* apply(Closure* c, void* arg) {
    return c->fn(c->env, arg);
}

void printList(Node *list) {
  if (list == NULL) return;
  printf("%d ", *(int*)list->head);
  printList(list->tail);
}

void printInt(int i) {
  printf("%d\n", i);
}

// box / unbox

int* box_int(int v) { int* p = malloc(sizeof(int)); *p = v; return p; }
int unbox_int(void* p) { return *(int*)p; }

bool* box_bool(bool v) { bool* p = malloc(sizeof(bool)); *p = v; return p; }
bool unbox_bool(void* p) { return *(bool*)p; }

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

void* fst(Pair* p) { return p->fst; }
void* snd(Pair* p) { return p->snd; }