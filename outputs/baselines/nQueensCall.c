
// imports
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "../listLib.c"

// pair type defitions
typedef struct Pair_Int_Int {
  int fst;
  int snd;
} Pair_Int_Int;

Pair_Int_Int* makePair_Int_Int(int fst, int snd) {
  Pair_Int_Int* p = malloc(sizeof(Pair_Int_Int));
  p->fst = fst;
  p->snd = snd;
  return p;
};

// function defitions
int v38(void* env38, void* v3_raw);
Closure* v39(void* env39, void* v2_raw);
int v0(Node* v1);
Node* v45(void* env45, void* v18_raw);
Closure* v46(void* env46, void* v17_raw);
Node* v48(void* env48, void* v16_raw);
Closure* v14(void* env14, void* v15_raw);
bool v50(void* env50, void* v36_raw);
bool v52(void* env52, void* v35_raw);
bool v54(void* env54, void* v34_raw);
Closure* v55(void* env55, void* v33_raw);
Closure* v57(void* env57, void* v32_raw);
Closure* v59(void* env59, void* v31_raw);
bool v65(void* env65, void* v30_raw);
Closure* v66(void* env66, void* v29_raw);
bool v68(void* env68, void* v28_raw);
Closure* v26(void* env26, void* v27_raw);
Node* v72(void* env72, void* v25_raw);
Node* v78(void* env78, void* v24_raw);
Node* v80(void* env80, void* v23_raw);
Closure* v81(void* env81, void* v22_raw);
Closure* v82(void* env82, void* v21_raw);
Closure* v19(void* env19, void* v20_raw);
Node* v92(void* env92, void* v13_raw);
Closure* v93(void* env93, void* v12_raw);
Node* v95(void* env95, void* v11_raw);
Closure* v96(void* env96, void* v10_raw);
Closure* v8(void* env8, void* v9_raw);
Node* v101(void* env101, void* v7_raw);
Closure* v5(void* env5, void* v6_raw);
Node* v104(int v4);

// env defitions
typedef struct {
} Env_v0;

typedef struct {
    int v4;
} Env_v5;

typedef struct {
} Env_v8;

typedef struct {
} Env_v14;

typedef struct {
} Env_v19;

typedef struct {
} Env_v26;

typedef struct {
} Env_v38;

typedef struct {
} Env_v39;

typedef struct {
    Node* v16;
    Node* v17;
} Env_v45;

typedef struct {
    Node* v16;
} Env_v46;

typedef struct {
    Node* v15;
} Env_v48;

typedef struct {
    int v32;
    int v33;
    int v35;
} Env_v50;

typedef struct {
    int v32;
    int v33;
    Pair_Int_Int *v34;
} Env_v52;

typedef struct {
    int v32;
    int v33;
} Env_v54;

typedef struct {
    int v32;
} Env_v55;

typedef struct {
    Pair_Int_Int *v31;
} Env_v57;

typedef struct {
} Env_v59;

typedef struct {
    Pair_Int_Int *v27;
    Pair_Int_Int *v29;
} Env_v65;

typedef struct {
    Pair_Int_Int *v27;
} Env_v66;

typedef struct {
    Pair_Int_Int *v27;
} Env_v68;

typedef struct {
    Node* v22;
    Pair_Int_Int *v24;
} Env_v72;

typedef struct {
    int v20;
    int v21;
    Node* v22;
    int v23;
} Env_v78;

typedef struct {
    int v20;
    int v21;
    Node* v22;
} Env_v80;

typedef struct {
    int v20;
    int v21;
} Env_v81;

typedef struct {
    int v20;
} Env_v82;

typedef struct {
    int v9;
    int v10;
    Node* v12;
} Env_v92;

typedef struct {
    int v9;
    int v10;
} Env_v93;

typedef struct {
    int v9;
    int v10;
} Env_v95;

typedef struct {
    int v9;
} Env_v96;

typedef struct {
    int v4;
    int v6;
} Env_v101;

typedef struct {
} Env_v104;

// function implementations
int v38(void* env38, void* v3_raw) {
  Node* v3 = (Node*)v3_raw;
  return (1 + v0(v3));
}

Closure* v39(void* env39, void* v2_raw) {
  Node* v2 = (Node*)v2_raw;
  Env_v38* env38 = malloc(sizeof(Env_v38));
  Closure* c38 = malloc(sizeof(Closure));
  c38->env = env38;
  c38->fn = (void* (*)(void*, void*))v38;
  return c38;
}

int v0(Node* v1) {
  Env_v39* env39 = malloc(sizeof(Env_v39));
  if (((v1) == NULL)) {
    return 0;
  } else {
    Closure* c39 = v39(env39, (void*)((v1)->head));
    return (int)(intptr_t)(c39)->fn((c39)->env, (v1)->tail);
  }
}

Node* v45(void* env45, void* v18_raw) {
  Node* v18 = (Node*)v18_raw;
  Env_v14* env14 = malloc(sizeof(Env_v14));
  Closure* c14 = v14(env14, (void*)(v18));
  return cons(((Env_v45*)env45)->v17, (Node*)(c14)->fn((c14)->env, ((Env_v45*)env45)->v16));
}

Closure* v46(void* env46, void* v17_raw) {
  Node* v17 = (Node*)v17_raw;
  Env_v45* env45 = malloc(sizeof(Env_v45));
  env45->v17 = v17;
  env45->v16 = ((Env_v46*)env46)->v16;
  Closure* c45 = malloc(sizeof(Closure));
  c45->env = env45;
  c45->fn = (void* (*)(void*, void*))v45;
  return c45;
}

Node* v48(void* env48, void* v16_raw) {
  Node* v16 = (Node*)v16_raw;
  Env_v46* env46 = malloc(sizeof(Env_v46));
  env46->v16 = v16;
  if (((((Env_v48*)env48)->v15) == NULL)) {
    return v16;
  } else {
    Closure* c46 = v46(env46, (void*)((((Env_v48*)env48)->v15)->head));
    return (Node*)(c46)->fn((c46)->env, (((Env_v48*)env48)->v15)->tail);
  }
}

Closure* v14(void* env14, void* v15_raw) {
  Node* v15 = (Node*)v15_raw;
  Env_v48* env48 = malloc(sizeof(Env_v48));
  env48->v15 = v15;
  Closure* c48 = malloc(sizeof(Closure));
  c48->env = env48;
  c48->fn = (void* (*)(void*, void*))v48;
  return c48;
}

bool v50(void* env50, void* v36_raw) {
  int v36 = *(int*)v36_raw;
  return ((((Env_v50*)env50)->v33 == v36) || (abs((((Env_v50*)env50)->v33 - v36)) == abs((((Env_v50*)env50)->v32 - ((Env_v50*)env50)->v35))));
}

bool v52(void* env52, void* v35_raw) {
  int v35 = *(int*)v35_raw;
  Env_v50* env50 = malloc(sizeof(Env_v50));
  env50->v35 = v35;
  env50->v32 = ((Env_v52*)env52)->v32;
  env50->v33 = ((Env_v52*)env52)->v33;
  return v50(env50, box_int((((Env_v52*)env52)->v34)->snd));
}

bool v54(void* env54, void* v34_raw) {
  Pair_Int_Int *v34 = (Pair_Int_Int*)v34_raw;
  Env_v52* env52 = malloc(sizeof(Env_v52));
  env52->v34 = v34;
  env52->v32 = ((Env_v54*)env54)->v32;
  env52->v33 = ((Env_v54*)env54)->v33;
  return v52(env52, box_int((v34)->fst));
}

Closure* v55(void* env55, void* v33_raw) {
  int v33 = *(int*)v33_raw;
  Env_v54* env54 = malloc(sizeof(Env_v54));
  env54->v33 = v33;
  env54->v32 = ((Env_v55*)env55)->v32;
  Closure* c54 = malloc(sizeof(Closure));
  c54->env = env54;
  c54->fn = (void* (*)(void*, void*))v54;
  return c54;
}

Closure* v57(void* env57, void* v32_raw) {
  int v32 = *(int*)v32_raw;
  Env_v55* env55 = malloc(sizeof(Env_v55));
  env55->v32 = v32;
  Closure* c55 = v55(env55, box_int((((Env_v57*)env57)->v31)->snd));
  return c55;
}

Closure* v59(void* env59, void* v31_raw) {
  Pair_Int_Int *v31 = (Pair_Int_Int*)v31_raw;
  Env_v57* env57 = malloc(sizeof(Env_v57));
  env57->v31 = v31;
  Closure* c57 = v57(env57, box_int((v31)->fst));
  return c57;
}

bool v65(void* env65, void* v30_raw) {
  Node* v30 = (Node*)v30_raw;
  Env_v26* env26 = malloc(sizeof(Env_v26));
  Env_v59* env59 = malloc(sizeof(Env_v59));
  Closure* c26 = v26(env26, (void*)(((Env_v65*)env65)->v27));
  Closure* c59 = v59(env59, (void*)(((Env_v65*)env65)->v27));
  return (!((Closure*)(c59)->fn((c59)->env, ((Env_v65*)env65)->v29)) && (bool)(intptr_t)(c26)->fn((c26)->env, v30));
}

Closure* v66(void* env66, void* v29_raw) {
  Pair_Int_Int *v29 = (Pair_Int_Int*)v29_raw;
  Env_v65* env65 = malloc(sizeof(Env_v65));
  env65->v29 = v29;
  env65->v27 = ((Env_v66*)env66)->v27;
  Closure* c65 = malloc(sizeof(Closure));
  c65->env = env65;
  c65->fn = (void* (*)(void*, void*))v65;
  return c65;
}

bool v68(void* env68, void* v28_raw) {
  Node* v28 = (Node*)v28_raw;
  Env_v66* env66 = malloc(sizeof(Env_v66));
  env66->v27 = ((Env_v68*)env68)->v27;
  if (((v28) == NULL)) {
    return true;
  } else {
    Closure* c66 = v66(env66, (void*)((v28)->head));
    return (bool)(intptr_t)(c66)->fn((c66)->env, (v28)->tail);
  }
}

Closure* v26(void* env26, void* v27_raw) {
  Pair_Int_Int *v27 = (Pair_Int_Int*)v27_raw;
  Env_v68* env68 = malloc(sizeof(Env_v68));
  env68->v27 = v27;
  Closure* c68 = malloc(sizeof(Closure));
  c68->env = env68;
  c68->fn = (void* (*)(void*, void*))v68;
  return c68;
}

Node* v72(void* env72, void* v25_raw) {
  Node* v25 = (Node*)v25_raw;
  Env_v26* env26 = malloc(sizeof(Env_v26));
  Closure* c26 = v26(env26, (void*)(((Env_v72*)env72)->v24));
  if ((bool)(intptr_t)(c26)->fn((c26)->env, ((Env_v72*)env72)->v22)) {
    return cons(cons(((Env_v72*)env72)->v24, ((Env_v72*)env72)->v22), v25);
  } else {
    return v25;
  }
}

Node* v78(void* env78, void* v24_raw) {
  Pair_Int_Int *v24 = (Pair_Int_Int*)v24_raw;
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Env_v72* env72 = malloc(sizeof(Env_v72));
  env72->v24 = v24;
  env72->v22 = ((Env_v78*)env78)->v22;
  Closure* c19 = v19(env19, box_int(((Env_v78*)env78)->v20));
  Closure* c107 = (c19)->fn((c19)->env, box_int(((Env_v78*)env78)->v21));
  Closure* c108 = (c107)->fn((c107)->env, ((Env_v78*)env78)->v22);
  return v72(env72, (void*)((Node*)(c108)->fn((c108)->env, box_int((((Env_v78*)env78)->v23 + 1)))));
}

Node* v80(void* env80, void* v23_raw) {
  int v23 = *(int*)v23_raw;
  Env_v78* env78 = malloc(sizeof(Env_v78));
  env78->v23 = v23;
  env78->v20 = ((Env_v80*)env80)->v20;
  env78->v21 = ((Env_v80*)env80)->v21;
  env78->v22 = ((Env_v80*)env80)->v22;
  if ((v23 == ((Env_v80*)env80)->v20)) {
    return NULL;
  } else {
    return v78(env78, (void*)(makePair_Int_Int(((Env_v80*)env80)->v21, v23)));
  }
}

Closure* v81(void* env81, void* v22_raw) {
  Node* v22 = (Node*)v22_raw;
  Env_v80* env80 = malloc(sizeof(Env_v80));
  env80->v22 = v22;
  env80->v20 = ((Env_v81*)env81)->v20;
  env80->v21 = ((Env_v81*)env81)->v21;
  Closure* c80 = malloc(sizeof(Closure));
  c80->env = env80;
  c80->fn = (void* (*)(void*, void*))v80;
  return c80;
}

Closure* v82(void* env82, void* v21_raw) {
  int v21 = *(int*)v21_raw;
  Env_v81* env81 = malloc(sizeof(Env_v81));
  env81->v21 = v21;
  env81->v20 = ((Env_v82*)env82)->v20;
  Closure* c81 = malloc(sizeof(Closure));
  c81->env = env81;
  c81->fn = (void* (*)(void*, void*))v81;
  return c81;
}

Closure* v19(void* env19, void* v20_raw) {
  int v20 = *(int*)v20_raw;
  Env_v82* env82 = malloc(sizeof(Env_v82));
  env82->v20 = v20;
  Closure* c82 = malloc(sizeof(Closure));
  c82->env = env82;
  c82->fn = (void* (*)(void*, void*))v82;
  return c82;
}

Node* v92(void* env92, void* v13_raw) {
  Node* v13 = (Node*)v13_raw;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  Env_v14* env14 = malloc(sizeof(Env_v14));
  Env_v19* env19 = malloc(sizeof(Env_v19));
  Closure* c19 = v19(env19, box_int(((Env_v92*)env92)->v9));
  Closure* c109 = (c19)->fn((c19)->env, box_int(((Env_v92*)env92)->v10));
  Closure* c110 = (c109)->fn((c109)->env, ((Env_v92*)env92)->v12);
  Closure* c8 = v8(env8, box_int(((Env_v92*)env92)->v9));
  Closure* c111 = (c8)->fn((c8)->env, box_int(((Env_v92*)env92)->v10));
  Closure* c14 = v14(env14, (void*)((Node*)(c110)->fn((c110)->env, box_int(0))));
  return (Node*)(c14)->fn((c14)->env, (Node*)(c111)->fn((c111)->env, v13));
}

Closure* v93(void* env93, void* v12_raw) {
  Node* v12 = (Node*)v12_raw;
  Env_v92* env92 = malloc(sizeof(Env_v92));
  env92->v12 = v12;
  env92->v9 = ((Env_v93*)env93)->v9;
  env92->v10 = ((Env_v93*)env93)->v10;
  Closure* c92 = malloc(sizeof(Closure));
  c92->env = env92;
  c92->fn = (void* (*)(void*, void*))v92;
  return c92;
}

Node* v95(void* env95, void* v11_raw) {
  Node* v11 = (Node*)v11_raw;
  Env_v93* env93 = malloc(sizeof(Env_v93));
  env93->v9 = ((Env_v95*)env95)->v9;
  env93->v10 = ((Env_v95*)env95)->v10;
  if (((v11) == NULL)) {
    return NULL;
  } else {
    Closure* c93 = v93(env93, (void*)((v11)->head));
    return (Node*)(c93)->fn((c93)->env, (v11)->tail);
  }
}

Closure* v96(void* env96, void* v10_raw) {
  int v10 = *(int*)v10_raw;
  Env_v95* env95 = malloc(sizeof(Env_v95));
  env95->v10 = v10;
  env95->v9 = ((Env_v96*)env96)->v9;
  Closure* c95 = malloc(sizeof(Closure));
  c95->env = env95;
  c95->fn = (void* (*)(void*, void*))v95;
  return c95;
}

Closure* v8(void* env8, void* v9_raw) {
  int v9 = *(int*)v9_raw;
  Env_v96* env96 = malloc(sizeof(Env_v96));
  env96->v9 = v9;
  Closure* c96 = malloc(sizeof(Closure));
  c96->env = env96;
  c96->fn = (void* (*)(void*, void*))v96;
  return c96;
}

Node* v101(void* env101, void* v7_raw) {
  Node* v7 = (Node*)v7_raw;
  Env_v5* env5 = malloc(sizeof(Env_v5));
  env5->v4 = ((Env_v101*)env101)->v4;
  Env_v8* env8 = malloc(sizeof(Env_v8));
  if ((((Env_v101*)env101)->v6 == ((Env_v101*)env101)->v4)) {
    return v7;
  } else {
    Closure* c8 = v8(env8, box_int(((Env_v101*)env101)->v4));
    Closure* c112 = (c8)->fn((c8)->env, box_int(((Env_v101*)env101)->v6));
    Closure* c5 = v5(env5, box_int((((Env_v101*)env101)->v6 + 1)));
    return (Node*)(c5)->fn((c5)->env, (Node*)(c112)->fn((c112)->env, v7));
  }
}

Closure* v5(void* env5, void* v6_raw) {
  int v6 = *(int*)v6_raw;
  Env_v101* env101 = malloc(sizeof(Env_v101));
  env101->v6 = v6;
  env101->v4 = ((Env_v5*)env5)->v4;
  Closure* c101 = malloc(sizeof(Closure));
  c101->env = env101;
  c101->fn = (void* (*)(void*, void*))v101;
  return c101;
}

Node* v104(int v4) {
  Env_v5* env5 = malloc(sizeof(Env_v5));
  env5->v4 = v4;
  Closure* c5 = v5(env5, box_int(0));
  return (Node*)(c5)->fn((c5)->env, cons(NULL, NULL));
}

// main
int main(void) {
  printInt(v0(v104(8)));
  return 0;
}

