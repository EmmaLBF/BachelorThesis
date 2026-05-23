
import random

def gen_listCons(nums):
    result = "NULL"
    for n in reversed(nums):
        result = "consInt(" + str(n) + ", " + result + ")"
    return result

def gen_listArray(nums):
    result = ""
    for n in reversed(nums):
        result = "[" + str(n) + ", " + result + "]"
    return result

for i in [600, 700, 800, 900, 1000]: # [100, 200, 300, 400, 500]:
    random_integers = [random.randint(1, i) for _ in range(i)]
    print("NodeInt* LIST" + str(i) + "() {")
    print("    return " + gen_listCons(random_integers) + ";")
    print("}\n")

    print("int* LIST" + str(i) + "_ARRAY() {")
    print(f"    int temp[] = " + "{" + str(','.join(map(str, (random_integers)))) + "};")
    print(f"    int *list = (int*)malloc({i} * sizeof(int));")
    print(f"    for(int i = 0; i < {i}; i++) " + "{")
    print(f"        list[i] = temp[i];")
    print("    }")
    print("    return list;")
    print("}\n")


