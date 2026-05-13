
import random

def gen_list(nums):
    if not nums:
        return "NULL"
    else:
        res = str(gen_list(nums[1:]))
        return "cons(box_int(" + str(nums[0]) + "), " + res + ")"

for i in [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]:
    random_integers = [random.randint(1, i) for _ in range(i)]
    print("Node* LIST" + str(i) + "() {")
    print("     return " + gen_list(random_integers) + ";")
    print("}")


