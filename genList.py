
import random

# def gen_list(nums):
#     if not nums:
#         return "NULL"
#     else:
#         res = str(gen_list(nums[1:]))
#         return "cons(box_int(" + str(nums[0]) + "), " + res + ")"
def gen_list(nums):
    result = "NULL"
    for n in reversed(nums):
        result = "cons(box_int(" + str(n) + "), " + result + ")"
    return result

# for i in [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]:
#     random_integers = [random.randint(1, i) for _ in range(i)]
#     print("Node* LIST" + str(i) + "() {")
#     print("     return " + gen_list(random_integers) + ";")
#     print("}")


random_integers = [random.randint(1, 1000) for _ in range(1000)]
print("Node* LIST" + str(1000) + "() {")
print("     return " + gen_list(random_integers) + ";")
print("}")


