
import random

# Generate 100 random integers between 1 and 1000
random_integers = [random.randint(1, 1000) for _ in range(200)]

def gen_list(nums):
    if not nums:
        return "NULL"
    else:
        res = str(gen_list(nums[1:]))
        return "cons(box_int(" + str(nums[0]) + "), " + res + ")"

print(gen_list(random_integers))


