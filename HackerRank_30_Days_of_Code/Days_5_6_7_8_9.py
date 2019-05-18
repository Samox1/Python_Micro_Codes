# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 5 - Loops

print("---------- Day 5 ----------")

# import math
# import os
# import random
# import re
# import sys
#
# if __name__ == '__main__':
#     n = int(input())
#
#     for i in range(1,11):
#         buf = "%d x %d = %d" % (n, i, n*i)
#         print(buf)


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 6 - Let's Review

print("---------- Day 6 ----------")

# import math
# import os
# import random
# import re
# import sys
#
# if __name__ == '__main__':
#     n = int(input())
#
#     for i in range(0,n):
#         s = input()
#
#     # First Option - Python easy approach
#         s1 = ""
#         s2 = ""
#
#         s1 = s[::2]
#         s2 = s[1::2]
#         print(s1 + " " + s2)
#
#     # Second Option - Typical C/C++ approach
#         s1 = ""
#         s2 = ""
#
#         for index in range(len(s)):
#             if index % 2 == 0:
#                 s1 += s[index]
#             else:
#                 s2 += s[index]
#
#         print(s1 + " " + s2)


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 7 - Arrays

print("---------- Day 7 ----------")

# import math
# import os
# import random
# import re
# import sys
#
# if __name__ == '__main__':
#     n = int(input())
#     arr = list(map(int, input().rstrip().split()))
#
#     for i in range(len(arr)):
#         print(arr[len(arr)-i-1], end=" ")


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 8 -

print("---------- Day 8 ----------")

n = int(input())
kappa = dict

for i in range(n):
    arr = input().rstrip().split()
    print(arr)
    kappa = {arr[0]: int(arr[1])}
    print(str(kappa))



print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 9 - Recursion 3

print("---------- Day 9 ----------")

# import math
# import os
# import random
# import re
# import sys
#
# # Complete the factorial function below.
# def factorial(n):
#     if n <= 1:
#         result = 1
#     else:
#         result = n * factorial(n-1)
#     return result
#
# if __name__ == '__main__':
#     #fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     n = int(input())
#     result = factorial(n)
#     print(result)
#     #fptr.write(str(result) + '\n')
#     #fptr.close()
