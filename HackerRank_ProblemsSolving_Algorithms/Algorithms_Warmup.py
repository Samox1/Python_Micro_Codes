# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines

print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Solve me first

# print("---------- Task 1 ----------")
#
# def solveMeFirst(a,b):
#     return a+b
#
# num1 = int(input())
# num2 = int(input())
# res = solveMeFirst(num1,num2)
# print(res)


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Simple Array Sum

# print("---------- Task 2 ----------")
#
# import os
# import sys
#
# # Complete the simpleArraySum function below.
# def simpleArraySum(ar_count, ar):
#     # Write your code here.
#     suma = 0
#     for i in range(ar_count):
#         suma += ar[i]
#     return suma
#
# if __name__ == '__main__':
#     fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     ar_count = int(input())
#     ar = list(map(int, input().rstrip().split()))
#     result = simpleArraySum(ar_count, ar)
#     fptr.write(str(result) + '\n')
#     fptr.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Compare the Triplets

# print("---------- Task 3 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the compareTriplets function below.
# def compareTriplets(a, b):
#     win = [0,0]
#     for i in range(len(a)):
#         if a[i] > b[i]:
#             win[0] += 1
#         if b[i] > a[i]:
#             win[1] += 1
#     return win
#
# if __name__ == '__main__':
#     fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     a = list(map(int, input().rstrip().split()))
#     b = list(map(int, input().rstrip().split()))
#     result = compareTriplets(a, b)
#     fptr.write(' '.join(map(str, result)))
#     fptr.write('\n')
#     fptr.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - A Very Big Sum

# print("---------- Task 4 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the aVeryBigSum function below.
# def aVeryBigSum(ar_count, ar):
#     suma = 0.0
#     for i in range(ar_count):
#         suma += ar[i]
#     return suma
#
# if __name__ == '__main__':
#     fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     ar_count = int(input())
#     ar = list(map(float, input().rstrip().split()))
#     result = round(aVeryBigSum(ar_count, ar))
#     fptr.write(str(result) + '\n')
#     fptr.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Plus Minus

print("---------- Task 5 ----------")

import math
import os
import random
import re
import sys

# Complete the plusMinus function below.
def plusMinus(arr):
    positive = 0.0
    negative = 0.0
    zeros = 0.0

    for i in arr:
        if i > 0:
            positive += 1
        elif i < 0:
            negative += 1
        else:
            zeros += 1

    m = len(arr)

    print(round(positive/m,6))
    print(round(negative/m,6))
    print(round(zeros/m,6))


if __name__ == '__main__':
    # n = 6
    # arr = [-4, 3, -9, 0, 4, 1]
    n = int(input())
    arr = list(map(int, input().rstrip().split()))
    plusMinus(arr)


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Staircase

print("---------- Task 6 ----------")


