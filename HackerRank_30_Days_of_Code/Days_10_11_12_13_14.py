# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines


# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 10 - Binary Numbers

print("---------- Day 10 ----------")

# import math
# import os
# import random
# import re
# import sys
#
# if __name__ == '__main__':
#     n = int(input())
#     binary = str(bin(n)[2:])
#     print(binary)
#     count = 0
#     count_max = 0
#
#     for i in range(0,len(binary)):
#         if binary[i]=='1':
#             count += 1
#             # print(count)
#             if count > count_max:
#                 count_max = count
#                 # print("MAX: ", count_max)
#         else:
#             count = 0
#
#     print(count_max)


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 11 - 2D Arrays

print("---------- Day 11 ----------")











# # Python Tutorial:
# if __name__ == '__main__':
#     # n = int(input())
#     arr = list(map(int, input().split()))
#
#     arr = sorted(arr, reverse=True)
#     print(arr)
#
#     for i in arr:
#         if i < max(arr):
#             second = i
#             break
#
#     print(second)


# # Codility Test for TCL:

# # Python program to print all permutations with
# # duplicates allowed
#
# def toString(List):
#     return ''.join(List)
#
# # Function to print permutations of string
# # This function takes three parameters:
# # 1. String
# # 2. Starting index of the string
# # 3. Ending index of the string.
# def permute(a, l, r):
#     MAX = []
#     Final = []
#
#     if l == r:
#         MAX.append(int(toString(a)))
#     else:
#         for i in range(l, r + 1):
#             a[l], a[i] = a[i], a[l]
#             permute(a, l + 1, r)
#             a[l], a[i] = a[i], a[l]  # backtrack
#
#     MAX = [t for t in MAX if t]
#     for i in MAX:
#         if i > 0 and i != None:
#             # Final.append(print(i))
#             Final.append(i)
# # Final.
#     print(Final)
#     # return (max(Final))
#
# if __name__ == '__main__':
# # Driver program to test the above function
#     N = 533
#     string = str(N)
#     # string = "ABC"
#     n = len(string)
#     a = list(string)
#     # permute(a, 0, n - 1)
#     print(permute(a, 0, n - 1))


# # Second Task in Codility for TCL:

# counter = 0
#
# def solution(S):
#     # write your code in Python 3.6
#     N = int(S, 2)
#
#     global counter
#     counter += 1
#
#     if N % 2 == 0:
#         N = N // 2
#     else:
#         N = N - 1
#
#     if N != 0:
#         S = str(bin(N))[2:]
#         return solution(S)
#     else:
#         # cout = counter
#         # print(counter)
#         return counter
#
# S = '011100'
# print(solution(S))
