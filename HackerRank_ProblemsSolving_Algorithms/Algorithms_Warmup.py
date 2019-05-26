# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines

print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Solve me first

print("---------- Task 1 ----------")
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

print("---------- Task 2 ----------")
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

print("---------- Task 3 ----------")
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

print("---------- Task 4 ----------")
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
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the plusMinus function below.
# def plusMinus(arr):
#     positive = 0.0
#     negative = 0.0
#     zeros = 0.0
#
#     for i in arr:
#         if i > 0:
#             positive += 1
#         elif i < 0:
#             negative += 1
#         else:
#             zeros += 1
#
#     m = len(arr)
#     print(round(positive/m,6))
#     print(round(negative/m,6))
#     print(round(zeros/m,6))
#
#
# if __name__ == '__main__':
#     # n = 6
#     # arr = [-4, 3, -9, 0, 4, 1]
#     n = int(input())
#     arr = list(map(int, input().rstrip().split()))
#     plusMinus(arr)


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Staircase

print("---------- Task 6 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the staircase function below.
# def staircase(n):
#     for i in range(n):
#         stair = ' ' * (n-i-1)
#         step = '#' * (i+1)
#         print(stair + step)
#
# if __name__ == '__main__':
#     n = int(input())
#     staircase(n)


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Mini-Max Sum

print("---------- Task 7 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the miniMaxSum function below.
# def miniMaxSum(arr):
#     kapa = [None]*(len(arr))
#     suma = sum(arr)
#
#     for i in range(0,len(arr)):
#         kapa[i] = suma - arr[i]
#         #print(kapa[i])
#         #print("--- " + str(arr[i]))
#     minimum = min(kapa)
#     maximum = max(kapa)
#     print(str(minimum) + "  " + str(maximum))
#
# if __name__ == '__main__':
#     # arr = [1,2,3,4,5]
#     arr = list(map(int, input().rstrip().split()))
#     miniMaxSum(arr)


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Birthday Cake Candles

print("---------- Task 8 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the birthdayCakeCandles function below.
# def birthdayCakeCandles(ar):
#     max_element = max(ar)
#     count = ar.count(max_element)
#     return count
#
# if __name__ == '__main__':
#     ar_count = 4
#     ar = [3,2,1,3]
#     result = birthdayCakeCandles(ar)
#     print(result)
#     # fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     # ar_count = int(input())
#     # ar = list(map(int, input().rstrip().split()))
#     # result = birthdayCakeCandles(ar)
#     # fptr.write(str(result) + '\n')
#     # fptr.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Birthday Cake Candles

print("---------- Task 9 ----------")
#
# import os
# import sys
#
# def timeConversion(s):
#     kapa = s.split(':', 4)
#     stamp = kapa[2][2:]
#     kapa[2] = kapa[2].replace('PM','')
#     kapa[2] = kapa[2].replace('AM','')
#     if kapa[0] == '12':
#         kapa[0] = '00'
#
#     if stamp == 'PM':
#         kapa[0] = str(int(kapa[0]) + 12)
#
#     time = kapa[0] + ':' + kapa[1] + ':' + kapa[2]
#     # print(kapa)
#     # print(stamp)
#     # print(time)
#     return time
#
#
# if __name__ == '__main__':
#     s = "12:05:45PM"
#
#     # f = open(os.environ['OUTPUT_PATH'], 'w')
#     # s = input()
#     result = timeConversion(s)
#     print(result)
#     # f.write(result + '\n')
#     # f.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Diagonal Difference

print("---------- Task 10 ----------")
#
# import math
# import os
# import random
# import re
# import sys
#
# # Complete the diagonalDifference function below.
# def diagonalDifference(n,arr):
#     diag1 = 0
#     diag2 = 0
#     for i in range(n):
#         diag1 += arr[i][i]
#         diag2 += arr[i][(n-1)-i]
#
#     diff = abs(diag1 - diag2)
#     return(diff)
#
# if __name__ == '__main__':
#     n = 3
#     arr = [[11, 2, 4], [4, 5, 6], [10, 8, -12]]
#     result = diagonalDifference(n, arr)
#     print(result)
#
#     # fptr = open(os.environ['OUTPUT_PATH'], 'w')
#     # n = int(input())
#     # arr = []
#     # for _ in range(n):
#     #     arr.append(list(map(int, input().rstrip().split())))
#     # result = diagonalDifference(n,arr)
#     # fptr.write(str(result) + '\n')
#     # fptr.close()


print("-----------------------------------------------------------------------------------")
# Problem Solving Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> Problem Solving (Algorithms)
# This is: Problem Solving (Algorithms) - Grading Students

print("---------- Task 11 ----------")

import os
import sys

#
# Complete the gradingStudents function below.
#
def gradingStudents(grades):
    # Write your code here.
    for i in range(len(grades)):
        if grades[i]>=38 and grades[i]%5>=3:
            grades[i]=grades[i]+(5 - grades[i]%5)
    return grades

if __name__ == '__main__':
    # f = open(os.environ['OUTPUT_PATH'], 'w')

    n = int(input())

    grades = []

    for _ in range(n):
        grades_item = int(input())
        grades.append(grades_item)

    result = gradingStudents(grades)

    print(result)                           # as a kolumn (string)
    # f.write('\n'.join(map(str, result)))
    # f.write('\n')
    # f.close()
