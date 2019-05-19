# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines


# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 10 - Binary Numbers

print("---------- Day 10 ----------")

import math
import os
import random
import re
import sys

if __name__ == '__main__':
    n = int(input())
    binary = str(bin(n)[2:])
    print(binary)
    count = 0
    count_max = 0

    for i in range(0,len(binary)):
        if binary[i]=='1':
            count += 1
            # print(count)
            if count > count_max:
                count_max = count
                # print("MAX: ", count_max)
        else:
            count = 0

    print(count_max)


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 11 - 2D Arrays

print("---------- Day 11 ----------")

