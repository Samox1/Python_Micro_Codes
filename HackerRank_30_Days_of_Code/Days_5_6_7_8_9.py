# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 5 - Loops

print("---------- Day 5 ----------")

import math
import os
import random
import re
import sys

if __name__ == '__main__':
    n = int(input())

    for i in range(1,11):
        buf = "%d x %d = %d" % (n, i, n*i)
        print(buf)


print("-----------------------------------------------------------------------------------")