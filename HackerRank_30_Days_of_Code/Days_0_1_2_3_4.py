
# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines

# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 0 - Hello, World.

print("---------- Day 0 ----------")

# Read a full line of input from stdin and save it to our dynamically typed variable, input_string.
input_string = input()

# Print a string literal saying "Hello, World." to stdout.
print('Hello, World.')

# TODO: Write a line of code here that prints the contents of input_string to stdout.
print(input_string)


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 1 - Data Types

print("---------- Day 1 ----------")

i = 4
d = 4.0
s = 'HackerRank '
# Declare second integer, double, and String variables.
i2 = 0
d2 = 0.0
s2 = ''

# Read and save an integer, double, and String to your variables.
i2 = input()
i2 = int(i2)
d2 = input()
d2 = float(d2)
s2 = input()

# Print the sum of both integer variables on a new line.
print(i+i2)
# Print the sum of the double variables on a new line.
print(d+d2)
# Concatenate and print the String variables on a new line
print(s+s2)
# The 's' variable above should be printed first.


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 2 - Operators

print("---------- Day 2 ----------")

import math
import os
import random
import re
import sys

# Complete the solve function below.
def solve(meal_cost, tip_percent, tax_percent):
    tip = meal_cost * tip_percent / 100.0
    tax = meal_cost * tax_percent / 100.0
    return(meal_cost+tip+tax)

if __name__ == '__main__':
    meal_cost = float(input())
    tip_percent = int(input())
    tax_percent = int(input())
    cost = solve(meal_cost, tip_percent, tax_percent)
    print(round(cost))


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 3 - Intro to Conditional Statements

print("---------- Day 3 ----------")

import math
import os
import random
import re
import sys

if __name__ == '__main__':
    N = int(input())

    if N%2 == 1:
        print("Weird")
    else:
        if 2<=N and N<=5:
            print("Not Weird")
        if 6<=N and N<=20:
            print("Weird")
        if N>20:
            print("Not Weird")


print("-----------------------------------------------------------------------------------")
# 30 Days of Code
# You can find this on: HackerRank -> Challenges (Tutorials) -> 30 Days of Code
# This is: Day 4 - Class vs. Instance

print("---------- Day 4 ----------")

class Person:
    age = 0

    def __init__(self, initialAge):
        if initialAge < 0:
            print("Age is not valid, setting age to 0.")
            self.age = 0
        else:
            self.age = initialAge
        # Add some more code to run some checks on initialAge

    def amIOld(self):
        if self.age < 13:
            print("You are young.")
        elif self.age >= 13 and self.age < 18:
            print("You are a teenager.")
        else:
            print("You are old.")
        # Do some computations in here and print out the correct statement to the console

    def yearPasses(self):
        self.age += 1
        #print(self.age)
        # Increment the age of the person in here


t = int(input())
for i in range(0, t):
    age = int(input())
    p = Person(age)
    p.amIOld()
    for j in range(0, 3):
        p.yearPasses()
    p.amIOld()
    print("")


print("-----------------------------------------------------------------------------------")

