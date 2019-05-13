# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines

# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 0 - Mean, Median and Mode

print("---------- Task 1 ----------")

N = input("Write N number:")    # Input N numbers in list
N = int(N)                      # Convert input to inr
licz = input("Numbers in list (with space):")   # Input list
num = licz.split()              # Separate numbers in input
num = [int(i) for i in num]     # Change list of strings to int
num = sorted(num)               # List sort

mean = sum(num) / N             # Mean calculate

if N%2==0:
    median = (num[(int)(N/2-1)]+num[(int)(N/2)])/2.0    # Median calculate for even N
else:
    median = num[int(N/2)]                              # Median caltulate for odd N

mode = max(num, key=num.count)                          # Find smallest number which occurence the most

print(round(mean,1))                     # Print Mean, Median and Mode
print(round(median,1))
print(mode)

# -------------------------------------------------------------------------------------------------------- #

# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 0 - Weighted Mean

print("---------- Task 2 ----------")

N = int(input())                             # Input N numbers in List
number = list(map(int, input().split()))     # Input numbers
weight = list(map(int, input().split()))     # input Weights

wemean = 0
wei = 0

for x in range(N):                      # Loop for calculate Weighted Mean
    wemean += number[x]*weight[x]
    wei += weight[x]

finalmean = wemean / wei

print(round(finalmean,1))
