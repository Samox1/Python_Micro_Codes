# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 1 - Quartiles

print("---------- Task 1 ----------")

N = int(input())                             # Input N numbers in List
number = list(map(int, input().split()))     # Input numbers
number = sorted(number)
#print(number)

if N%2==0:
    Q2 = (number[(int)(N/2-1)]+number[(int)(N/2)])/2.0    # Median calculate for even N
    number1 = number[:int(N/2)]
    number3 = number[int(N/2):]
else:
    Q2 = number[int(N/2)]
    number1 = number[:int(N/2)]
    number3 = number[int(N/2+1):]

#print(number1)
#print(number3)

M=len(number1)

if (M)%2==0:
    Q1 = (number1[(int)(M/2-1)]+number1[(int)(M/2)])/2.0    # Median calculate for Q1
    Q3 = (number3[(int)(M/2-1)]+number3[(int)(M/2)])/2.0    # Median calculate for Q3
else:
    Q1 = number1[int(M/2)]
    Q3 = number3[int(M/2)]

print(Q1)
print(Q2)
print(Q3)

# -------------------------------------------------------------------------------------------------------- #

# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 1 - Interquartile Range

print("---------- Task 2 ----------")




# -------------------------------------------------------------------------------------------------------- #

# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 1 - Standard Deviation

print("---------- Task 3 ----------")

