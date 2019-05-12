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

N = int(input())                             # Input N numbers in List
number = list(map(int, input().split()))     # Input numbers
weight = list(map(int, input().split()))     # Input wights

S = list()
for i in range(N):
    S.extend([number[i]] * weight[i])

# print(S)
S = sorted(S)
# print(S)

N = len(S)

if (N)%2==0:
    Q2 = (S[(int)(N/2-1)]+S[(int)(N/2)])/2.0    # Median calculate for even N
    number1 = S[:int(N/2)]
    number3 = S[int(N/2):]
else:
    Q2 = S[int(N/2)]
    number1 = S[:int(N/2)]
    number3 = S[int(N/2+1):]

M = len(number1)

if (len(number1))%2==0 and (len(number3))%2==0:
    Q1 = (number1[(int)(M/2-1)]+number1[(int)(M/2)])/2.0    # Median calculate for Q1
    Q3 = (number3[(int)(M/2-1)]+number3[(int)(M/2)])/2.0    # Median calculate for Q3
else:
    Q1 = number1[int(M/2)]
    Q3 = number3[int(M/2)]

# print(number1)
# print(number3)
# print(Q1, Q3)
print(Q3-Q1)


# -------------------------------------------------------------------------------------------------------- #

# Statistics Training
# You can find this on: HackerRank -> Challenges (Tutorials) -> 10 Days of Statistics
# This is: Day 1 - Standard Deviation

print("---------- Task 3 ----------")

N = int(input())                             # Input N numbers in List
number = list(map(int, input().split()))     # Input numbers

mean = sum(number) / N
# print(mean)

diff = [i-mean for i in number]
stddev = (sum([i**2 for i in diff]) / N)**(1/2.0)
print(round(stddev,1))


# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines

