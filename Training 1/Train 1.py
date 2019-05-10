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

#print(N)
#print(num)
print(mean)                     # Print Mean, Median and Mode
print(median)
print(mode)