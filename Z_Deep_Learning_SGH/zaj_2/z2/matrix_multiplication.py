import numpy as np

A = np.random.randint(low = 0,high = 10, size = [3,4])
B = np.random.randint(low = 0,high = 10, size = [4,5])

C = np.matmul(A,B)


A1 = np.expand_dims(A, axis=2) #3, 4, 1
B1 = np.expand_dims(B, axis=0) #1, 4, 5
# B1 = B
print(A1.shape)
print(B1.shape)

C1 = A1 * B1
print(C1.shape)

C2 = np.sum(C1,axis=1)

print(C-C2)