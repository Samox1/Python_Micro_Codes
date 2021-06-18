import numpy as np
A = np.random.randint(low = 0,high = 10, size = [100 , 5])
B = np.random.randint(low = 0,high = 10, size = [20 , 5])

D = np.zeros((100,20))
for i in range(100):
    for j in range(20):
        D[i,j] = np.sqrt(np.sum(np.power(A[i,:]-B[j,:],2)))



A1 = np.expand_dims(A, axis=1) #100, 1, 5
B1 = np.expand_dims(B, axis=0) #1, 20, 5
C1 = A1 - B1 #(100,20,5)
D1 = np.sqrt(np.sum(np.power(C1,2), axis=2))



print(np.max(np.abs(D-D1)))
nr = np.argmin(D1, axis = 1)
print(nr.shape)
