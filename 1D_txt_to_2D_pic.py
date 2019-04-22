import numpy as np
import scipy.misc

row = 24
col = 32


IN = np.genfromtxt("1D_to_2D_pic.txt", dtype=float ,delimiter='\n')
OUT = np.reshape(IN, (-1,col))
#min = min(OUT)
max = np.amax(OUT)

#print(OUT)
OUT = OUT * 255.0 / max
#print(OUT)

OUT.astype(int)

name = "OUT" + ".png"
scipy.misc.imsave(name, OUT)