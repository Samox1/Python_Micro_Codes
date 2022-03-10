from multiprocessing.connection import wait
from time import sleep
from matplotlib.image import imread
import numpy as np
import cv2
import pyautogui

# sleep(2)
image = pyautogui.screenshot()
image = cv2.cvtColor(np.array(image), cv2.COLOR_RGB2BGR)

cv2.imwrite("image1.png", image)

image2 = cv2.imread("Trade_GOLD.png")

# methods = [cv2.TM_CCOEFF, cv2.TM_CCOEFF_NORMED, cv2.TM_CCORR,
#             cv2.TM_CCORR_NORMED, cv2.TM_SQDIFF, cv2.TM_SQDIFF_NORMED] 
# 
# for method in methods:
#     src2 = src.copy()
#     result = cv2.matchTemplate(src2, temp, method)
#     min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
#     print(min_loc, max_loc)
#     if method in [cv2.TM_SQDIFF,cv2.TM_CCORR]:
#       lacation = min_loc
#     else:
#       location = max_loc
#     bottom_right = (location[0] + W, location[1] + H)
#     cv2.rectangle(src2, location,bottom_right, 255, 5)
#     cv2_imshow(src2)
#     cv2.waitKey(0)
#     cv2.destroyAllWindows() 