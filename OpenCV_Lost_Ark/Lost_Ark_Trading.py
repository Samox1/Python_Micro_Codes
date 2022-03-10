from time import sleep
from matplotlib.image import imread
import numpy as np
import cv2
import pyautogui


# 1 - Screenshot of gameplay
# sleep(2)
screen_1 = pyautogui.screenshot()
screen_1 = cv2.cvtColor(np.array(screen_1), cv2.COLOR_RGB2BGR)
cv2.imwrite("image1.png", screen_1)
#screen_1 = cv2.cvtColor(screen_1, cv2.COLOR_RGB2GRAY)

# 2 - Read "Trade_GOLD.png" to compare
trade_gold = cv2.imread("Trade_GOLD.png")
#trade_gold = cv2.cvtColor(trade_gold, cv2.COLOR_RGB2GRAY) 
H, W, kolory = trade_gold.shape


# 3 - Find Trade_GOLD window on screenshot

# methods = [cv2.TM_CCOEFF, cv2.TM_CCOEFF_NORMED, cv2.TM_CCORR,
#             cv2.TM_CCORR_NORMED, cv2.TM_SQDIFF, cv2.TM_SQDIFF_NORMED] 
# 
# for method in methods:
#     screen_1_copy = screen_1.copy()
#     result = cv2.matchTemplate(screen_1_copy, trade_gold, method)
#     min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
#     print(min_loc, max_loc)
#     if method in [cv2.TM_SQDIFF,cv2.TM_CCORR]:
#       location = min_loc
#     else:
#       location = max_loc
#     bottom_right = (location[0] + W, location[1] + H)
#     cv2.rectangle(screen_1_copy, location, bottom_right, 255, 5)
#     cv2.imshow("image", screen_1_copy)
#     cv2.waitKey(0)
#     cv2.destroyAllWindows() 

screen_1_copy = screen_1.copy()
result = cv2.matchTemplate(screen_1_copy, trade_gold, cv2.TM_CCOEFF)
min_val, max_val, min_loc, max_loc = cv2.minMaxLoc(result)
print(min_loc, max_loc)
location = max_loc
bottom_right = (location[0] + W, location[1] + H)

# (left, top, width, height)
trade_gold_find_screen = pyautogui.screenshot(region = (location[0], location[1], W, H))
trade_gold_find_screen_cv = cv2.cvtColor(np.array(trade_gold_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
cv2.imshow("image", trade_gold_find_screen_cv)
cv2.waitKey(0)



