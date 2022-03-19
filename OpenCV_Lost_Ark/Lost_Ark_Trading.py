from time import sleep
from matplotlib.image import imread
import pandas
import numpy as np
import cv2
import pyautogui
import pytesseract


# 1 - Screenshot of gameplay
sleep(2)
screen_1 = pyautogui.screenshot()
screen_1 = cv2.cvtColor(np.array(screen_1), cv2.COLOR_RGB2BGR)
# cv2.imwrite("image1.png", screen_1)
# screen_1 = cv2.cvtColor(screen_1, cv2.COLOR_RGB2GRAY)

# screen_1 = cv2.imread("Z_Screen_GOLD_BG_1.png")

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



pytesseract.pytesseract.tesseract_cmd = r'C:/Program Files/Tesseract-OCR/tesseract.exe'

GOLD_table = pytesseract.image_to_string("Trade_GOLD_test.png")
print(GOLD_table)

print(GOLD_table.split("\n"))

x, y = pyautogui.position()




### OLD CODE

# GOLD - ITEMS

# print("Move mouse to left-top corner of items")
# sleep(3)
# Gold_items_left_top = pyautogui.position()
# print("OKEY")
# sleep(2)
# print("Move mouse to right-bottom corner of items")
# sleep(3)
# Gold_items_right_bottom = pyautogui.position()
# print("OKEY")

# Gold_items_find_screen = pyautogui.screenshot(region = (Gold_items_left_top[0], Gold_items_left_top[1], Gold_items_right_bottom[0] - Gold_items_left_top[0], Gold_items_right_bottom[1] - Gold_items_left_top[1]))
# Gold_items_find_screen_cv = cv2.cvtColor(np.array(Gold_items_find_screen), cv2.COLOR_RGB2BGR)

# # show found area with gold trading window
# # cv2.imshow("image", Gold_items_find_screen_cv)
# # cv2.waitKey(0)

# Gold_Items_table = pytesseract.image_to_string(cv2.cvtColor(Gold_items_find_screen_cv, cv2.COLOR_RGB2GRAY) )
# Gold_Items_List = Gold_Items_table.split("\n")
# Gold_Items_List = [x for x in Gold_Items_List if x.strip() or not(item.startswith("[Sold"))]
# # Gold_Items_List_new = list()
# # for item in Gold_Items_List:
# #     if item != "" and not(item.startswith("[Sold")):
# #         Gold_Items_List_new.append(item)
# print(Gold_Items_List)

# sleep(2)

# # GOLD - Lowest Price

# print("Move mouse to left-top corner of GOLD - Lowest Price")
# sleep(3)
# Gold_lowest_price_left_top = pyautogui.position()
# print("OKEY")
# sleep(2)
# print("Move mouse to right-bottom corner of GOLD - Lowest Price")
# sleep(3)
# Gold_lowest_price_right_bottom = pyautogui.position()
# print("OKEY")

# Gold_lowest_price_find_screen = pyautogui.screenshot(region = (Gold_lowest_price_left_top[0], Gold_lowest_price_left_top[1], Gold_lowest_price_right_bottom[0] - Gold_lowest_price_left_top[0], Gold_lowest_price_right_bottom[1] - Gold_lowest_price_left_top[1]))
# Gold_lowest_price_find_screen_cv = cv2.cvtColor(np.array(Gold_lowest_price_find_screen), cv2.COLOR_RGB2BGR)

# # show found area with gold trading window
# # cv2.imshow("image", Gold_lowest_price_find_screen_cv)
# # cv2.waitKey(0)

# Gold_lowest_price_table = pytesseract.image_to_string(cv2.cvtColor(Gold_lowest_price_find_screen_cv, cv2.COLOR_RGB2GRAY) )
# Gold_lowest_price_List = Gold_lowest_price_table.split("\n")
# Gold_lowest_price_List = [x for x in Gold_lowest_price_List if x.strip()]
# print(Gold_lowest_price_List)


# TABLE = pandas.DataFrame({"Items": Gold_Items_List_new, 
#                         "Lowest Gold": Gold_lowest_price_List, 
#                         "Shard units": '',
#                         "Shard price": ''})
# print(TABLE)



# # SHARD ITEMS

# # Shard Item - 1
# print("Move mouse to left-top corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_left_top = pyautogui.position()
# print("OKEY")
# sleep(2)
# print("Move mouse to right-bottom corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_right_bottom = pyautogui.position()
# print("OKEY")

# Shard_Item_1_find_screen = pyautogui.screenshot(region = (Shard_Item_1_left_top[0], Shard_Item_1_left_top[1], Shard_Item_1_right_bottom[0] - Shard_Item_1_left_top[0], Shard_Item_1_right_bottom[1] - Shard_Item_1_left_top[1]))
# Shard_Item_1_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_find_screen), cv2.COLOR_RGB2BGR)

# # show found area with gold trading window
# # cv2.imshow("image", Shard_Item_1_find_screen_cv)
# # cv2.waitKey(0)

# Shard_Item_1_page_1 = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_find_screen_cv, cv2.COLOR_RGB2GRAY) )
# print(Shard_Item_1_page_1)

# # Shard Item - How many - 1
# print("Move mouse to left-top corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_units_left_top = pyautogui.position()
# print("OKEY")
# sleep(2)
# print("Move mouse to right-bottom corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_units_right_bottom = pyautogui.position()
# print("OKEY")

# Shard_Item_1_units_find_screen = pyautogui.screenshot(region = (Shard_Item_1_units_left_top[0], Shard_Item_1_units_left_top[1], Shard_Item_1_units_right_bottom[0] - Shard_Item_1_units_left_top[0], Shard_Item_1_units_right_bottom[1] - Shard_Item_1_units_left_top[1]))
# Shard_Item_1_units_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_units_find_screen), cv2.COLOR_RGB2BGR)

# # show found area with gold trading window
# # cv2.imshow("image", Shard_Item_1_units_find_screen_cv)
# # cv2.waitKey(0)


# Shard_Item_1_page_1_units = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_units_find_screen_cv, cv2.COLOR_RGB2GRAY) )
# print(Shard_Item_1_page_1_units)


# # Shard Item Price - 1
# print("Move mouse to left-top corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_price_left_top = pyautogui.position()
# print("OKEY")
# sleep(2)
# print("Move mouse to right-bottom corner of SHARD - Item #1")
# sleep(3)
# Shard_Item_1_price_right_bottom = pyautogui.position()
# print("OKEY")

# Shard_Item_1_price_find_screen = pyautogui.screenshot(region = (Shard_Item_1_price_left_top[0], Shard_Item_1_price_left_top[1], Shard_Item_1_price_right_bottom[0] - Shard_Item_1_price_left_top[0], Shard_Item_1_price_right_bottom[1] - Shard_Item_1_price_left_top[1]))
# Shard_Item_1_price_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_price_find_screen), cv2.COLOR_RGB2BGR)

# # show found area with gold trading window
# # cv2.imshow("image", Shard_Item_1_price_find_screen_cv)
# # cv2.waitKey(0)

# Shard_Item_1_page_1_price = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_price_find_screen_cv, cv2.COLOR_RGB2GRAY) )
# print(Shard_Item_1_page_1_price)

# TABLE.at[TABLE["Items"] == Shard_Item_1_page_1, "Shard units"] = Shard_Item_1_page_1_units
# TABLE.at[TABLE["Items"] == Shard_Item_1_page_1, "Shard units"] = Shard_Item_1_page_1_price
# print(TABLE)