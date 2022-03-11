from time import sleep
from matplotlib.image import imread
import pandas
import numpy as np
import cv2
import pyautogui
import pytesseract


# ZROBIC PLIK KONFIGURACYJNY

# GOLD - ITEMS

print("Move mouse to left-top corner of items")
sleep(3)
Gold_items_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of items")
sleep(3)
Gold_items_right_bottom = pyautogui.position()

Gold_items_find_screen = pyautogui.screenshot(region = (Gold_items_left_top[0], Gold_items_left_top[1], Gold_items_right_bottom[0] - Gold_items_left_top[0], Gold_items_right_bottom[1] - Gold_items_left_top[1]))
Gold_items_find_screen_cv = cv2.cvtColor(np.array(Gold_items_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Gold_items_find_screen_cv)
# cv2.waitKey(0)

pytesseract.pytesseract.tesseract_cmd = r'C:/Program Files/Tesseract-OCR/tesseract.exe'
Gold_Items_table = pytesseract.image_to_string(cv2.cvtColor(Gold_items_find_screen_cv, cv2.COLOR_RGB2GRAY) )
Gold_Items_List = Gold_Items_table.split("\n")
Gold_Items_List_new = list()
for item in Gold_Items_List:
    if item != "" and not(item.startswith("[Sold")):
        Gold_Items_List_new.append(item)
print(Gold_Items_List_new)

sleep(2)
# GOLD - Recent Price

print("Move mouse to left-top corner of Avg. Day Price")
sleep(3)
Gold_recent_price_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of Avg. Day Price")
sleep(3)
Gold_recent_price_right_bottom = pyautogui.position()

Gold_recent_price_find_screen = pyautogui.screenshot(region = (Gold_recent_price_left_top[0], Gold_recent_price_left_top[1], Gold_recent_price_right_bottom[0] - Gold_recent_price_left_top[0], Gold_recent_price_right_bottom[1] - Gold_recent_price_left_top[1]))
Gold_recent_price_find_screen_cv = cv2.cvtColor(np.array(Gold_recent_price_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Gold_recent_price_find_screen_cv)
# cv2.waitKey(0)

pytesseract.pytesseract.tesseract_cmd = r'C:/Program Files/Tesseract-OCR/tesseract.exe'
Gold_recent_price_table = pytesseract.image_to_string(cv2.cvtColor(Gold_recent_price_find_screen_cv, cv2.COLOR_RGB2GRAY) )
Gold_recent_price_List = Gold_recent_price_table.split("\n")
Gold_recent_price_List = [x for x in Gold_recent_price_List if x.strip()]
print(Gold_recent_price_List)


TABLE = pandas.DataFrame({"Items":Gold_Items_List_new, "Recent Gold":Gold_recent_price_List})
print(TABLE)