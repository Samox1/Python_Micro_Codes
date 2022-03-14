import string
from time import sleep
from matplotlib.image import imread
import pandas
import numpy as np
import cv2
import pyautogui
import pytesseract


# ZROBIC PLIK KONFIGURACYJNY

def Region_to_read(what_string):
    print("Move mouse to left-top corner of: " + what_string)
    sleep(3)
    position_left_top = pyautogui.position()
    print("OKEY")
    sleep(2)
    print("Move mouse to right-bottom corner of: " +  + what_string)
    sleep(3)
    position_right_bottom = pyautogui.position()
    print("OKEY")

    position_find_screen = pyautogui.screenshot(region = (position_left_top[0], position_left_top[1], position_right_bottom[0] - position_left_top[0], position_right_bottom[1] - position_left_top[1]))
    position_find_screen_cv = cv2.cvtColor(np.array(position_find_screen), cv2.COLOR_RGB2BGR)

    return(position_find_screen_cv)


def Show_and_Save(position_find_screen_cv, what_string, flag_show, flag_save):

    if flag_save:
        cv2.imwrite(string(what_string + "_PY_script.png"), position_find_screen_cv)

    if flag_show:
        cv2.imshow("image", position_find_screen_cv)
        cv2.waitKey(0)


# set Tesseract-OCR -> EXE directory
pytesseract.pytesseract.tesseract_cmd = r'C:/Program Files/Tesseract-OCR/tesseract.exe'


# GOLD - ITEMS

print("Move mouse to left-top corner of items")
sleep(3)
Gold_items_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of items")
sleep(3)
Gold_items_right_bottom = pyautogui.position()
print("OKEY")

Gold_items_find_screen = pyautogui.screenshot(region = (Gold_items_left_top[0], Gold_items_left_top[1], Gold_items_right_bottom[0] - Gold_items_left_top[0], Gold_items_right_bottom[1] - Gold_items_left_top[1]))
Gold_items_find_screen_cv = cv2.cvtColor(np.array(Gold_items_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Gold_items_find_screen_cv)
# cv2.waitKey(0)

Gold_Items_table = pytesseract.image_to_string(cv2.cvtColor(Gold_items_find_screen_cv, cv2.COLOR_RGB2GRAY) )
Gold_Items_List = Gold_Items_table.split("\n")
Gold_Items_List_new = list()
for item in Gold_Items_List:
    if item != "" and not(item.startswith("[Sold")):
        Gold_Items_List_new.append(item)
print(Gold_Items_List_new)

sleep(2)

# GOLD - Lowest Price

print("Move mouse to left-top corner of GOLD - Lowest Price")
sleep(3)
Gold_lowest_price_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of GOLD - Lowest Price")
sleep(3)
Gold_lowest_price_right_bottom = pyautogui.position()
print("OKEY")

Gold_lowest_price_find_screen = pyautogui.screenshot(region = (Gold_lowest_price_left_top[0], Gold_lowest_price_left_top[1], Gold_lowest_price_right_bottom[0] - Gold_lowest_price_left_top[0], Gold_lowest_price_right_bottom[1] - Gold_lowest_price_left_top[1]))
Gold_lowest_price_find_screen_cv = cv2.cvtColor(np.array(Gold_lowest_price_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Gold_lowest_price_find_screen_cv)
# cv2.waitKey(0)

Gold_lowest_price_table = pytesseract.image_to_string(cv2.cvtColor(Gold_lowest_price_find_screen_cv, cv2.COLOR_RGB2GRAY) )
Gold_lowest_price_List = Gold_lowest_price_table.split("\n")
Gold_lowest_price_List = [x for x in Gold_lowest_price_List if x.strip()]
print(Gold_lowest_price_List)


TABLE = pandas.DataFrame({"Items": Gold_Items_List_new, 
                        "Lowest Gold": Gold_lowest_price_List, 
                        "Shard units": '',
                        "Shard price": ''})
print(TABLE)



# SHARD ITEMS

# Shard Item - 1
print("Move mouse to left-top corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_right_bottom = pyautogui.position()
print("OKEY")

Shard_Item_1_find_screen = pyautogui.screenshot(region = (Shard_Item_1_left_top[0], Shard_Item_1_left_top[1], Shard_Item_1_right_bottom[0] - Shard_Item_1_left_top[0], Shard_Item_1_right_bottom[1] - Shard_Item_1_left_top[1]))
Shard_Item_1_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Shard_Item_1_find_screen_cv)
# cv2.waitKey(0)

Shard_Item_1_page_1 = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_find_screen_cv, cv2.COLOR_RGB2GRAY) )
print(Shard_Item_1_page_1)

# Shard Item - How many - 1
print("Move mouse to left-top corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_units_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_units_right_bottom = pyautogui.position()
print("OKEY")

Shard_Item_1_units_find_screen = pyautogui.screenshot(region = (Shard_Item_1_units_left_top[0], Shard_Item_1_units_left_top[1], Shard_Item_1_units_right_bottom[0] - Shard_Item_1_units_left_top[0], Shard_Item_1_units_right_bottom[1] - Shard_Item_1_units_left_top[1]))
Shard_Item_1_units_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_units_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Shard_Item_1_units_find_screen_cv)
# cv2.waitKey(0)


Shard_Item_1_page_1_units = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_units_find_screen_cv, cv2.COLOR_RGB2GRAY) )
print(Shard_Item_1_page_1_units)


# Shard Item Price - 1
print("Move mouse to left-top corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_price_left_top = pyautogui.position()
print("OKEY")
sleep(2)
print("Move mouse to right-bottom corner of SHARD - Item #1")
sleep(3)
Shard_Item_1_price_right_bottom = pyautogui.position()
print("OKEY")

Shard_Item_1_price_find_screen = pyautogui.screenshot(region = (Shard_Item_1_price_left_top[0], Shard_Item_1_price_left_top[1], Shard_Item_1_price_right_bottom[0] - Shard_Item_1_price_left_top[0], Shard_Item_1_price_right_bottom[1] - Shard_Item_1_price_left_top[1]))
Shard_Item_1_price_find_screen_cv = cv2.cvtColor(np.array(Shard_Item_1_price_find_screen), cv2.COLOR_RGB2BGR)

# show found area with gold trading window
# cv2.imshow("image", Shard_Item_1_price_find_screen_cv)
# cv2.waitKey(0)

Shard_Item_1_page_1_price = pytesseract.image_to_string(cv2.cvtColor(Shard_Item_1_price_find_screen_cv, cv2.COLOR_RGB2GRAY) )
print(Shard_Item_1_page_1_price)

TABLE.at[TABLE["Items"] == Shard_Item_1_page_1, "Shard units"] = Shard_Item_1_page_1_units
TABLE.at[TABLE["Items"] == Shard_Item_1_page_1, "Shard units"] = Shard_Item_1_page_1_price
print(TABLE)
