from numpy import array
from cv2 import imread, imwrite, imshow, waitKey, cvtColor, COLOR_RGB2BGR, COLOR_RGB2GRAY
import pytesseract 
from pyautogui import position, screenshot


### TODO: ZROBIC PLIK KONFIGURACYJNY

def Region_to_Read(what_string):
    print("Move mouse to left-top corner of: " + what_string)
    input("And press ENTER")
    position_left_top = position()
    print("OKEY")
    print("Move mouse to right-bottom corner of: " + what_string)
    input("And press ENTER")
    position_right_bottom = position()
    print("OKEY")

    position_find_screen = screenshot(region = (position_left_top[0], position_left_top[1], position_right_bottom[0] - position_left_top[0], position_right_bottom[1] - position_left_top[1]))
    position_find_screen_cv = cvtColor(array(position_find_screen), COLOR_RGB2BGR)

    return(position_find_screen_cv)



def Show_and_Save(position_find_screen_cv, what_string, flag_show, flag_save):

    filename = what_string + "_PY_script.png"

    if flag_save:
        imwrite(filename, position_find_screen_cv)

    if flag_show:
        imshow(filename, position_find_screen_cv)
        waitKey(0)



def Region_to_Text(what_string, flag_show = 1, flag_save = 1):
    position = Region_to_Read(what_string)
    Show_and_Save(position, what_string, flag_show, flag_save)
    found_text = pytesseract.image_to_string(cvtColor(position, COLOR_RGB2GRAY) )
    found_text = found_text.split("\n")
    return found_text



def print_menu():
    print("\n")
    print("1 - Gold Items")
    print("2 - Gold Items - price")
    print("3 - Shard Item")
    print("4 - Shard Item - units")
    print("5 - Shard Item - price")
    print("6 - Show TABLE")



# set Tesseract-OCR -> EXE directory
pytesseract.pytesseract.tesseract_cmd = r'C:/Program Files/Tesseract-OCR/tesseract.exe'

print("HEllo")
gold_items = ''
gold_items_price = ''

table_all_items = list()

while True:
    
    print_menu()
    option = ''

    try:
        option = int(input('Enter your choice: '))
    except:
        print('Wrong input. Please enter a number ...')

    if option == 1:
        gold_items = Region_to_Text("Gold_items")
        gold_items = [x for x in gold_items if x.strip() and not(x.startswith("[Sold"))]
        table_all_items.append(gold_items)
        print(gold_items)
    elif option == 2:
        gold_items_price = Region_to_Text("Gold_items_price")
        gold_items_price = [x for x in gold_items_price if x.strip()]
        print(gold_items_price)
        table_all_items.append(gold_items_price)
    elif option == 3:
        print("1")
    elif option == 4:
        print("1")
    elif option == 5:
        print("1")
    elif option == 6:
        print("1")
    elif option == 9:
        print(gold_items)
        print(gold_items_price)
        print("Table - all items with price")
        print(table_all_items)
    elif option == 0:
        print('Thanks message before exiting')
        exit()
    else:
        print('Invalid option. Please enter a number between 1 and 4.')





