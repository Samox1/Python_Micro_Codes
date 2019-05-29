# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines


# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Basic Data Types -> List

# if __name__ == '__main__':
#     N = int(input())
#     L = []
#
#     for i in range(0, N):
#         in_put = list(input().rstrip().split())
#         # print(in_put)
#
#         if in_put[0] == "insert":
#             L.insert(int(in_put[1]),int(in_put[2]))
#         elif in_put[0] == "print":
#             print(L)
#         elif in_put[0] == "remove":
#             L.remove(int(in_put[1]))
#         elif in_put[0] == "append":
#             L.append(int(in_put[1]))
#         elif in_put[0] == "sort":
#             L.sort()
#         elif in_put[0] == "pop":
#             L.pop(len(L)-1)
#         elif in_put[0] == "reverse":
#             L.reverse()

# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> Find a string

# def count_substring(string, sub_string):
#     counter = 0
#     for i in range(0, len(string)):
#         index = string.find(sub_string)
#         if index >= 0:
#             # print(index)
#             # print(index)
#             counter += 1
#             string = string[index+1:]
#
#     return counter
#
# if __name__ == '__main__':
#     string = input().strip()
#     sub_string = input().strip()
#
#     count = count_substring(string, sub_string)
#     print("Count:",count)


# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> String Validators

# if __name__ == '__main__':
#     s = input()
#     num = False
#     alfa = False
#     digit = False
#     lower = False
#     upper = False
#
#     for i in s:
#         if i.isalnum() == True:
#             num = True
#         if i.isalpha() == True:
#             alfa = True
#         if i.isdigit() == True:
#             digit = True
#         if i.islower() == True:
#             lower = True
#         if i.isupper() == True:
#             upper = True
#
#     print(num)
#     print(alfa)
#     print(digit)
#     print(lower)
#     print(upper)


# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> Text Alignment

# # Replace all ______ with rjust, ljust or center.
# thickness = int(input()) #This must be an odd number
# # thickness = 5
# c = 'H'
#
# if thickness%2 == 1:
#     #Top Cone
#     for i in range(thickness):
#         print((c*i).rjust(thickness-1)+c+(c*i).ljust(thickness-1))
#
#     #Top Pillars
#     for i in range(thickness+1):
#         print((c*thickness).center(thickness*2)+(c*thickness).center(thickness*6))
#
#     #Middle Belt
#     for i in range((thickness+1)//2):
#         print((c*thickness*5).center(thickness*6))
#
#     #Bottom Pillars
#     for i in range(thickness+1):
#         print((c*thickness).center(thickness*2)+(c*thickness).center(thickness*6))
#
#     #Bottom Cone
#     for i in range(thickness):
#         print(((c*(thickness-i-1)).rjust(thickness)+c+(c*(thickness-i-1)).ljust(thickness)).rjust(thickness*6))


# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> Text Wrap

# import textwrap
#
# def wrap(string, max_width):
#     return textwrap.fill(string,width=max_width)
#
# if __name__ == '__main__':
#     string, max_width = input(), int(input())
#     result = wrap(string, max_width)
#     print(result)


# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> Designer Door Mat

# if __name__ == '__main__':
#     size = list(map(int,input().rstrip().split())) #This must be an odd number
#     welcome = "-WELCOME-"
#     myl = "---"
#     stat = ".|."
#
#     if size[0]>5 and size[0]%2==1 and size[1]==(size[0]*3):
#         maximum = (size[0]//2)
#         for i in range(0,maximum):
#             print(myl*(maximum-i) + (2*i+1)*stat + myl*(maximum-i))
#         print(myl*(maximum-1) + welcome + myl*(maximum-1))
#         for i in range(0,maximum):
#             print(myl*(i+1) + (2*(maximum)-(2*i+1))*stat + myl*(i+1))


# --------------------------------------------------------------- #

# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python -> Strings -> String Formatting

def print_formatted(number):
    octal = len(str(oct(number)[2:]))
    hexa = len(str(hex(number)[2:]))
    binar = len(str(bin(number)[2:]))

    for i in range(1,number+1):
        print(str(i).rjust(binar) + " " + str(oct(i)[2:]).rjust(binar) + " " + str(hex(i)[2:]).rjust(binar).upper() + " " + str(bin(i)[2:]).rjust(binar))

if __name__ == '__main__':
    n = int(input())
    print_formatted(n)