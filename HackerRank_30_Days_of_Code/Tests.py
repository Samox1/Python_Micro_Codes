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


# Replace all ______ with rjust, ljust or center.

thickness = int(input()) #This must be an odd number
c = 'H'

#Top Cone
for i in range(thickness):
    print((c*i).______(thickness-1)+c+(c*i).______(thickness-1))

#Top Pillars
for i in range(thickness+1):
    print((c*thickness).______(thickness*2)+(c*thickness).______(thickness*6))

#Middle Belt
for i in range((thickness+1)//2):
    print((c*thickness*5).______(thickness*6))

#Bottom Pillars
for i in range(thickness+1):
    print((c*thickness).______(thickness*2)+(c*thickness).______(thickness*6))

#Bottom Cone
for i in range(thickness):
    print(((c*(thickness-i-1)).______(thickness)+c+(c*(thickness-i-1)).______(thickness)).______(thickness*6))