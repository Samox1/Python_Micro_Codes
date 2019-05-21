# Nice shortcuts:
#   Ctrl + R = Replace
#   Ctrl + F = Find
#   Ctrl + D = Duplicate line
#   Ctrl + / = Comment block of lines


# Python Tutorial on HackerRank
# You can find this on: HackerRank -> Practice -> Python
# This is: Python - Basic Data Types - List

if __name__ == '__main__':
    N = int(input())
    L = []

    for i in range(0, N):
        in_put = list(input().rstrip().split())
        # print(in_put)

        if in_put[0] == "insert":
            L.insert(int(in_put[1]),int(in_put[2]))
        elif in_put[0] == "print":
            print(L)
        elif in_put[0] == "remove":
            L.remove(int(in_put[1]))
        elif in_put[0] == "append":
            L.append(int(in_put[1]))
        elif in_put[0] == "sort":
            L.sort()
        elif in_put[0] == "pop":
            L.pop(len(L)-1)
        elif in_put[0] == "reverse":
            L.reverse()

# --------------------------------------------------------------- #
