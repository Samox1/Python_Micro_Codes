# Python OpenCV ver 4 - HAVE FUN

import numpy as np
import matplotlib
import cv2
import time

video = cv2.VideoCapture("wesele.mp4")
check1, frame0 = video.read()
gray0 = cv2.cvtColor(frame0, cv2.COLOR_BGR2GRAY)
frames = video.get(cv2.CAP_PROP_FRAME_COUNT)
fps = video.get(cv2.CAP_PROP_FPS)
width = video.get(cv2.CAP_PROP_FRAME_WIDTH)
height = video.get(cv2.CAP_PROP_FRAME_HEIGHT)
pxs = width * height
print("Frames in video: " + str(int(frames)))
print("Frames per second: " + str(int(fps)))
print("All pixels in frame: "+str(int(width))+"x"+str(int(height))+" = " + str(int(pxs)))
framecount = 1

while True:
    check, frame = video.read()
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    diff = cv2.subtract(gray0, gray)
    pxnon0 = (cv2.countNonZero(diff) / pxs) * 100
    gray0 = gray
    framecount = framecount + 1
    if pxnon0 > 40:
        print(str(framecount) + ": " + str(pxnon0) + "%")

    cv2.imshow("Capture", diff)

    key = cv2.waitKey(1)
    if key == ord('q'):
        break

video.release()
cv2.destroyAllWindows()

