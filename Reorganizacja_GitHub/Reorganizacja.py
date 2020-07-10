import os
x = [f.name for f in os.scandir() if f.is_file()]
print(x)


# Another example with scandir (a little variation from docs.python.org)
# This one is more efficient than os.listdir.
# In this case, it shows the files only in the current directory
# where the script is executed.

import os
with os.scandir() as i:
    for entry in i:
        if entry.is_file():
            print(entry.name)
