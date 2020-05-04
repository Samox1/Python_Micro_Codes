import logging
logging.basicConfig(level=logging.INFO)
from PyCIM import cimread
import pandas
import xlsxwriter
import xml.etree.ElementTree as ET

print("Hello, This is CIM import parser")

d = cimread('D:\Programming\Python_Micro_Codes\CIM_import_program\MIKRONIKA_23A10h_short.xml')
print(d)
# g = pandas.DataFrame(list(d.items()))
# print(g)
# g.to_excel("output.xlsx", engine='xlsxwriter')

# tree = ET.parse('D:\Programming\Python_Micro_Codes\CIM_import_program\MIKRONIKA_23A10h_short.xml')
# root = tree.getroot()
# print(root.tag)
# print(root.attrib)
# for child in root:
#     print(child.tag, child.attrib)