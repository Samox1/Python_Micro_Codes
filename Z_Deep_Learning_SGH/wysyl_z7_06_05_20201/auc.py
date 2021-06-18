from sklearn.metrics import roc_auc_score
from openpyxl import load_workbook
import pandas as pd
import numpy as np

file = 'D:/Zajecia/Lato_2020_2021/Deep Learning/z6/AUC.xlsx'
wb = load_workbook(file, data_only=True)
sheet_names = wb.get_sheet_names()
sheet_ranges = wb[sheet_names[0]]
df = pd.DataFrame(sheet_ranges.values)

df_for_roc = df.iloc[1:,[2,3]]
df_for_roc.columns=['y_pred', 'y_true']
excel_AUC = df.iloc[13,16]
print(np.sum(df_for_roc['y_true']))
# print(np.unique(df_for_roc['y_true']))
# print(np.unique(df_for_roc['y_pred']))
y_true = np.array(df_for_roc['y_true']).astype(np.int)
y_pred = np.array(df_for_roc['y_pred']).astype(np.float)
AUC = roc_auc_score(y_true=y_true, y_score=y_pred)
print(f"AUC = {AUC}, excel_AUC = {excel_AUC}")


