### This script in Python is for Visualization data from Orange POPC II program - Fiber everywhere
### By: Szymon Baczyński

import pandas as pd
import folium

# 1 Step = Import Excel file
dane = pd.read_excel("lista-punktow-adresowych-popc-nabor-ii-gotowych-sprzedazowo-w-terminie-nie-mniej-niz-30-dni-18.11.2020.xlsx", header=None).iloc[2:, 0:]
dane = dane.rename(columns = dane.iloc[0,:])
dane = dane[1:]
print(dane.head())

# 2 Step = Find coordinates for specific "Gmina"
jaktorow = dane[dane["Gmina"].str.match("JAKTORÓW")]
geodata = jaktorow.loc[:,["Szerokość", "Długość"]]
cols = geodata.columns
geodata[cols] = geodata[cols].apply(pd.to_numeric, errors='coerce')

# 3 Step = Visualization coordinates on map
# Make an empty map
jaktorow_geo = [52.079488, 20.551613]
m = folium.Map(location=jaktorow_geo, tiles='OpenStreetMap', zoom_start=15)
# m = folium.Map(location=[geodata['Szerokość'].mean(), geodata['Długość'].mean()], tiles='OpenStreetMap', zoom_start=15)

# I can add marker one by one on the map
for i in range(0, len(geodata)):
    folium.Marker([geodata.iloc[i]['Szerokość'], geodata.iloc[i]['Długość']], popup=geodata.iloc[i,:]).add_to(m)

# Save it as html
m.save('mapka.html')
