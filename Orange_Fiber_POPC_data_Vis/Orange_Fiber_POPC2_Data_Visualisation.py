### This script in Python is for Visualization data from Orange POPC II program - Fiber everywhere
### By: Szymon Baczyński

import pandas as pd
import folium
from scrapy import Selector
import requests

# 1 Step = Import Excel file
dane = pd.read_excel("lista-punktow-adresowych-popc-nabor-ii-gotowych-sprzedazowo-w-terminie-nie-mniej-niz-30-dni-18.11.2020.xlsx", header=None).iloc[2:, 0:]
dane = dane.rename(columns = dane.iloc[0,:])
dane = dane[1:]
print(dane.head())

orange_server = 'https://www.hurt-orange.pl'
orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
orange_fiber_site = requests.get(orange_url).content
sel = Selector(text = orange_fiber_site)

# /html/body/div[3]/div[8]/div/div/div/main/article/div/div/div/div[6]/div/div/div[4]/div/ul/li[29]/a
#css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) > li:nth-child(29) > a:nth-child(2)'
css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'
plik = sel.css(css_test).xpath('@href').extract()
#plik = sel.css(css_test).extract()
print(plik)                             # Wszystkie linki do plikow na stronie Oragne z adresami do swiatlowodu
print(len(plik))

# link_pobierania = orange_server + plik[0]
# print(link_pobierania)
# dane_z_orange = pd.read_excel(link_pobierania, header=None).iloc[2:, 0:]
# print(dane_z_orange)


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
    folium.Marker([geodata.iloc[i]['Szerokość'], geodata.iloc[i]['Długość']], popup=geodata.iloc[i,:], icon=folium.Icon(color='green')).add_to(m)

# Save it as html
m.save('mapka.html')
