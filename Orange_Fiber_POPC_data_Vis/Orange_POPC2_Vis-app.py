### This script in Python is for Visualization data from Orange POPC II program - Fiber everywhere
### By: Szymon Baczyński

import streamlit as st
from streamlit_folium import folium_static
import pandas as pd
import numpy
import folium
from scrapy import Selector
import requests
import datetime


def _max_width_():
    max_width_str = f"max-width: 2000px;"
    st.markdown(
        f"""
    <style>
    .reportview-container .main .block-container{{
        {max_width_str}
    }}
    </style>    
    """,
        unsafe_allow_html=True,
    )

#_max_width_()


### 1 - Import Data from Orange website



date_now = datetime.date.today()            ### Wez godzine i rozbij funkcje na 2 --- 1) sprawdza co godzine czy pojaiwlo sie wiecej plikow | 2) jesli jest wiecej plikow to pobierz i przemiel pliki
minute_now = datetime.datetime.now().minute
hour_now = datetime.datetime.now().hour


@st.cache
def Check_Data_on_Orange_Website(time):
    #orange_server = 'https://www.hurt-orange.pl'
    orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
    orange_fiber_site = requests.get(orange_url).content
    sel = Selector(text=orange_fiber_site)
    # css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) > li:nth-child(29) > a:nth-child(2)'
    css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'
    plik = sel.css(css_test).xpath('@href').extract()
    # print(plik)                                                                 # Wszystkie linki do plikow na stronie Oragne z adresami do swiatlowodu
    ile = len(plik)
    # print(ile)
    print('--- Date now (Check_Data_on_Orange_Website) = ' + str(datetime.datetime.now()) + ' ---')
    return plik, ile


@st.cache
def Import_Data_Orange_Fiber(plik, ile):
    orange_server = 'https://www.hurt-orange.pl'
    #orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
    #orange_fiber_site = requests.get(orange_url).content
    #sel = Selector(text = orange_fiber_site)
    ## css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) > li:nth-child(29) > a:nth-child(2)'
    #css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'
    #plik = sel.css(css_test).xpath('@href').extract()
    ##print(plik)                                                                 # Wszystkie linki do plikow na stronie Oragne z adresami do swiatlowodu
    #ile = len(plik)
    ##print(ile)
    print('--- Date now = ' + str(date_now) + ' ---')
    print('*** Extract files names from Orange website ***')

    # columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']

    dane_z_orange = pd.DataFrame(columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali'])
    for i in range(1,len(plik)):
        if (plik[i] != '/wp-content/uploads/2020/11/lista-obszarow-i-miejscowosci-objetych-planami-realizacji-orange-w-ramach-ii-konkursu-popc.xlsx' and plik[i] != '/wp-content/uploads/2020/09/lista-punktow-adresowych-ii-konkurs-popc-1.xlsx'):
            link_pobierania = orange_server + plik[i]
            print(str(i) + ': ' + link_pobierania)
            tymczasowa_tablica = pd.read_excel(link_pobierania, header=[2,]).iloc[2:, 0:]
            ### Dodac kolumne z Data Ostatniego Zapisania pliku Excel
            dane_z_orange = dane_z_orange.append(tymczasowa_tablica, ignore_index=True, sort=False)

    ### 1.2 Step = Take data and make sure if coordinates are numeric values
    dane_z_orange = dane_z_orange[['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']]
    dane_z_orange[["Szerokość", "Długość"]] = dane_z_orange[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')
    dane_z_orange = dane_z_orange[dane_z_orange["Szerokość"].notna()]
    dane_z_orange = dane_z_orange[dane_z_orange["Długość"].notna()]

    Gminy_ALL = dane_z_orange['Gmina'].unique()
    Gminy_ALL.sort()
    Gminy_ALL = list(Gminy_ALL)
    jaktorow_position = Gminy_ALL.index('JAKTORÓW')
    print('*** End of Extract Data ***')
    t1 = datetime.datetime.now()
    time_update = t1.strftime("%d/%m/%Y, godzina: %H:%M")

    return dane_z_orange, Gminy_ALL, jaktorow_position, ile, time_update

plik, ile = Check_Data_on_Orange_Website(hour_now)
dane_z_orange, Gminy_ALL, jaktorow_position, ile, time_update = Import_Data_Orange_Fiber(plik, ile)

### 3.1 - Make Streamlit app

st.title("""Wizualizacja danych Orange POPC2 - Fiber To The Home
Mapa punktów, które zostały lub będą podłączone do sieci światłowodowej """)
st.text(" Plików na stronie = " + str(ile) + '          Ostatnia aktualizacja: ' + str(time_update))


st.sidebar.markdown(""" [Strona Orange, z której pochodzą dane (POPC2)] (https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii/) """)
st.sidebar.header('Wybierz gminę:')
Gmina = st.sidebar.selectbox('Gmina: ', Gminy_ALL, index=jaktorow_position)
st.sidebar.text('\n\n\nMade by SamoX')

# st.sidebar.slider('Do momentu w czasie', ['29-11-2020', '2-12-2020'])


dane_z_orange_jaktorow = dane_z_orange[dane_z_orange["Gmina"].str.contains(Gmina)]
ile_punktow = len(dane_z_orange_jaktorow)

st.text(" Gmina: " + Gmina + "  |  Podłączonych lokalizacji: " + str(ile_punktow))

if (Gmina == "JAKTORÓW"):
    jaktorow_geo = [52.079488, 20.551613]
    jaktorow_pkp_geo = [52.086587629803624, 20.55210270975992]
else:
    jaktorow_geo = [((dane_z_orange_jaktorow['Szerokość'].max() + dane_z_orange_jaktorow['Szerokość'].min())/2.0), ((dane_z_orange_jaktorow['Długość'].max() + dane_z_orange_jaktorow['Długość'].min())/2.0)]
    jaktorow_pkp_geo = [((dane_z_orange_jaktorow['Szerokość'].max() + dane_z_orange_jaktorow['Szerokość'].min())/2.0), ((dane_z_orange_jaktorow['Długość'].max() + dane_z_orange_jaktorow['Długość'].min())/2.0)]

m = folium.Map(location=jaktorow_geo, tiles='OpenStreetMap', zoom_start=13)
popup_text_liczba = 'Gmina: ' + Gmina + '\n' + 'Liczba podłączeń = ' + str(ile_punktow)
folium.Marker([jaktorow_pkp_geo[0], jaktorow_pkp_geo[1]], popup=popup_text_liczba, icon=folium.Icon(color='blue', icon='info-sign')).add_to(m)

# I can add marker one by one on the map
for i in range(0, len(dane_z_orange_jaktorow)):
    popup_text = 'Miejscowość: ' + str(dane_z_orange_jaktorow.iloc[i]['Miejscowość']) + '\nUlica: ' + str(dane_z_orange_jaktorow.iloc[i]['Ulica']) + ' ' + str(dane_z_orange_jaktorow.iloc[i]['Nr '])
    folium.Marker([dane_z_orange_jaktorow.iloc[i]['Szerokość'], dane_z_orange_jaktorow.iloc[i]['Długość']], popup=popup_text, icon=folium.Icon(color='green')).add_to(m)

folium_static(m)



#### 1 Step = Import Excel file from Orange website
#
#orange_server = 'https://www.hurt-orange.pl'
#orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
#orange_fiber_site = requests.get(orange_url).content
#sel = Selector(text = orange_fiber_site)
## css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) > li:nth-child(29) > a:nth-child(2)'
#css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'
#plik = sel.css(css_test).xpath('@href').extract()
#print(plik)                                                                 # Wszystkie linki do plikow na stronie Oragne z adresami do swiatlowodu
#print(str('*** How many files are there on Orange website = ' + len(plik)))
## columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']
#dane_z_orange = pd.DataFrame(columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali'])
#for i in range(1,len(plik)):
#    if (plik[i] != '/wp-content/uploads/2020/11/lista-obszarow-i-miejscowosci-objetych-planami-realizacji-orange-w-ramach-ii-konkursu-popc.xlsx' and plik[i] != '/wp-content/uploads/2020/09/lista-punktow-adresowych-ii-konkurs-popc-1.xlsx'):
#        link_pobierania = orange_server + plik[i]
#        print(link_pobierania)
#        tymczasowa_tablica = pd.read_excel(link_pobierania, header=[2,]).iloc[2:, 0:]
#        dane_z_orange = dane_z_orange.append(tymczasowa_tablica, ignore_index=True, sort=False)
#### 2 Step = Take data and make sure if coordinates are numeric values
#dane_z_orange = dane_z_orange[['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']]
#dane_z_orange[["Szerokość", "Długość"]] = dane_z_orange[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')
#
#
## dane_z_orange = dane_z_orange[dane_z_orange["Szerokość"].notna()]
## dane_z_orange = dane_z_orange[dane_z_orange["Długość"].notna()]
## print(dane_z_orange[["Szerokość", "Długość"]].isnull().sum())
## dane_z_orange.to_excel('Orange_Fiber_Gotowe_Sprzedazowo.xlsx')            # Save to excel - local file
## dane_z_orange = pd.read_excel("Orange_Fiber_Gotowe_Sprzedazowo.xlsx")     # Read Excel - local file
#
#
#### 3 Step = Visualization coordinates on map
#
#gmina = 'JAKTORÓW'
#dane_z_orange_jaktorow = dane_z_orange[dane_z_orange["Gmina"].str.contains(gmina)]
#dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Szerokość"].notna()]
#dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Długość"].notna()]
#
#
## Make an empty map with specific start
#jaktorow_geo = [52.079488, 20.551613]
#jaktorow_pkp_geo = [52.086587629803624, 20.55210270975992]
#
#m = folium.Map(location=jaktorow_geo, tiles='OpenStreetMap', zoom_start=15)
## m = folium.Map(location=[geodata['Szerokość'].mean(), geodata['Długość'].mean()], tiles='OpenStreetMap', zoom_start=15)
#
#popup_text_liczba = 'Gmina: ' + gmina + '\n' + 'Liczba podłączeń = ' + str(len(dane_z_orange_jaktorow))
#folium.Marker([jaktorow_pkp_geo[0], jaktorow_pkp_geo[1]], popup=popup_text_liczba, icon=folium.Icon(color='red', icon='info-sign')).add_to(m)
#
## I can add marker one by one on the map
#for i in range(0, len(dane_z_orange_jaktorow)):
#    popup_text = 'Miejscowość: ' + str(dane_z_orange_jaktorow.iloc[i]['Miejscowość']) + '\nUlica: ' + str(dane_z_orange_jaktorow.iloc[i]['Ulica']) + ' ' + str(dane_z_orange_jaktorow.iloc[i]['Nr '])
#    folium.Marker([dane_z_orange_jaktorow.iloc[i]['Szerokość'], dane_z_orange_jaktorow.iloc[i]['Długość']], popup=popup_text, icon=folium.Icon(color='green')).add_to(m)
## Save it as html
#m.save('mapka.html')




### -------------------------------------------------------------------------------------------------------------------------------------------------------------
### Test for offline file ---------------------------------------------------------------------------------------------------------------------------------------
### -------------------------------------------------------------------------------------------------------------------------------------------------------------

### 1 Step = Import Excel file
# dane = pd.read_excel("lista-punktow-adresowych-popc-nabor-ii-gotowych-sprzedazowo-w-terminie-nie-mniej-niz-30-dni-18.11.2020.xlsx", header=None).iloc[2:, 0:]
# dane = dane.rename(columns = dane.iloc[0,:])
# dane = dane[1:]
# print(dane.head())

# 2 Step = Find coordinates for specific "Gmina"
# jaktorow = dane[dane["Gmina"].str.match("JAKTORÓW")]
# geodata = jaktorow.loc[:,["Szerokość", "Długość"]]
# cols = geodata.columns
# geodata[cols] = geodata[cols].apply(pd.to_numeric, errors='coerce')
#
# # 3 Step = Visualization coordinates on map
# # Make an empty map
# jaktorow_geo = [52.079488, 20.551613]
# m = folium.Map(location=jaktorow_geo, tiles='OpenStreetMap', zoom_start=15)
# # m = folium.Map(location=[geodata['Szerokość'].mean(), geodata['Długość'].mean()], tiles='OpenStreetMap', zoom_start=15)
#
# # I can add marker one by one on the map
# for i in range(0, len(geodata)):
#     folium.Marker([geodata.iloc[i]['Szerokość'], geodata.iloc[i]['Długość']], popup=geodata.iloc[i,:], icon=folium.Icon(color='green')).add_to(m)
#
# # Save it as html
# m.save('mapka.html')
