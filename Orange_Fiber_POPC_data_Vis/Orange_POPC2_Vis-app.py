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


@st.cache
def Check_Data_on_Orange_Website(time):

    orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
    orange_fiber_site = requests.get(orange_url).content
    sel = Selector(text=orange_fiber_site)

    css_test = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'
    plik = sel.css(css_test).xpath('@href').extract()
    ile = len(plik)
    print('--- Date now (Check_Data_on_Orange_Website) = ' + str(datetime.datetime.now()) + ' ---')

    return plik, ile


@st.cache
def Import_Data_Orange_Fiber(plik, ile):

    with st.spinner("Importing Data from Orange Website - give me a second - there are " + str(ile) + ' files'):

        orange_server = 'https://www.hurt-orange.pl'
        date_now = datetime.date.today()
        print('--- Date now (Import_Data_Orange_Fiber) = ' + str(date_now) + ' ---')
        print('*** Extract files names from Orange website ***')

        data_from_website = pd.DataFrame(columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali'])
        for i in range(1, ile):
            if (plik[i] != '/wp-content/uploads/2020/11/lista-obszarow-i-miejscowosci-objetych-planami-realizacji-orange-w-ramach-ii-konkursu-popc.xlsx' and plik[i] != '/wp-content/uploads/2020/09/lista-punktow-adresowych-ii-konkurs-popc-1.xlsx'):
                link_pobierania = orange_server + plik[i]
                print(str(i) + ': ' + link_pobierania)
                tymczasowa_tablica = pd.read_excel(link_pobierania, header=[2,]).iloc[2:, 0:]
                data_from_website = data_from_website.append(tymczasowa_tablica, ignore_index=True, sort=False)             ### Dodac kolumne z Data Ostatniego Zapisania pliku Excel

        ### 1.2 Step = Take data and make sure if coordinates are numeric values
        data_from_website = data_from_website[['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']]
        data_from_website[["Szerokość", "Długość"]] = data_from_website[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')

        data_from_website = data_from_website[data_from_website["Szerokość"].notna()]
        data_from_website = data_from_website.drop(data_from_website[(data_from_website["Szerokość"] < 48.0) & (data_from_website["Szerokość"] > 55.0)].index)

        data_from_website = data_from_website[data_from_website["Długość"].notna()]
        data_from_website = data_from_website.drop(data_from_website[(data_from_website["Długość"] < 14.0) & (data_from_website["Długość"] > 25.0)].index)

        data_from_website['Gmina'] = data_from_website['Gmina'].str.upper()

        Gminy_ALL = data_from_website['Gmina'].unique()
        Gminy_ALL.sort()
        Gminy_ALL = list(Gminy_ALL)
        jaktorow_position = Gminy_ALL.index('JAKTORÓW')
        print('*** End of Extract Data ***')
        t1 = datetime.datetime.now()
        time_update = t1.strftime("%d/%m/%Y, godzina: %H:%M")
        print(time_update)

    return data_from_website, Gminy_ALL, jaktorow_position, ile, time_update


### --------------------------------------------------------------------------------------------------------------------------------- ###

minute_now = datetime.datetime.now().minute
hour_now = datetime.datetime.now().hour

### 1 - Import Data from Orange website
plik, ile = Check_Data_on_Orange_Website(minute_now)
dane_z_orange, Gminy_ALL, jaktorow_position, ile, time_update = Import_Data_Orange_Fiber(plik, ile)

### 3.1 - Make Streamlit app

st.title("""Wizualizacja danych Orange POPC2 - Fiber To The Home
Mapa punktów, które zostały lub będą podłączone do sieci światłowodowej """)
st.text(" Plików na stronie = " + str(ile) + '          Ostatnia aktualizacja: ' + str(time_update))

st.sidebar.markdown(""" [Strona Orange, z której pochodzą dane (POPC2)] (https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii/) """)
st.sidebar.header('Wybierz gminę:')
Gmina = st.sidebar.selectbox('Gmina: ', Gminy_ALL, index=jaktorow_position)
st.sidebar.text('\n\n\nMade by SamoX')

with st.spinner("Searching for Gmina: " + Gmina):
    dane_z_orange_jaktorow = dane_z_orange[dane_z_orange["Gmina"].str.contains(Gmina)]
ile_punktow = len(dane_z_orange_jaktorow)
st.text(" Gmina: " + Gmina + "  |  Podłączonych lokalizacji: " + str(ile_punktow))

if (Gmina == "JAKTORÓW"):
    jaktorow_geo = [52.079488, 20.551613]
    jaktorow_pkp_geo = [52.086587629803624, 20.55210270975992]
    start_zoom = 14
else:
    jaktorow_geo = [((dane_z_orange_jaktorow['Szerokość'].max() + dane_z_orange_jaktorow['Szerokość'].min())/2.0), ((dane_z_orange_jaktorow['Długość'].max() + dane_z_orange_jaktorow['Długość'].min())/2.0)]
    jaktorow_pkp_geo = [((dane_z_orange_jaktorow['Szerokość'].max() + dane_z_orange_jaktorow['Szerokość'].min())/2.0), ((dane_z_orange_jaktorow['Długość'].max() + dane_z_orange_jaktorow['Długość'].min())/2.0)]
    start_zoom = 12

m = folium.Map(location=jaktorow_geo, tiles='OpenStreetMap', zoom_start = start_zoom)
popup_text_liczba = 'Gmina: ' + Gmina + '\n' + 'Liczba podłączeń = ' + str(ile_punktow)
folium.Marker([jaktorow_pkp_geo[0], jaktorow_pkp_geo[1]], popup=popup_text_liczba, icon=folium.Icon(color='blue', icon='info-sign')).add_to(m)

# I can add marker one by one on the map
with st.spinner("Adding markers to the map - please wait a second"):
    for i in range(0, len(dane_z_orange_jaktorow)):
        popup_text = 'Miejscowość: ' + str(dane_z_orange_jaktorow.iloc[i]['Miejscowość']) + '\nUlica: ' + str(dane_z_orange_jaktorow.iloc[i]['Ulica']) + ' ' + str(dane_z_orange_jaktorow.iloc[i]['Nr '])
        folium.Marker([dane_z_orange_jaktorow.iloc[i]['Szerokość'], dane_z_orange_jaktorow.iloc[i]['Długość']], popup=popup_text, icon=folium.Icon(color='green')).add_to(m)

folium_static(m)

st.write(dane_z_orange_jaktorow[['Gmina', 'Miejscowość', 'Ulica', 'Nr ', 'Szerokość', 'Długość', 'Dostępna prędkość [Mb]']].sort_values(['Miejscowość', 'Ulica', 'Nr ']))
