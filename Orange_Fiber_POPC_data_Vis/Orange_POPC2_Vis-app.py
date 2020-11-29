### This script in Python is for Visualization data from Orange POPC II program - Fiber everywhere
### By: Szymon Baczyński

import streamlit as st
from streamlit_folium import folium_static
import pandas as pd
import numpy                # numpy == 1.19.3   (with 1.19.4 there is a bug on Windows)
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

    files_new = pd.DataFrame(sel.css(css_test).xpath('@href').extract(), columns=['names'])
    how_many_files = len(files_new)
    print('--- Date now (Check_Data_on_Orange_Website) = ' + str(datetime.datetime.now()) + ' ---')

    return files_new, how_many_files


@st.cache
def Import_Data_Orange_Fiber(files_new, files_old):

    with st.spinner("Importing Data from Orange Website - give me a second - there are " + str(len(files_new) - len(files_old)) + ' new files'):

        orange_server = 'https://www.hurt-orange.pl'
        date_now = datetime.date.today()
        print('--- Date now (Import_Data_Orange_Fiber) = ' + str(date_now) + ' ---')
        print('*** Extract files names from Orange website ***')

        data_from_website = pd.DataFrame(columns=['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali'])

        files_diff = files_new.merge(files_old, how='outer')
        print(files_diff)
        counter = 0

        for i in files_diff.values:
            if (i != '/wp-content/uploads/2020/11/lista-obszarow-i-miejscowosci-objetych-planami-realizacji-orange-w-ramach-ii-konkursu-popc.xlsx' and i != '/wp-content/uploads/2020/09/lista-punktow-adresowych-ii-konkurs-popc-1.xlsx'):
                link_pobierania = orange_server + i
                counter = counter + 1
                print(str(counter) + ' : ' + link_pobierania[0])

                tymczasowa_tablica = pd.read_excel(link_pobierania[0], header=[2,]).iloc[2:, 0:]
                tymczasowa_tablica_prawidlowa = tymczasowa_tablica.copy()
                tymczasowa_tablica_prawidlowa[["Szerokość", "Długość"]] = tymczasowa_tablica_prawidlowa[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')

                tymczasowa_tablica = tymczasowa_tablica[tymczasowa_tablica["Szerokość"].notna()]
                tymczasowa_tablica = tymczasowa_tablica[tymczasowa_tablica["Długość"].notna()]
                tymczasowa_tablica[["Szerokość", "Długość"]] = tymczasowa_tablica[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')

                tymczasowa_tablica = tymczasowa_tablica[(tymczasowa_tablica["Szerokość"] > 0.0) & (tymczasowa_tablica["Szerokość"] < 55.0)]
                tymczasowa_tablica = tymczasowa_tablica[(tymczasowa_tablica["Długość"] > 0.0) & (tymczasowa_tablica["Długość"] < 55.0)]

                if ((tymczasowa_tablica['Szerokość'].min() < 48.0) & (tymczasowa_tablica['Długość'].max() > 25.0)):
                    change_col_szer = tymczasowa_tablica_prawidlowa['Szerokość'].copy()
                    change_col_dlug = tymczasowa_tablica_prawidlowa['Długość'].copy()
                    tymczasowa_tablica_prawidlowa['Szerokość'] = change_col_dlug
                    tymczasowa_tablica_prawidlowa['Długość'] = change_col_szer

                data_from_website = data_from_website.append(tymczasowa_tablica_prawidlowa, ignore_index=True, sort=False)             ### Dodac kolumne z Data Ostatniego Zapisania pliku Excel

        ### 1.2 Step = Take data and make sure if coordinates are numeric values
        data_from_website = data_from_website[['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']]
        data_from_website = data_from_website.drop_duplicates()

        ### Z TEGO TRZEBA ZROBIC FUNKCJE - FILTR przed MAPA
        # data_from_website = data_from_website[data_from_website["Szerokość"].notna()]
        # data_from_website = data_from_website.drop(data_from_website[(data_from_website["Szerokość"] < 48.0) & (data_from_website["Szerokość"] > 55.0)].index)
        # data_from_website = data_from_website[data_from_website["Długość"].notna()]
        # data_from_website = data_from_website.drop(data_from_website[(data_from_website["Długość"] < 14.0) & (data_from_website["Długość"] > 25.0)].index)

        data_from_website['Gmina'] = data_from_website['Gmina'].str.upper()
        Gminy_ALL = data_from_website['Gmina'].unique()
        Gminy_ALL.sort()
        Gminy_ALL = list(Gminy_ALL)
        jaktorow_position = int(Gminy_ALL.index('JAKTORÓW'))

        t1 = datetime.datetime.now()
        time_update = t1.strftime("%d/%m/%Y, godzina: %H:%M")
        print('*** End of Extract Data: ' + time_update + ' ***')

    return data_from_website, Gminy_ALL, time_update, jaktorow_position, files_new


### --------------------------------------------------------------------------------------------------------------------------------- ###

# ctx = st.report_thread.get_report_ctx()
# print(ctx.session_id)

minute_now = datetime.datetime.now().minute
hour_now = datetime.datetime.now().hour

### 1 - Import Data from Orange website

store = pd.HDFStore('store.h5')
#print(store.info())

# files_old_web = pd.DataFrame(columns=['names'])
files_old_web = store['files_old_web']
plik, how_many_files = Check_Data_on_Orange_Website(hour_now)

if (plik.equals(files_old_web)):
    #print('*** files_new == files_old_web ***')
    Gminy_ALL = store['Gminy_ALL']
    time_update = store['time_and_position'].iloc[0,0]
    jaktorow_position = int(store['time_and_position'].iloc[0,1])
    dane_z_orange = store['dane_z_orange']

else:
    #print('*** files_new != files_old_web ***')
    dane_z_orange, Gminy_ALL, time_update, jaktorow_position, files_old = Import_Data_Orange_Fiber(plik, files_old_web)
    time_and_position = pd.DataFrame([[time_update, jaktorow_position]], columns=['time', 'position'])
    #print(time_and_position)
    store.remove('Gminy_ALL', 'time_and_position', 'files_old_web', 'dane_z_orange')
    store['Gminy_ALL'] = pd.DataFrame(Gminy_ALL, columns=['Gminy_ALL'])
    store['time_and_position'] = time_and_position
    store['files_old_web'] = files_old
    store['dane_z_orange'] = dane_z_orange

store.close()

### 3.1 - Make Streamlit app

st.title("""Wizualizacja danych Orange POPC2 - Fiber To The Home
Mapa punktów, które zostały lub będą podłączone do sieci światłowodowej """)
st.text(" Plików na stronie = " + str(len(plik)) + '          Ostatnia aktualizacja: ' + str(time_update))

st.sidebar.markdown(""" [Strona Orange, z której pochodzą dane (POPC2)] (https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii/) """)
st.sidebar.header('Wybierz gminę:')
Gmina = st.sidebar.selectbox('Gmina: ', Gminy_ALL, index=jaktorow_position)
st.sidebar.text('\n\n\nMade by SamoX')

with st.spinner("Searching for Gmina: " + Gmina):
    dane_z_orange_jaktorow = dane_z_orange[dane_z_orange["Gmina"].str.contains(Gmina)].copy()
ile_punktow = len(dane_z_orange_jaktorow)
st.text(" Gmina: " + Gmina + "  |  Podłączonych lokalizacji: " + str(ile_punktow) + "  |  Wszystkich: " + str(len(dane_z_orange)))

### --- Z TEGO TRZEBA ZROBIC FUNKCJE - FILTR przed MAPA --- ###
dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Szerokość"].notna()]
dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Szerokość"].notnull()]
dane_z_orange_jaktorow = dane_z_orange_jaktorow.drop(dane_z_orange_jaktorow[(dane_z_orange_jaktorow["Szerokość"] < 48.0) & (dane_z_orange_jaktorow["Szerokość"] > 55.0)].index)
dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Długość"].notna()]
dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow["Długość"].notnull()]
dane_z_orange_jaktorow = dane_z_orange_jaktorow.drop(dane_z_orange_jaktorow[(dane_z_orange_jaktorow["Długość"] < 14.0) & (dane_z_orange_jaktorow["Długość"] > 25.0)].index)
dane_z_orange_jaktorow = dane_z_orange_jaktorow.drop_duplicates(subset='Identyfikator budynku')
# dane_z_orange_jaktorow = dane_z_orange_jaktorow[dane_z_orange_jaktorow['Długość'] != 44094.0]
# print(dane_z_orange_jaktorow['Długość'].max())
### --- Z TEGO TRZEBA ZROBIC FUNKCJE - FILTR przed MAPA --- ###

if (Gmina == "JAKTORÓW"):
    jaktorow_geo = [52.079488, 20.551613]
    jaktorow_pkp_geo = [52.086587629803624, 20.55210270975992]
    start_zoom = 13
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
