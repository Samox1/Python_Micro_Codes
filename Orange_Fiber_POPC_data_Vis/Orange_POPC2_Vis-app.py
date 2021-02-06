### This script in Python is for Visualization data from Orange POPC II program - Fiber everywhere
### By: Szymon Baczyński

### Requirements - libraries:
# streamlit==0.71.0
# streamlit-folium==0.1.0
# pandas==0.25.3
# numpy==1.18.1
# tables==3.6.1
# scrapy==2.4.1
# folium==0.11.0
# xlrd==1.2.0

import streamlit as st
from streamlit_folium import folium_static
import pandas as pd
import folium
from scrapy import Selector
import requests
import datetime
import os
#import numpy                # numpy == 1.19.3   (with 1.19.4 there is a bug on Windows)


@st.cache
def Check_Data_on_Orange_Website(time):

    orange_url = 'https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii'
    orange_fiber_site = requests.get(orange_url).content
    sel = Selector(text=orange_fiber_site)
    css_path = 'div.tm_pb_attachments_extra:nth-child(4) > div:nth-child(2) > ul:nth-child(2) li a'

    files_on_orange_website = pd.DataFrame(sel.css(css_path).xpath('@href').extract(), columns=['names'])
    print('--- Date now (Check_Data_on_Orange_Website) = ' + str(datetime.datetime.now()) + ' ---')

    del orange_url
    del orange_fiber_site
    del sel
    del css_path

    return files_on_orange_website


@st.cache
def Import_Data_Orange_Fiber(data_from_website, files_on_orange_website_new, files_on_orange_website_old):

    files_diff_2 = files_on_orange_website_new[~files_on_orange_website_new.apply(tuple, 1).isin(files_on_orange_website_old.apply(tuple, 1))]
    print(files_diff_2)

    with st.spinner("Importing Data from Orange Website - give me a second - there are " + str(len(files_diff_2)) + ' new files:'):
    #len(files_on_orange_website_new) - len(files_on_orange_website_old)

        orange_server = 'https://www.hurt-orange.pl'
        print('*** Extract files names from Orange website ***')

        for xxx in files_diff_2.values:
            print("Kupa: " + xxx)

        #files_diff_2 = files_on_orange_website_new[~files_on_orange_website_new.apply(tuple, 1).isin(files_on_orange_website_old.apply(tuple, 1))]
        #print(files_diff_2)
        #files_exclude = ('/wp-content/uploads/2020/11/lista-obszarow-i-miejscowosci-objetych-planami-realizacji-orange-w-ramach-ii-konkursu-popc.xlsx',
        #                 '/wp-content/uploads/2020/09/lista-punktow-adresowych-ii-konkurs-popc-1.xlsx',
        #                 '/wp-content/uploads/2020/12/lista-punktow-adresowych-gotowych-sprzedazowo-popc-nabor-ii-1.xlsx')

        files_exclude = ('/wp-content/uploads/2020/12/lista-punktow-adresowych-gotowych-sprzedazowo-popc-nabor-ii-1.xlsx')

        for i_files in files_diff_2.values:
            if i_files not in files_exclude:
                link_download = orange_server + i_files

                print('X: ' + link_download[0])

                temp_data = pd.read_excel(link_download[0], header=[2, ]).iloc[2:, 0:]
                temp_data = temp_data[['Nazwa obszaru', 'Identyfikator budynku', 'Województwo', 'Powiat', 'Gmina', 'Kod TERC', 'Miejscowość', 'SIMC', 'Ulica', 'Kod ULIC', 'Nr ', 'Szerokość', 'Długość', 'SFH/MFH', 'Dostępna prędkość [Mb]', 'Liczba lokali']]
                temp_data = temp_data.drop_duplicates()

                temp_data_correct = temp_data.copy()
                temp_data_correct[["Szerokość", "Długość"]] = temp_data_correct[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')

                temp_data = temp_data[temp_data["Szerokość"].notna()]
                temp_data = temp_data[temp_data["Długość"].notna()]
                temp_data[["Szerokość", "Długość"]] = temp_data[["Szerokość", "Długość"]].apply(pd.to_numeric, errors='coerce')

                temp_data = temp_data[(temp_data["Szerokość"] > 0.0) & (temp_data["Szerokość"] < 55.0)]
                temp_data = temp_data[(temp_data["Długość"] > 0.0) & (temp_data["Długość"] < 55.0)]

                if ((temp_data['Szerokość'].min() < 48.0) & (temp_data['Długość'].max() > 25.0)):
                    change_col_szer = temp_data_correct['Szerokość'].copy()
                    change_col_dlug = temp_data_correct['Długość'].copy()
                    temp_data_correct['Szerokość'] = change_col_dlug
                    temp_data_correct['Długość'] = change_col_szer
                    del change_col_dlug
                    del change_col_szer

                data_from_website = data_from_website.append(temp_data_correct, ignore_index=True, sort=False)

        del temp_data
        del temp_data_correct
        del orange_server
        del link_download

        data_from_website['Gmina'] = data_from_website['Gmina'].str.upper()
        gminy_data = data_from_website['Gmina'].unique()
        gminy_data.sort()
        gminy_data = list(gminy_data)
        jaktorow_gmina_position = int(gminy_data.index('JAKTORÓW'))

        t1 = datetime.datetime.now()
        time_update_data = t1.strftime("%d/%m/%Y, godzina: %H:%M")
        print('*** End of Extracting Data: ' + time_update_data + ' ***')

    return data_from_website, gminy_data, time_update_data, jaktorow_gmina_position, files_on_orange_website_new


@st.cache
def Filtering_data_for_Map(data):
    data = data[data["Szerokość"].notna()]
    data = data[data["Szerokość"].notnull()]
    data = data[data["Długość"].notna()]
    data = data[data["Długość"].notnull()]
    data = data[(data["Długość"] > 14.0) & (data["Długość"] < 25.0)]
    data = data[(data["Szerokość"] > 48.0) & (data["Szerokość"] < 55.0)]

    return data


### --------------------------------------------------------------------------------------------------------------------------------- ###

hour_now = datetime.datetime.now().hour

### --- Import Data from Orange website --- ###

store = pd.HDFStore('store.h5')

files_old_web = store['files_old_web']
Gminy_ALL = store['Gminy_ALL']
time_update = store['time_and_position'].iloc[0, 0]
jaktorow_position = int(store['time_and_position'].iloc[0, 1])
data_orange = store['dane_z_orange']

files_on_website = Check_Data_on_Orange_Website(hour_now)

if (files_on_website.equals(files_old_web)):
    pass
else:
    store.close()
    del store
    os.remove('store.h5')

    data_orange, Gminy_ALL, time_update, jaktorow_position, files_old = Import_Data_Orange_Fiber(data_orange, files_on_website, files_old_web)
    time_and_position = pd.DataFrame([[time_update, jaktorow_position]], columns=['time', 'position'])

    store = pd.HDFStore('store.h5')
    store['Gminy_ALL'] = pd.DataFrame(Gminy_ALL, columns=['Gminy_ALL'])
    store['time_and_position'] = time_and_position
    store['files_old_web'] = files_old
    store['dane_z_orange'] = data_orange
    st.balloons()

store.close()
del store



### --- Make Streamlit app --- ###

st.title("""Wizualizacja danych Orange POPC2 - Fiber To The Home""")
st.text("""Mapa punktów, które zostały lub będą podłączone do sieci światłowodowej - 
Projekt: Program Operacyjny Polska Cyfrowa (POPC II)""")

st.text(" Plików na stronie = " + str(len(files_on_website)) + '          Ostatnia aktualizacja: ' + str(time_update))

st.sidebar.markdown("""[Link do GitHub] (https://github.com/Samox1/Orange_POPC2_Visualization-App)\n
[Strona Orange, z której pochodzą dane (POPC2)] (https://www.hurt-orange.pl/operatorzy-krajowi/popc-nabor-i-i-ii/)""")

st.sidebar.header('Wybierz gminę:')
Gmina = st.sidebar.selectbox('Gmina: ', Gminy_ALL, index=jaktorow_position)
st.sidebar.text('Made by SamoX')

with st.spinner("Searching for Gmina: " + Gmina):
    data_orange_gmina = pd.DataFrame(data_orange[data_orange["Gmina"].str.contains(Gmina)]).copy()
points_for_gmina = len(data_orange_gmina)
st.text(" Gmina: " + Gmina + "  |  Podłączonych lokalizacji: " + str(points_for_gmina) + "  |  Wszystkich: " + str(len(data_orange)))

with st.spinner("Filtering data for visualization"):
    data_orange_gmina = Filtering_data_for_Map(data_orange_gmina)

if (Gmina == "JAKTORÓW"):
    map_center = [52.079488, 20.551613]
    map_center_marker = [52.086587629803624, 20.55210270975992]
    start_zoom = 13
else:
    map_center = [((data_orange_gmina['Szerokość'].max() + data_orange_gmina['Szerokość'].min()) / 2.0), ((data_orange_gmina['Długość'].max() + data_orange_gmina['Długość'].min()) / 2.0)]
    map_center_marker = [((data_orange_gmina['Szerokość'].max() + data_orange_gmina['Szerokość'].min()) / 2.0), ((data_orange_gmina['Długość'].max() + data_orange_gmina['Długość'].min()) / 2.0)]
    start_zoom = 12

m = folium.Map(location=map_center, tiles='OpenStreetMap', zoom_start=start_zoom)
center_marker_popup = 'Gmina: ' + Gmina + '\n' + 'Liczba podłączeń = ' + str(points_for_gmina)
folium.Marker(map_center_marker, popup=center_marker_popup, icon=folium.Icon(color='blue', icon='info-sign')).add_to(m)

# I can add marker one by one on the map
with st.spinner("Adding markers to the map - please wait a second"):
    for i in range(0, len(data_orange_gmina)):
        popup_text = 'Miejscowość: ' + str(data_orange_gmina.iloc[i]['Miejscowość']) + '\nUlica: ' + str(data_orange_gmina.iloc[i]['Ulica']) + ' ' + str(data_orange_gmina.iloc[i]['Nr '])
        folium.Marker([data_orange_gmina.iloc[i]['Szerokość'], data_orange_gmina.iloc[i]['Długość']], popup=popup_text, icon=folium.Icon(color='green')).add_to(m)

    folium_static(m)
    st.write(data_orange_gmina[['Gmina', 'Miejscowość', 'Ulica', 'Nr ', 'Szerokość', 'Długość', 'Dostępna prędkość [Mb]']].sort_values(['Miejscowość', 'Ulica', 'Nr ']))


del m
del Gminy_ALL
del data_orange
del data_orange_gmina
del points_for_gmina
del Gmina