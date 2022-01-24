# 1) Do rozwiązania mają Państwo 3 problemy:
#   a) klasyfikacja binarna,
#   b) klasyfikacja wieloklasowa,
#   c) regresja.
#   Na tę chwilę każdy z Państwa otrzymał jeden zbiór danych (albo do klasyfikacji binarnej albo wieloklasowej). 
#   Z repozytorium https://archive.ics.uci.edu/ml/index.php wybieraja Państwo, wedle upodobania, dodatkowe dwa zbiory tj. 
#   do regresji i klasyfikacji (tej której nie dotyczy otrzymany już zbiór danych). 
#   
# 2) Rozwiązują Państwo 3 powyższe problemy wykorzystując opracowane algorytmy:
#   a) Dla klasyfikacji binarnej: k-najbliższych sąsiadów, drzewa decyzyjne, maszynę wektorów nośnych, sieci neuronowe.
#   b) Dla klasyfikacji wieloklasowej: k-najbliższych sąsiadów, drzewa decyzyjne, sieci neuronowe.
#   c) Dla regresji: k-najbliższych sąsiadów, drzewa decyzyjne, sieci neuronowe.
# 
# 3) Porównują Państwo wyniki otrzymane dla własnych algorymtów z wynikami otrzymanymi dla algorytmów pochodzących z innych 
#    pakietów w R, np: caret, rpart, nnet, neuralnet, kernlab, e1071.
# 
# 4) Analiza powinna obejmować:
#   a) Wpływ hiper-parametrów na jakość opracowanych modeli.
#      np. liczba neuronów ukrytych, liczba najbliższych sąsiadów, głębokość drzewa itd.
#   b) Porównanie wyników najlepszych modeli, własnych jak i tych wbudowanych.
# 
# 5) Spakowany katalog z projektem powinien zawierać:
#   a) Pliki z danymi źródłowymi,
#   b) Plik funkcje.R zawierający definicję wszystkich opracowanych funkcji,
#   c) Plik Glowny.R przeprowadzający całą analizę. Uruchomienie tego pliku powinno generować różne obiekty i wykresy, które umieszczone będą w pliku z wynikami.
#     - plik dla każdego z 3 problemów: wczytuje funkcje, wczytuje dane i przekształca je, przeprowadza cross-walidację własnych oraz wbudowanych algorytmów, wybiera najlepsze wyniki.
#   d) Plik o dowolnym rozszerzeniu np. pdf, docx, pptx, zawierający opis i wyniki z przeprowadzonej analizy.
# 
# 6) Projekty proszę przesłać do 29 stycznia do godziny 23.59.