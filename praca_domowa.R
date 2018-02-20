#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("magrittr")
#install.packages("moments")
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(moments)

#Nie udało mi się zrobić zadania 4.2, jednak reszta powinna działać bez zarzutu

#Zadanie 1. (1 pkt.)
#Wczytaj dane z pliku piractwo.csv. Wykonaj następujące polecenia (najlepiej w jednym potoku):
#wybierz zmienne PISF, Hip, oraz te zaczynające się od słów WTP,
#w nazwach zmiennych określających gotowość do płacenia zamień “_" na “.”,
#odfiltruj osoby po 50. roku życia i takie które odmówiły podania swoich dochodów,
#policz średnią, odchylenie standardowe, skośność i kurtozę gotowości do płacenia dla każdego z atrybutów.


wynik1 <- read.csv('piractwo.csv',header=T,dec=".",sep=';',stringsAsFactors = FALSE) %>% 
  #filter(age>50 & is.na(income)) komenda ta zwraca nam pustą tabelę, ponieważ takie dane nie istnieją 
  rename(wtp.delay=wtp_delay, 
         wtp.risk=wtp_risk,
         wtp.qual=wtp_qual, 
         wtp.legal=wtp_legal,
         wtp.ilegal=wtp_ilegal) %>% 
  summarise_each(args=contains("wtp"), funs(mean,sd,kurtosis,skewness))


#Zadanie 2. (1 pkt.)
#Wczytaj dane z pliku piractwo.csv. Wykonaj następujące polecenia (najlepiej w jednym potoku):
#stwórz 5 kategorii wiekowych,
#pogrupuj bazę danych po kategoriach wiekowych i częstotliwości chodzenia do kina,
#policz zmienną która będzie informowała o indywidualnej różnicy w gotowości do płacenia za atrybut
#“legalny” i “nielegalny”.


wynik2 <- read.csv('piractwo.csv',header=T,dec=".",sep=';',stringsAsFactors = FALSE) %>% 
    mutate(wiek_kat=cut(age,breaks=5, include.lowest =T)) %>% 
    plyrr::arrange(wiek_kat, freq_cinema) %>% 
    dplyr::mutate(difference = (wtp_legal - wtp_ilegal))  %>% 
    select(wiek_kat,freq_cinema)
    
    
#Zadanie 3. (1 pkt.)
#Wczytaj dane z pliku piractwo.csv. Wykonaj następujące polecenia (najlepiej w jednym potoku):
#a. Stwórz tablicę kontyngencji, w której w wierszach będą poziomy zmiennej PISF, w kolumnach poziomy
#zmiennej Hip a w komórkach tabeli liczby osób, które brały udział w sesji eksperymentalnej o danej
#kombinacji cech. (Wskazówka: przypomnij sobie komendy do zamieniania danych z wide
#na long i vice versa)
#b. Stwórz tablicę kontyngencji, w której w wierszach będzie płeć respondenta, w kolumnach częstotliwość
#oglądania filmów ze źródeł nielegalnych, a w komórkach tabeli liczebności uczestników eksperymentu
#należących do danej kategorii. (Wskazówka: przypomnij sobie komendy do zamieniania
#danych z wide na long i vice versa)


#install.packages("gmodels")
library(gmodels)

wynik3.1 <- read.csv2('piractwo.csv', header=T, dec=".", sep=";", stringsAsFactors = FALSE) %>% 
  select(PISF,Hip) %>% 
  cbind() %>%  
  with(., table(PISF,Hip))

wynik3.2 <- read.csv2('piractwo.csv', header=T, dec=".", sep=";", stringsAsFactors = FALSE) %>% 
  select(sex,freq_movie_il) %>% 
  cbind() %>%  
  with(., table(sex,freq_movie_il))


#Zadanie 4. (3 pkt.)
#Który atrybut filmu był dla danego respondenta najważniejszy? Policz:
#maksymalną i minimalną skłonność do płacenia za atrybut ze zbioru atrybutów (zmienne wtp_delay,
#wtp_risk, wtp_qual, wtp_legal, wtp_ilegal))
#Dodatkowo: (+1 pkt.) Spróbuj napisać polecenie, które dla danego zawodnika zwróci nazwę zmiennej
#dla której wystąpiła wartość maksymalna/minimalna.
#Dodatkowo: (+1 pkt.) Dla każdego repondenta sprawdź, czy wycena najcenniejszego i najmniej cennego
#atrybutu na podstawie gotowości do płacenia pokrywa się z informacjami raportowanymi w w ankiecie
#poeksperymentalnej w zmiennych imp_delay imp_qual imp_legal imp_risk. Respondenci
#odpowiadali w nich który atrybut był dla nich najważniejszy (ustalając ranking 1-5) - sprawdźmy czy w
#takim razie czy najważniejszy atrybut był dla nich również najcenniejszy. Uwaga. Żeby wykonać to
#polecenie, wyłącz z analizy zmienne imp_price oraz wtp_ilegal.


wynik4 <- read.csv('piractwo.csv',header=T,dec=".",sep=';',stringsAsFactors = FALSE) %>% 
  summarise(max.wtp_delay=max(wtp_delay),
            max.wtp_risk=max(wtp_risk),
            max.wtp_qual=max(wtp_qual),
            max.wtp_legal=max(wtp_legal),
            max.wtp_ilegal=max(wtp_ilegal),
            min.wtp_delay=min(wtp_delay),
            min.wtp_risk=min(wtp_risk),
            min.wtp_qual=min(wtp_qual),
            min.wtp_legal=min(wtp_legal),
            min.wtp_ilegal=min(wtp_ilegal))


#4.1(dodatkowe)

piractwo %>% select(starts_with('Wtp')) %>%
  apply(1,max) -> piractwo$max_wtp

piractwo %>% select(starts_with('Wtp')) %>%
  apply(1, min) -> piractwo$min_wtp

piractwo %>% select(id, min_wtp, max_wtp) -> wyniki4.1

