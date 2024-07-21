# House-Price-Analysis
```{r include=FALSE}
library(ggplot2)
library(dplyr)
library(corrplot)
houses <- read.csv(file = "houses.csv")
```

# Wstęp

Dane, którymi będziemy się zajmować dotyczą nieruchomości w USA. Dane składają się z 4600 obserwacji oraz 18 kolumn. Chcemy zbadać jak zmienne takie jak powierzchnia nieruchomości, ilość łazienek czy stan nieruchomości wpływają na cenę. Przeprowadzona zostanie analiza wpływu różnych czynników (tj. powierzchnia nieruchomości, ilość łazienek czy sypialń) na klasyfikację ceny nieruchomości.

# Hipoteza

Wiele czynników ma wpływ na cenę nieruchomości.

# Zmiennne
  
zmienna         opis
--------        ----
`date`          data zebrania danych
`price`         cena nieruchomości ($)
`bedrooms`      liczba sypialni
`bathrooms`     liczba łazienek (1 -  umywalka, toaleta oraz wanna i prysznic,
                0.75 - umywalka, toaleta i wanna LUB prysznic, 0.5 - umywalka i toaleta, 
                0.25 - tylko toaleta)
`sqft_living`   powierzchnia mieszkalna budynku (w stopach kwadratowych)
`sqft_lot`      powierzchnia działki (w stopach kwadratowych)
`floors`        liczba pięter
`waterfront`    1 - nieruchomość przy wodzie, 0 - nie ma wody 
`view`          jak dobry jest widok z nieruchomości (w skali 0-4)
`condition`     stan nieruchomości (w skali 0-5)
`sqft above`    powierzchnia mieszkalna, która znajduje się nad poziomem
                gruntu (nie obejmuje powierzchni piwnicy)
`sqft_basement` powierzchnia piwnicy (w stopach kwadratowych)
`yr_built`      rok budowy
`yr_renovated`  rok renowacji
`street`        ulica
`city`          miasto
`statezip`      kod stanu
`country`       kraj (USA)

# Przekształcenia danych

```{r}
head(houses)
sum(is.na(houses))
```
Wszystkie parametry z pewnością nie będą przydatne do modelowania. Dlatego przechodzimy do eliminacji niektórych z nich. Informacja o czasie, podobnie jak adres, nie wnosi nic do badania, dlatego usuwamy ją z naszej bazy danych. Informacja o liczbie pięter $floors$ wydaje się być dość nietypowa (nie są to wartości całkowite) i w pewnym stopniu powiela informacje o powierzchni nieruchomości, więc nie uwzględniamy jej w dalszych badaniach. Podobnie jest z cechą widok $view$ (prawie wszystkie przypadki mają wartość równą 0). 

Pozostałe zmienne pozostawiamy, ale niektóre z nich wymagają modyfikacji. Wszystkie parametry z "sqft" (square foot) zmieniamy na $m^{2}$. 

# Zamiana na $m^2$

Dane są zebrane na podstawie cen nieruchomości ze Stanów Zjednoczonych. Tam powierzchnię podaje się w stopach kwadratowych. My zamienimy je na metry kwadratowe, ponieważ ta jednostka jest nam bardziej znana.
```{r}
houses$sqft_living <- houses$sqft_living*0.092903
houses$sqft_lot <- houses$sqft_lot*0.092903
houses$sqft_above <- houses$sqft_above*0.092903
```

# Zmiana nazw zmiennych

Przy zamianie na metry kwadratowe zmieniamy tutaj nazwy kolumn, ponieważ sugerują one wcześniejszą jednostkę, którą zamienialiśmy. 
```{r}
houses <- houses %>% 
  rename("m2_living" = "sqft_living", "m2_lot" = "sqft_lot", 
         "m2_above" = "sqft_above")
```

# Zamiana na zmienne kategoryczne



Zmieniamy cechę $sqft\_basement$ na zero-jedynkową (gdzie wartość 1 oznacza, że nieruchomość ma piwnicę, a 0 nie ma piwnicy), ponieważ nie ma sensu porównywać powierzchni piwnicy, gdy ponad połowa nieruchomości jej nie ma. 

```{r}
houses %>% count(sqft_basement == 0)
```

Podobna sytuacja ma miejsce z rokiem remontu - zmieniamy go na zmienną dwuwartościową, w której 1 oznacza, że nieruchomość była remontowana, a 0, że takie działania nie miały miejsca. Ostatniej zmiany dokonujemy w przypadku cechy rok budowy nieruchomości. Uzyskując w ten sposób stare (0) i nowe (1) nieruchomości. Rok 1970 został przyjęty jako punkt odcięcia, ponieważ znajduje się w pobliżu mediany.

```{r}
median(houses$yr_built)
```

Niektóre dane przekształcamy na zmienne zero-jedynkowe w celu ułatwienia analizy. Na przykład informacja, w którym roku nieruchomość została wyremontowana nie ma dla nas takiego znaczenia. Przydatniejszą informacją będzie to czy nieruchomość jest po remoncie czy jest w stanie początkowym.  
```{r}
houses$is_renovated <- ifelse(houses$yr_renovated > 0, 1, 0)
houses$is_new <- ifelse(houses$yr_built> 1970, 1, 0)
houses$is_basement <- ifelse(houses$sqft_basement > 0, 1, 0)
head(houses)
```

# Wykres względem zmiennej zależnej 

Zmienne są wstępnie uporządkowane, ale należy również przyjrzeć się przypadkom. 
Naszą zmienną zależną jest cena nieruchomości. Przyjrzyjmy się więc jej wartościom.
```{r}
plot(houses$price, xlab = "Numer obserwacji", ylab = "Cena [w $]")
```
Jak widzimy, istnieją wartości, które wyróżniają się na tle innych. Są to nieruchomości o bardzo wysokiej cenie. Dlatego decydujemy się wyeliminować te przypadki, w których cena nieruchomości przekraczają 1 milion dolarów, ponieważ są one wysoce nietypowe w badanym zagadnieniu.

# Usuwanie danych odstających

```{r}
houses <- houses[,c(2:14, 19:21)]
houses$price <- ifelse(houses$price == 0, NA, houses$price)
houses$price <- ifelse(houses$price > 1000000, NA, houses$price)
houses <- houses[complete.cases(houses), ]
plot(houses$price, xlab = "Numer obserwacji", ylab = "Cena [w $]")
```
Po usunięciu cen nieruchomości powyżej miliona dolarów na wykresie lepiej widać rozmieszczenie obserwacji.
Dodamy również nową kolumnę zawierającą zmienne kategoryczne, do której będziemy się odnosić w naszej analizie. Kolumna ta będzie zawierać informację o tym czy cena nieruchomości znajduję się poniżej 500 000$ (0) czy powyżej tej kwoty (1).

```{r}
houses$price_classification <- ifelse(houses$price > 500000.0, 1, 0)
```


Po wszystkich przekształceniach nasze dane prezentują się następująco:
```{r}
houses_clean <- houses[,c(1:5,7,9,10,14:17)]
head(houses_clean)
```
# Typy danych

Sprawdzamy jakie mamy typy danych w poszczególnych kolumnach w celu ułatwienia dalszej analizy.
```{r}
sapply(houses_clean,class)
```
# Zmienne po przekształceniach

zmienna                 opis
--------                ----
`price`                 cena nieruchomości ($)
`bedrooms`              liczba sypialni
`bathrooms`             liczba łazienek (1 -  umywalka, toaleta oraz wanna i prysznic,
                        0.75 - umywalka, toaleta i wanna LUB prysznic, 0.5 - umywalka i toaleta, 
                        0.25 - tylko toaleta)
`m2_living`             powierzchnia mieszkalna budynku (w metrach kwadratowych)
`m2_lot`                powierzchnia działki (w metrach kwadratowych)
`waterfront`            1 - nieruchomość przy wodzie, 0 - nie ma wody 
`condition`             stan nieruchomości (w skali 0-5)
`m2_above`              powierzchnia mieszkalna, która znajduje się nad poziomem
                        gruntu (nie obejmuje powierzchni piwnicy)
`is_renovated`          czy była renowacja (1 - była, 0 - nie)  
`is_new`                czy jest nowy - zbudowany po 1970 (1 - tak, 0 - nie)
`is_basement`           czy jest piwnica (1 - tak, 0 - nie)
`price_classfication`   cena powyżej 500000(1), poniżej (0)

Kiedy mamy już przygotowaną bazę z potencjalnymi zmiennymi, nadszedł czas na przeprowadzenie wstępnej analizy. W naszym przypadku zmienną objaśnianą będzie $price\_classification$.

```{r}
summary(houses_clean)
```
Najniższa cena nieruchomości jaka występuje w naszych danych to 7800 dolarów, a najwyższa sięga miliona dolarów. Średnia powierzchnia mieszkalna wynosi 185,67 $m^2$. Największa rozbieżność występuje między wartościami powierzchni działki ($m^2$). Najniższa wartość to 59,27, a najwyższa 99798,07. 

Średnia cena oscyluje w okolicach 500 000, ale zobaczmy jak ilościowo prezentuje się rozmieszczanie danych z podziałem na dwie kategorie cenowe. 
```{r}
ggplot(houses_clean, aes(x = factor(price_classification),fill = factor(price_classification))) +
  geom_bar() +
  labs(title = "Liczba mieszkań ze względu na cenę",
       x = "Klasyfikacja ceny",
       y = "Liczba mieszkań") +
  scale_x_discrete(labels = c("Poniżej 500 000", "Powyżej 500 000"))+
  scale_fill_manual(values = c("lightblue", "lightpink"))

```
Widzimy, że większa część nieruchomości z naszych danych znajduje się poniżej ceny 500 000.

Zobaczmy na wykresie, czy ilość łazienek wpływa na cenę mieszkania.
```{r}
ggplot(houses_clean, aes(x = factor(price_classification), y = bathrooms)) +
geom_boxplot() +
labs(title = "",
x = "cena",
y = "liczba łazienek")
```
Mediana liczby łazienek w domach powyżej 500 000 dolarów jest wyższa niż w domach poniżej 500 000 dolarów, co sugeruje, że droższe nieruchomości mają tendencję do posiadania większej liczby łazienek. W pierwszej klasyfikacji maksymalna liczba łazienek wynosi 4,75, w drugiej ta liczba jest mniejsza, ponieważ wynosi 3,75. W obu grupach cenowych występują wartości odstające, jednak bardziej widoczne są w grupie domów powyżej 500 000 dolarów. Zakres liczby łazienek w domach poniżej 500 000 dolarów jest większy, co może sugerować większą różnorodność w liczbie łazienek w tej kategorii cenowej. Widzimy, że średnio nieruchomości o wyższych cenach posiadają większą ilość łazienek. Trudno się temu dziwić. Im więcej dom ma do zaoferowania tym wyższa jest jego cena. 

Zobaczmy teraz jak wygląda zależność ceny mieszkania od jego powierzchni oraz od tego czy został wyremontowany.

```{r}
ggplot(houses_clean, aes(x = price, y = m2_living,color = factor(is_renovated))) + geom_point() +
  labs(title = "Cena vs Powierzchnia", x = "Cena ($)", y = "Powierzchnia mieszkalna (m2)") +
  scale_color_manual(values = c("darkblue", "cornflowerblue"))
```
Pojawiają się takie dane w których widać mieszkania cenowo zbliżone do miliona posiadające zaledwie 150$m^2$ (dla przypomnienia największa powierzchnia to 553$m^2$) oraz nie będące po renowacji. Pojawia się również sytuacja, w której mieszkanie ma prawie 500$m^2$, jest po renowacji a jego cena wynosi ok.200 000. Tak jak się spodziewaliśmy, średnio, im większa powierzchnia mieszkania tym większa jest jego cena. Jendak to czy dom był remontowany nie wydaje się mieć widocznego wpływu na cenę nieruchomości. Sprawdźmy teraz w modelu czy faktycznie tak jest.

# Modele

Przeanalizujmy, jak powierzchnia mieszkania oraz stan nieruchomości wpływają na prawdopodobieństwo tego czy nieruchomość kosztuje więcej niż 500 000. W tym przypadku utworzymy model regresji logistycznej GLM, w której zmienną zależną jest cena ($price\_classification$), a zmiennymi niezależnymi jest powierzchnia mieszkalna ($m2\_living$) oraz to czy nieruchomość jest po renowacji ($is\_renovated$).

```{r}
model1 <- glm(price_classification ~ m2_living + is_renovated, houses_clean, family=binomial) 
summary(model1)
```
Szacowana wartość współczynnika $is\_renovated$ wynosi 0.04387, co sugeruje, że występowanie renowacji nie ma znaczącego wpływu na prawdopodobieństwo zaklasyfikowania domu powyżej 500 000. Zmienna z value ma niską wartość (0,58) i p-value na poziomie nieistotności statystycznej, co oznacza, że nie jest dobrym predyktorem w tym modelu. Dowodzi to naszej tezy - to, że dom był remontowany nie wpływa znacząco na jego cenę. 

Szacowana wartość współczynnika $m2\_living$ wynosi 0.01875, co oznacza, że każdy dodatkowy metr powierzchni mieszkaniowej zwiększa szanse tego, że nieruchomość będzie kosztowała więcej niż 500 000. Zmienna z value ma wysoką wartość (28.32) i niskie p-value. Wskazuje to na istotny statystycznie wpływ na prawdopodobieństwo, że dom znajdzie się w kategorii wyższej ceny.

Przeanalizujmy teraz wykresy diagnostyczne modelu.

```{r}
plot(model1)
```
1. Residuals vs. Fitted:
Wykres ten przedstawia reszty wykreślone względem wartości dopasowanych. Punkty powinny być równomiernie rozrzucone względem osi `x`. Czerwona linia (wygładzenie) wskazuje na lekkie niedopasowane modelu dla większych wartości.
2. Normal Q-Q:
Punkty powinny układać się wzdłuż prostej linii, co sugeruje normalny rozkład reszt. Na początku widzimy odchylenie, co może sugerować, że reszty nie są idealnie normalnie rozłożone.
3. Scale-Location:
Ten wykres służy do badania homoskedastyczności. Występuje ona, gdy czerwona krzywa jest pozioma. Wtedy wariancje błędów losowych (czyli reszt) są stałe wzdłuż wartości przewidywanych przez model. Nasza czerwona krzywa nie jest pozioma dla wartości średnich, czyli w tym przypadku mamy heteroskedastyczność.
4. Residuals vs. Leverage:
Wyróżniają się dwa punkty charakterystyczne (4433, 4399), które mogą wskazywać na obserwacje wpływowe. Te punkty mają duży wpływ na dopasowanie modelu. 


W kolejnym modelu sprawdzimy jak ilość łazienek oraz sypialń wpływa na prawdopodobieństwo tego czy nieruchomość kosztuje więcej niż 500 000.

```{r}
model2 <- glm(price_classification ~ bathrooms + bedrooms, houses_clean, family=binomial) 
summary(model2)
```
Patrząc na p-value możemy stwierdzić, że obydwie zmienne wpływają na to, czy cena nieruchomości będzie wyższa niż 500 000. Każda łazienka podwyższa podwyższa szanse tego, że nieruchomość będzie kosztowała więcej niż 500 000. Patrząc na z-value możemy wywnioskować, że ilość łazienek, bardziej niż ilość sypialń, wpływa na klasyfikacje ceny.

```{r}
plot(model2)
```
1. Residuals vs. Fitted:
Punkty powinny być równomiernie rozrzucone, czerwona linia ukazuje niedopasowanie dla dużych wartości.
2. Normal Q-Q:
Im bliżej końców wykresu, tym punkty bardziej odbiegają od linii. Sugeruje to, że reszty nie są idealnie normalnie rozłożone.
3. Scale-Location:
Wariancje reszt dla wartości średnich są niejednolite - występuje heteroskedastyczność.
4. Residuals vs. Leverage:
Trzy wyróżniające się punkty (3912, 2280, 1018) mogą być uznane za obserwacje wpływowe, ponieważ znacząco wpływają na dopasowanie modelu. Punkt 3912 wydaje się być najbardziej wpływowy.

W ostatnim modelu weźmiemy zmienne, które wcześniej okazały się istotne. We wcześniejszych modelach przeanalizowaliśmy w sumie cztery zmienne niezależne, lecz jedna - $is\_renovated$ - nie wpływała na model, zatem ją pomijamy.

```{r}
model3 <- glm(price_classification ~ m2_living + bathrooms + bedrooms, houses_clean, family=binomial) 
summary(model3)
```
W tym modelu widzimy dwie ciekawe zależności. Po pierwsze, mimo to, że $bathrooms$ wcześniej było istotne, w tym modelu już nie jest. Dzieje się tak dlatego, że mamy tutaj bardzo istotnie wpływającą zmienną $m2\_living$. Przez dodanie jej do modelu ilość łazienek przestaję być istotna statystycznie, tzn. nie wpływa już znacząco na to, że nieruchomość będzie kosztowała więcej niż 500 000.

Po drugie, negatywna wartość współczynnika (-0.2996) sugeruje, że większa liczba sypialni jest związana z mniejszym prawdopodobieństwem przynależności domu do wyższej klasy cenowej. Innymi słowy, gdy liczba sypialni rośnie, szansa na to, że cena domu będzie wyższa, maleje. Liczba sypialni - $bedrooms$ - jest istotnym czynnikiem wpływającym na cenę domu, ale w sposób odwrotny do intuicyjnego. Wyższa liczba sypialni nie zawsze przekłada się na wyższą cenę, co może wynikać z różnych czynników, takich jak wielkość całkowita domu.

```{r}
plot(model3)
```
1. Residuals vs. Fitted:
Czerwona linia sugeruje niewielkie niedopasowanie dla dużych wartości.
2. Normal Q-Q:
Punkty na początku wykresu odbiegają od linii, co sugeruje, że reszty nie są idealnie normalnie rozłożone.
3. Scale-Location:
Wariancje reszt dla wartości średnich są niejednolite - występuje heteroskedastyczność.
4. Residuals vs. Leverage:
W tym wykresie nie widzimy punktów wpływowych, linia czerwona jest prawie idealnie pozioma.

# Podsumowanie

Na podstawie powyższej analizy możemy stwierdzić, że to czy cena nieruchomości przekracza 500 000 $ zależy od wielu czynników takich jak powierzchnia mieszkania, ilość łazienek czy sypialń.
