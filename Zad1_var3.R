#Бабаева Ксения 123 Вар3.
# Регион 74 Chelyabinsk
#урожайность пшеницы в 2017 году, взяв для рассчета средние суммы активных температур 
#за предыдущие 15 лет, с метеостанций в радиусе не более 120 км
setwd("E:/Chelyabinsk")
getwd()
#instal packages ("tidyverse")
library (tidyverse)
#install.packages("rnoaa")
library(rnoaa)
#1.Скачивание списка метеостанций
station_data = ghcnd_stations()
#записываем в файл для последующей работы 
write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")
#Формирование списка метеостанций
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,
#создав таблицу с именем региона и координатами его столицы
Chelyabinsk = data.frame(id = "CHELYABINSK", latitude = 55.15402,  longitude = 61.42915)
#Вызываем справку
?meteo_nearby_stations
Chelyabinsk_around = meteo_nearby_stations(lat_lon_df = Chelyabinsk, station_data = station_data, var = c("PRCP", "TAVG"), year_min = 2002, year_max = 2017)
#Chelyabinsk_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их 
# удалленности от Челябинска, очевидно что первым элементом таблицы будет идентификатор метеостанции Челябинска,
#его то мы и попытаемся получить
Chelyabinsk_id = Chelyabinsk_around[["CHELYABINSK"]][["id"]][1]
#получение всех данных с метеостанций
summary (Chelyabinsk_id)
str(Chelyabinsk_around)
all_Chelyabinsk_data = meteo_tidy_ghcnd(stationid = Chelyabinsk_id)
#2)чтобы получить таблицу всех метеостанций вокруг Челябинска нужно выбрать целиком первый объект из списка
Chelyabinsk_table = Chelyabinsk_around[[1]]
summary(Chelyabinsk_table)
#в таблице ufa_table оказалось 67783 объектов, ранжированных по расстоянию от Челябинска
#нужно убедится, что этот список включает нужные по условию задачи метеостанции
# отфильтруем все станции, на расстоянии более 120 км при помощи фунции filter
Chelyabinsk_stations=filter(Chelyabinsk_table, distance<=120)
str(Chelyabinsk_stations)
Chelyabinsk_stations$id
#3. Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте #след. команду
all_Chelyabinsk_data = meteo_tidy_ghcnd(stationid = Chelyabinsk_id)
#посмотрим, что же скачивается
summary(all_Chelyabinsk_data)
#Подумаем, какие из этих данных нам нужны
#нам нужны средние суммы активных температур за вышеуказанный период (2002-2017 гг.)
##Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_Chelyabinsk_meteodata = data.frame()
#Цикл для всех метеостанций
for(i in Chelyabinsk_stations$id) 
{ 
  all_i  = meteo_tidy_ghcnd(stationid =  i)
  #выберем нужные свойства
  all_i = all_i[ ,c("id","date","tavg")]
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном этапах цикла
  print(all_i)
  all_Chelyabinsk_meteodata=rbind(all_Chelyabinsk_meteodata, all_i)
}
#Записываем полученные результаты
write.csv(all_Chelyabinsk_meteodata,"all_Chelyabinsk_meteodata.csv")

all_Chelyabinsk_meteodata = read.csv("all_Chelyabinsk_meteodata.csv")
#4. Разбивка даты на составляющие(год, месяц, день года) 
# считываем данные из файла all_Chelyabinsk_meteodata.csv
all_Chelyabinsk_meteodata = read.csv("all_Chelyabinsk_meteodata.csv")
#посмотрим на данные
str(all_Chelyabinsk_meteodata)  
#видим, что дата записана в формате "1896-07-02"
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
# вытащить год
#проверим, что работает
y = year(all_Chelyabinsk_meteodata$date); y
all_Chelyabinsk_meteodata [,"year"]= year(all_Chelyabinsk_meteodata$date)
#добавим месяц
all_Chelyabinsk_meteodata [,"month"]= month(all_Chelyabinsk_meteodata$date)
#вытащить день от начала года
all_Chelyabinsk_meteodata [,"day_of_the_year"]=yday(all_Chelyabinsk_meteodata$date)
#проверим результат
str(all_Chelyabinsk_meteodata)
#отфильтруем данные за 2002-2017
years_Chelyabinsk_meteodata = filter(all_Chelyabinsk_meteodata, year >2002&year<2017)
#проверим результат
str(years_Chelyabinsk_meteodata)
summary(years_Chelyabinsk_meteodata)
# 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц
#Изучаем формулу и видим, что единственное, что нужно расчитать
#- это сумму температур больше 5 град. по месячно, остальное в формуле-  константы
years_Chelyabinsk_meteodata[,"tavg"]= years_Chelyabinsk_meteodata$tavg / 10
summary (years_Chelyabinsk_meteodata)
# 2. Превратим в нули все NA и где  tavg <5 
years_Chelyabinsk_meteodata[is.na(years_Chelyabinsk_meteodata$tavg),"tavg"] = 0
years_Chelyabinsk_meteodata[years_Chelyabinsk_meteodata$tavg<5, "tavg"] = 0
#проверяем, что температура получилась или 0, или больше 5 градусов
summary(years_Chelyabinsk_meteodata)
# Расчитаем суммарную температуру за месяц за 7 лет для всех станций 
# группируем по метеостанциям, годам и месяцам
#group_by
alldays = group_by(years_Chelyabinsk_meteodata,id,year,month)  
#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_Chelyabinsk = summarize(alldays, tsum = sum(tavg))
# максимальная суммарная температура за месяц 689,30, то есть 689,30/30=22,9, что достаточно разумно
summary(sumT_alldays_Chelyabinsk)   
# Сгруппируем данные по месяцам  
groups_Chelyabinsk_months = group_by(sumT_alldays_Chelyabinsk,month)
groups_Chelyabinsk_months
# найдем для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months = summarize(groups_Chelyabinsk_months, St = mean(tsum))
sumT_months
## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) # константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) # константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) # отношение числа дней i-го месяца, 
#входящих в период вегетации культуры, к общему 
#числу дней в месяце,константа по табл. 1.
y = 1.0 # Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300 # Коэффициент использования ФАР посевом 
Qj = 1600 # калорийность урожая культуры 
Lj = 2.2 # сумма частей основной и побочной продукции 
Ej = 25 # стандартная влажность культуры 
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам ##
Yield = sum(sumT_months$Yi);  Yield
