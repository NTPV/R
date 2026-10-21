getwd()
setwd("C:/ModInf/zan4")
getwd()
test = c(123, "hi", TRUE)
rm(list = ls()) #Очищаем Environment
data() #Смотрим данные по умолчанию
iris #Дата сет ИРИС        
iris3 #Дата сет ИРИС3       
?iris3 #Справка по датасету ирис3

dim(iris) #Размерность датасета
dim(iris3) #Размерность датасета

#DAT_IRIS = iris
#DAT_IRIS3 = iris3

str(iris) #структура таблицы
str(iris3) #структура таблицы

summary(iris) #выводит общую информацию
summary(iris3) #выводит общую информацию

dna = rep(c("T","G","C","A"), c(10,18,6,10)) #rep(что, сколько раз) # 10T 18G 6C 10A
# "T" "T" "T" "T" "T" "T" "T" "T" "T" "T" "G" "G" "G" "G" ...


dna = factor(dna) #фактор w4 уровни (levels "A","C","G","T" )
dna
summary (dna) #Общая информация
?sample #Информация в справке

sample(dna)
sam1 = sample(dna, size = 44, replace = TRUE) #Выбор 44 элементов без замещения
sam2 = sample(dna, size = 44, replace = FALSE) #Выбор 44 элементов с замещением

rm(list = ls()) #Очистили список
y=c() # присваиваем y пустое значение
i = 1
for (i in 1:10) # для значений i от 1 до 10
{ # открываем цикл
  y=c(y,i)
} # закрываем цикл
y # смотрим результат 1 2 3 4 5 6 7 8 9 10


y=c() # присваиваем y пустое значение
i = 1
x = 1
for (i in 1:10) # для значений i от 1 до 10
{ x=i+2
y=c(y,x) }
y

#вспомним структуру файла данных iris
summary(iris)
iris_data = iris


# Создадим фрейм, где у строк будут имена
iris_desc = data.frame(row.names = c("average","standart
deviation","length"))



# Используем циклы - функцию for для первых 4 признаков из iris
for (i in 1:4)
{
  average = mean(iris[ ,i])
  standart_dev = sd(iris[ ,i])
  length = length(iris[ ,i])
  iris_desc[,i] = c(average, standart_dev, length)
}

iris_desc

##########################################
############ ПРАКТИКА ####################
##########################################
#Очищаем Данные
rm(list = ls())

#1. Сохраните таблицу данных в файле типа csv. Считайте файл, записав
#содержимое в новый объект - таблицу. Изучите данные, которые Вы скачали
#и напишите комментарий. Каков тип скаченных переменных?

hum = read.csv("data10_5.csv", header = TRUE, sep = ";", dec = ",")
hum           # Смотрим объект
dim(hum)      # Количество строк и столбцов
str(hum)      # Краткая информация
summary(hum)  # Краткие характеристика объекта

#2. Выберите из таблицы данных только данные, относящиеся к Вашему
#варианту. Переменную назовите так, чтобы она включала данные Вашего
#варианта, например, mydata101.

mydata10 = hum$v10 # Считываем данные 10 варианта и записываем в переменную mydata10

#3. Посмотрите, 1) что содержится в новом векторе, 2) его общую
#характеристику, 3) какова его длина (команда length).

mydata10 # Выводим все значения
str(mydata10) # Краткая информация
summary(mydata10) #Характеристики (мин квартили макс)
length(mydata10) # Количество элементов


#4. Выберите 7 случайных выборок по 10 из данных Вашего варианта
#(функция sample), запишите их в таблицу- фрейм, где каждая выборка – это
#признак.

#Выбираем семь выборок длинной 10 каждая
v1 = sample(mydata10, 10, replace = FALSE)
v2 = sample(mydata10, 10, replace = FALSE)
v3 = sample(mydata10, 10, replace = FALSE)
v4 = sample(mydata10, 10, replace = FALSE)
v5 = sample(mydata10, 10, replace = FALSE)
v6 = sample(mydata10, 10, replace = FALSE)
v7 = sample(mydata10, 10, replace = FALSE)
# Создаем датафрейм (7 столбцов column_<n>, 10 строк)
df10 = data.frame(column_1 = v1, column_2 = v2,
                  column_3 = v3, column_4 = v4,
                  column_5 = v5, column_6 = v6,
                  column_7 = v7)

#5. Используя функцию for (цикл) для объекта со случайными выборками
#посчитайте статистические характеристики: среднее, стандартное отклонение,
#коэффициент вариации и ошибку среднего.

df10_desc = data.frame()
i = 1

for (variable in df10) {
  df10_desc[i, "Sred"] = mean(variable)
  df10_desc[i, "StOtk"] = sd(variable)
  df10_desc[i, "KoefVar"] = 100*sd(variable)/mean(variable)
  df10_desc[i, "OshSred"] = sd(variable)/sqrt(10)
  i = i + 1
}
df10_desc

#6. Создайте 7 векторов, содержащих номера для систематического
#отбора (функция seq).

s1 = sample(seq(1,140,15))
s2 = sample(seq(140,280,15))
s3 = sample(seq(280,420,15))
s4 = sample(seq(420,560,15))
s5 = sample(seq(560,700,15))
s6 = sample(seq(700,840,15))
s7 = sample(seq(840,980,15))

#7. Выберите 7 систематических выборок (например, mydata[numbers]).
sv1 = mydata10[s1]
sv2 = mydata10[s2]
sv3 = mydata10[s3]
sv4 = mydata10[s4]
sv5 = mydata10[s5]
sv6 = mydata10[s6]
sv7 = mydata10[s7]

#8. Используя функцию for (цикл) для объекта с систематическими
#выборками посчитайте статистические характеристики: среднее, стандартное
#отклонение, коэффициент вариации и ошибку среднего.

# Создаем датафрейм (7 столбцов column_<n>, 10 строк)
df10_sys = data.frame(column_1 = sv1, column_2 = sv2,
                  column_3 = sv3, column_4 = sv4,
                  column_5 = sv5, column_6 = sv6,
                  column_7 = sv7)

df10_desc_s = data.frame()
i = 1

for (variable in df10_sys) {
  df10_desc_s[i, "Sred"] = mean(variable)
  df10_desc_s[i, "StOtk"] = sd(variable)
  df10_desc_s[i, "KoefVar"] = 100*sd(variable)/mean(variable)
  df10_desc_s[i, "OshSred"] = sd(variable)/sqrt(10)
  i = i + 1
}
df10_desc_s

#9. Последовательно соедините (например, с помощью функции c) все
#случайные и систематические выборки в одну большую, длинной 140.
#Посчитайте для нее статистические характеристики (для этого цикл не нужен ☺)
#среднее, стандартное отклонение, коэффициент вариации и ошибку среднего.

v140 = c(v1, v2, v3, v4, v5, v6, v7, sv1, sv2, sv3, sv4, sv5, sv6, sv7)
Sr140 = mean(v140)
Stotkl140 = sd(v140)
KVar140 = 100*sd(v140)/mean(v140)
OshSred = sd(v140)/sqrt(140)
Disp = sd(v140) * sd(v140)

df_result_140 = data.frame()
df_result_140[1,1] = "Srednee"
df_result_140[1,2] = Sr140
df_result_140[2,1] = "StandOtkl"
df_result_140[2,2] = Stotkl140
df_result_140[3,1] = "KoefVar"
df_result_140[3,2] = KVar140
df_result_140[4,1] = "OshSred"
df_result_140[4,2] = OshSred
df_result_140[5,1] = "Disp"
df_result_140[5,2] = Disp

write.table(df_result_140, file = "Results_R_140.csv", dec = ",")
write.table(v140, file = "v140.csv", col.names = FALSE, row.names = FALSE, dec = ",")
