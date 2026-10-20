########
#ТЕОРИЯ
########
getwd()
setwd("C:/ModInf/zan3")
getwd()
?getwd
dat10 = read.csv("data.csv", header = TRUE, sep = ";", dec = ",")
dat10
#Размерность таблицы (число строк и столбцов)
#ОБРАЩЕНИЕ К ЭЛЕМЕНТАМ ТАБЛИЦЫ
#dat[Номера строк, Колонки]
dim(dat10)
dat10_1 = dat10[2] #Вторая колонка K2O (тип таблица dataframe)
#Если в обращении по индексу несколько параметров, то тип будет значением из таблицы, если параметр один, то тип будет - таблица
dat10_2 = dat10[,2] #Все строки второй колонки K2O
dat10_3 = dat10[,1] #Все строки первой колонки A2
dat10_4 = dat10[1,2] #Первая строка (второй колонки K20): 18
dat10_5 = dat10[1,1] #Первая строка (первой колонки A2): 5.4
dat10_6 = dat10[2,1] #Вторая строка (первой колонки A2): 7.3
dat10_7 = dat10[5,1] #Пятая строка (первой колонки A2): 6.0
dat10_8 = dat10[3:7, 2] #3я-7я строка второй колонки K20 (int: 10 13 30 16 22)

#dat[Номера строк, Колонки]
dat10_9 = dat10[,"K2O"] #Все строки второй колонки тип int
dat10_10 = dat10["K2O"] #Все строки второй колонки тип таблица (фрейм)


dat10_3col = read.csv("data_3col.csv", header = TRUE, sep = ";", dec = ",")
dat10_3col_1 = dat10_3col[2:4, 2:3] # 2я-4я строка 2й-3й колонки

columns <- c("test", "K2O")
dat10_3col_2 = dat10_3col[3:8,columns] #3я-8я строка в колонках (test и K2O)

#Выбираем первую и вторую колонку из датафрейма 
dat10 = dat10_3col[,1:2]

#Имена колонок
names(dat10_3col)

#Тип строк и названия столбцов
str(dat10_3col)

#Обращение по имени колонки
dat10_3col$K2O

#Краткая информация об объекте
summary(dat10_3col)
summary("Привет")

#Квантиль 50% от столбца A2
quantile(dat10_3col$A2, 0.5)

#Верхний квартиль от 3й-10й строки K2O
part_of_K2O = dat10_3col[3:10, "K2O"]
quantile(part_of_K2O, 0.75)

#Верхний квартиль от строк колонки А2, где значения больше 6
part_of_A2 = dat10_3col[dat10_3col$A2>6, "A2"]
quantile(part_of_A2, 0.75)

#Медиана от строк колонки A2, где значения больше 7
part_of_A2 = dat10_3col[dat10_3col$A2>7, "A2"] # 7.3 8.7 7.5 7.9 7.9 7.2 8.6
quantile(part_of_A2, 0.5) # 7.9

#Несколько квартилей
dat10_3col
quant = c(0.33, 0.5, 0.8, 1)
quantile(dat10_3col$A2, quant)

#График квантилей
kvart = c(0,0.1,0.25,0.5,0.75,0.9,1) 
res_A2 = quantile(dat10_3col$A2, kvart)
plot(res_A2, kvart, type = "b", main = "график квантилей", xlab = "Мощность A2", ylab = "Квантили", col = "red")

res_K2O = quantile(dat10_3col$K2O, kvart)
plot(res_K2O, kvart, type = "b", main = "график квантилей", xlab = "Содержание Калия", ylab = "Квантили", col = "red")

#Огива
#Количество значений
len = length(dat10_3col$A2); len
Y = 1:len; Y
# Накопленные частоты
U = Y/length(dat10_3col$A2); U
# ???
Y = Y/(length(dat10_3col$A2) + 1); Y # 0.03846154 0.07692308 0.11538462 ... 0.96153846
X = sort(dat10_3col$A2); X
plot(X, Y)

# строим гистограммы для свойства 
hist(dat10_3col$A2)
hist(dat10_3col$A2, border = "red")
hist(dat10_3col$A2, border = "red", main = "Гистрограмма мощности A2", col = "lightblue", xlab = "Мощность А2", ylab = "Количество вариант (абсолютная частота)")
hist(dat10_3col$K2O, border = "red", main = "Гистрограмма калия  K2O", col = "lightblue", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)")

hist(dat10_3col$K2O, border = "red", main = "Гистрограмма калия  K2O", col = "lightblue", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 22)


# Коробочка с усиками (минимум, нижний квартиль, медиана, верхний квартиль, макс)
boxplot(dat10_3col$A2)
boxplot(dat10_3col$K2O)

boxplot(dat10_3col$A2, ylab = "Мощность А2", main = "График мощности А2 (Коробочка с усиками)", col = "green")
boxplot(dat10_3col$K2O, ylab = "Калий K2O", main = "График калия K2O (Коробочка с усиками)", col = "green")

###########
# ПРАКТИКА
###########
#График квантилей
kvart = c(0,0.1,0.25,0.5,0.75,0.9,1) 
res_A2 = quantile(dat10_3col$A2, kvart)
plot(res_A2, kvart, type = "b", main = "график квантилей", xlab = "Мощность A2", ylab = "Квантили", col = "purple")

res_K2O = quantile(dat10_3col$K2O, kvart)
plot(res_K2O, kvart, type = "b", main = "график квантилей", xlab = "Содержание Калия", ylab = "Квантили", col = "purple")


# строим гистограммы для свойства 
hist(dat10_3col$A2)
hist(dat10_3col$A2, border = "purple", main = "Гистрограмма мощности A2", col = "yellow", xlab = "Мощность А2", ylab = "Количество вариант (абсолютная частота)")
hist(dat10_3col$K2O, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)")

hist(dat10_3col$K2O, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 4)
hist(dat10_3col$A2, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 4)

hist(dat10_3col$K2O, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 5)
hist(dat10_3col$A2, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 5)

hist(dat10_3col$K2O, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 7)
hist(dat10_3col$A2, border = "purple", main = "Гистрограмма калия  K2O", col = "yellow", xlab = "Калий", ylab = "Количество вариант (абсолютная частота)", breaks = 7)