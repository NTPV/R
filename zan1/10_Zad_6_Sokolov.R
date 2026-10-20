#6 Фреймы (таблицы данных), создать фрейм, загрузить из файла и сохранить в файл
#Теория 1.6
x=5/0;x #Inf

# Задаем значения первой колонки
n=c(1.2,2.3,4.5);n

# Задаем значения второй колонки
s=c("aa","bb","cc");s

# Задаем значения третьей колонки
b=c(TRUE,FALSE,TRUE);b

#Сделаем четвертую колонку в df
newcolumn = c("Math", "Ecology", "Statistics")

# Создаем data frame
df=data.frame(n,s,b, newcolumn)
df

# Обращаемся к переменной n (к столбцу с названием "n")
df$n # 1.2 2.3 4.5
df$b # TRUE FALSE  TRUE

# Обращаемся к именам колок
names(df) #"n" "s" "b" 

# Обращаемся к именам строк
rownames(df) #"1" "2" "3" "newcolumn"


# Смотрим общую статистическую характеристику
summary(df)


#___________ Работа с файлами _________________#

# Считаем данные из файла csv в переменную pole
pole=read.table("pole123_sokolov.csv",header = TRUE, sep = " ",dec = ".") 
#sep это separator - разделитель между значениями, dec это decimal - разделитель в десятичной дроби (4,2 разделитель ",")
pole

# Через метод read.csv
data = read.csv ("pole123_sokolov.csv", header = TRUE, sep = " ", dec = ".")
data

#___________ Сохранение файлов _________________#
# между значениями разделитель " ff ", десятичные дроби разделены звёздочкой
write.table(pole, "razdelitelff.csv", sep=" ff ", dec = "*", quote=FALSE)

# Посмотрим ранее созданный табличный объект df
df
write.table(df, "df.csv", sep = "|_|", dec = ",", quote = FALSE)
write.table(df, "df___.csv", sep = "___", dec = ".", quote = FALSE)


#Задание 1.6
novob10 = read.csv ("pole123_sokolov.csv", header = TRUE,    sep = " ", dec = ".")
novob10

#Размерность (dim)
dim(novob10)
# Общая характеристика переменных (summary)
summary(novob10)
# Выведите названия столбцов (names)
names(novob10)

#Отдельно выведите каждую переменную ($)
x = novob10$Humus; x
y = novob10$K2O; y
z = novob10$Pole; z
s = novob10$A1; s

#_________________График Гумус-Калий________________________#
#Постройте график между содержанием гумуса и обменного калия.

#Столбчатая диаграмма
barplot(x,y, xlab = "Гумус", ylab = "Калий", col = "green")
#график
plot(x,y, xlab = "Гумус", ylab = "Калий", col = "blue")

novob10
write.csv(novob10, "novob.csv", quote = FALSE)

