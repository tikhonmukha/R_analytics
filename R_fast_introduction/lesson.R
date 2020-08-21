#Векторы
##Создадим первый вектор
a <- 1
a

##Создадим вектор из нескольких элементов
b <- c(1,2,3)
b
typeof(b)

d <- c("Anna", "Josh")
d
typeof(d)

e <- c("Jack", 7, 8)
e
typeof(e)

##Работа с элементами вектора
x <- c(88,5,12,13)
x
x[2]
x[c(1,3)]
x[2:4]
x[-3]
x[c(3,2,4,1)]
x[1:5]
x <- c(x[1:3], 168, x[4])
x
x[3] <- 22
x

##Некоторые функции для работы с векторами
length(x) #сколько элементов в векторе?

a <- c(1,2,3)
sum(a) #сумма
mean(a) #среднее
median(a) #медиана
max(a) #максимум
min(a) #минимум
which.max(a) #номер максимального элемента в векторе
which.min(a) #номер минимального элемента в векторе

b <- c(4,5,6)
a + b #попарное сложение векторов
a*b #попарное умножение векторов

seq(from = 1, to = 10, by = 1) #создание последовательности чисел
rep(a, times = 3) #создание последовательности чисел путем их повторения
rep(a, each = 3) 

x>8 #условные функции
any(x>8) 
all(x>8)
ifelse(x > 8, "ok", "not ok")

x <- c(1,2,3,NA) #работа с NA-значениями
x
sum(x, na.rm = T) #убираем NA значения в функциях
mean(x, na.rm = T) 
max(x, na.rm = T)
is.na(x)
anyNA(x)
x <- na.omit(x)
x
sum(x)

x[x>1] #фильтрация и сортировка векторов
x[x<mean(x)]
order(x)
order(-x)

#Датафреймы
##Создадим первый датафрейм
kids <- c("John", "Anna", "Jill")
marks <- c(10, 8, 6)
df1 <- data.frame(kids, marks) 
df1
str(df1) #структура датафрейма
nrow(df1) #сколько строк в датафрейме?
ncol(df1) #сколько столбцов в датафрейме?

##Выбор элементов датафрейма
df1
df1$kids #возьмем отдельный столбец - он станет вектором
df1$marks
df1[,2, drop = F] #а так столбец останется датафреймом
df1[2,] #а теперь возьмем одну запись
df[2:3,] #а теперь несколько записей
df1[2,2]
df1[df1$kids == "Anna",] #если нам нужна Анна
df1[df1$marks >7,] #фильтрация записей по условию
df1[df1$marks >7 & df1$kids == "John",] #если условий несколько
subset(df1, df1$marks>7)

##если есть NA значения
df_na <- data.frame(kids = c("Ben", NA, "Josh"), marks = c(NA, 4, 6))
df1 <- rbind(df1, df_na)
df1
df1[complete.cases(df1),]
df1 <- na.omit(df1)

##вычисляемые столбцы
prev_marks <- c(7,8,10,9)
df1 <- cbind(df1, prev_marks)
df1
df1$diff <- df1$marks - df1$prev_marks
df1

#Базовая графика
##функция plot
t <- mtcars
str(t)
head(t) #смотрим на начало и конец данных
tail(t)
plot(t$mpg) #базовый точечный график
plot(t$mpg, t$hp)
hist(t$drat) #гистограмма
boxplot(t$mpg ~ t$cyl) #ящик с усами

#Работа с данными - пакет dplyr
library(dplyr)
dt <- iris
head(iris)
tail(iris)
str(iris)

dp <- tibble(dt) #из обычного датафрейма - в dplyr-датафрейм
dp
glimpse(dp)

##отбор столбцов/строк, фильтрация, сортировка, переименование, вычисляемые столбцы
select(dp, Sepal.Length, Species)
select(dp, Sepal.Length:Petal.Length)
select(dp, 1:3)
select(dp, -Sepal.Width)
select(dp, contains("Sepal"))
select(dp, starts_with("Sepal"))
select(dp, ends_with("Width"))

slice(dp, 12:25)
slice(dp, 3, 8, 22, 47)
slice(dp, -c(10:150))

filter(dp, Sepal.Length > 5)
filter(dp, Species != "setosa")
filter(dp, Species == "versicolor" & Sepal.Width >= mean(Sepal.Width))

arrange(dp, Sepal.Length)
arrange(dp, desc(Sepal.Length))

rename(dp, "pet_len" = Petal.Length)

mutate(dp, Sepal_diff = Sepal.Length - mean(Sepal.Length))
mutate_if(dp, is.numeric, funs(.+1))
mutate_if(dp, is.factor, funs(as.character(.)))

## оператор %>%, группировка, агрегирующие функции summarise, join'ы
dp_group <- group_by(dp, Species) #группируем чтобы применить аггрегирующие функции
dp_group
summarise(dp_group, max_sepal_length = max(Sepal.Length))

dp %>% 
  group_by(Species) %>% 
  summarise(max_sepal_length = max(Sepal.Length)) #сила оператора %>% 

dp_species <- dp %>% 
  select(Species, Sepal.Length) %>% 
  filter(Sepal.Length > mean(Sepal.Length)) %>% 
  group_by(Species) %>% 
  summarise(sep_max = max(Sepal.Length), sep_min = min(Sepal.Length),
            sep_med = median(Sepal.Length), obs_num = n()) %>% 
  arrange(desc(sep_max))
dp_species

customers <- tibble(data.frame(id = factor(c("001","002","003")),
                               name = c("Jack", "John", "Anna"),
                               city = c("London","Paris","Berlin"))) #join'ы
customers
purchases <- tibble(data.frame(cust_id = factor(c("002","001","001","003","002")),
                               purchase_sum = c(500,350,200,660,890)))
purchases

purchases %>% 
  left_join(customers, by = c("cust_id" = "id"))

purchases %>% 
  left_join(customers, by = c("cust_id" = "id")) %>% 
  select(city, purchase_sum) %>% 
  group_by(city) %>% 
  summarise(tot_sum = sum(purchase_sum)) %>% 
  arrange(desc(tot_sum)) #dplyr - аналог sql


#Продвинутая графика - ggplot2
library(ggplot2)
d <- tibble(mtcars)
d
d <- d %>% 
  mutate_at(.vars = c("cyl", "vs", "am", "gear", "carb"), .funs = factor) %>% 
  mutate(car_num = seq(1, nrow(d), by = 1))
d

##qplot - базовая функция
qplot(data = d, x = hp, y = mpg)
qplot(data = d, x = hp, y = mpg, geom = "point")
qplot(data = d, x = mpg)
qplot(data = d, x = hp, y = mpg, color = cyl)
qplot(data = d, x = hp, y = mpg, shape = cyl)

##ggplot и настройки графиков
ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point()

ggplot(data = d, aes(x = cyl, y = mpg))+
  geom_boxplot()

ggplot(data = d, aes(x = car_num, y = mpg))+
  geom_line()

ggplot(data = d, aes(x = factor(car_num), y = mpg))+
  geom_col()

d_cyl <- d %>% 
  select(cyl, mpg) %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg)) %>% 
  arrange(desc(avg_mpg))
d_cyl

ggplot(data = d_cyl, aes(x = cyl, y = avg_mpg))+
  geom_col()

ggplot(data = d_cyl, aes(x = cyl, y = avg_mpg, fill = cyl))+
  geom_col()

ggplot(data = d_cyl, aes(x = cyl, y = avg_mpg, fill = avg_mpg))+
  geom_col()

###подпишем оси и дадим название графику
ggplot(data = d_cyl, aes(x = cyl, y = avg_mpg, fill = cyl))+
  geom_col()+
  labs(title = "Зависимость скорости от числа цилиндров",
       x = "Количество цилиндров",
       y = "Средняя скорость")

###разместим легенду и название графика красиво
ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  labs(title = "Зависимость скорости автомобиля от мощности двигателя",
       x = "Мощность двигателя",
       y = "Скорость автомобиля",
       color = "Число цилиндров")

###настроим оси
ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point()+
  scale_x_continuous(breaks = seq(50, 350, by = 25), limits = c(50, 350))+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  labs(title = "Зависимость скорости автомобиля от мощности двигателя",
       x = "Мощность двигателя",
       y = "Скорость автомобиля",
       color = "Число цилиндров")

ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point(alpha = I(0.5))+
  scale_x_continuous(breaks = seq(50, 350, by = 25), limits = c(50, 350))+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  labs(title = "Зависимость скорости автомобиля от мощности двигателя",
       x = "Мощность двигателя",
       y = "Скорость автомобиля",
       color = "Число цилиндров")

###разобьем один график на несколько
ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point(alpha = I(0.5))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Зависимость скорости автомобиля от мощности двигателя",
       x = "Мощность двигателя",
       y = "Скорость автомобиля")+
  facet_wrap(~ cyl)

ggplot(data = d, aes(x = hp, y = mpg, color = cyl))+
  geom_point(alpha = I(0.5))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Зависимость скорости автомобиля от мощности двигателя",
       x = "Мощность двигателя",
       y = "Скорость автомобиля")+
  facet_wrap(~ cyl, scales = "free")

#Кейс: топ-10 производителей-брендов, топ-20 скю, abc-анализ, визуализации
yog <- read.csv("yogurts.csv", header = T, sep = ";", dec = ",", stringsAsFactors = T, na.strings = c("NA",""))
head(yog)
tail(yog)
str(yog)

yog_dp <- tibble(yog)
yog_dp
yog_dp <- yog_dp %>% 
  rename("MANUFACTURER"=BRAND.OWNER, "NUM_SEL_DIST"=NUMERIC.SELLING.DISTRIBIUTION..AVG.,
         "WEI_SEL_DIST"=WEIGHTED.SELLING.DISTRIBIUTION..AVG.,"OFFTAKE"=Average.Sales.Per.Shop.Selling) %>% 
  select(1:6,8,10:17)

##Топ-10 производителей-брендов
yog_dp_man_br <- yog_dp %>% 
  filter(PERIOD == "MAT APR'20") %>% 
  select(MANUFACTURER, BRAND, VOLUME.SALES) %>%
  mutate(volume_share = VOLUME.SALES/sum(VOLUME.SALES, na.rm = T)*100) %>% 
  group_by(MANUFACTURER, BRAND) %>% 
  summarise(tot_volume_share = sum(volume_share, na.rm = T)) %>% 
  arrange(desc(tot_volume_share))
yog_dp_man_br  

yog_dp_top10man <- yog_dp %>% 
  filter(PERIOD == "MAT APR'20") %>% 
  select(MANUFACTURER, VOLUME.SALES) %>%
  mutate(volume_share = VOLUME.SALES/sum(VOLUME.SALES, na.rm = T)*100) %>% 
  group_by(MANUFACTURER) %>% 
  summarise(tot_volume_share = sum(volume_share, na.rm = T)) %>% 
  arrange(desc(tot_volume_share)) %>% 
  head(10)
yog_dp_top10man

yog_dp_top10_man_br <- yog_dp_top10man %>% 
  left_join(yog_dp_man_br, by = c("MANUFACTURER"="MANUFACTURER")) %>% 
  select(MANUFACTURER, BRAND, tot_volume_share.y) %>% 
  rename("tot_volume_share" = tot_volume_share.y)
yog_dp_top10_man_br

##топ-20 скю
yog_dp_top20_sku <- yog_dp %>% 
  filter(PERIOD == "MAT APR'20") %>% 
  select(SKU, VOLUME.SALES) %>% 
  mutate(volume_share = VOLUME.SALES/sum(VOLUME.SALES, na.rm = T)*100) %>% 
  arrange(desc(volume_share)) %>% 
  head(20)

##abc-анализ
yog_dp_abc <- yog_dp %>% 
  filter(PERIOD == "MAT APR'20") %>% 
  select(SKU, VOLUME.SALES, WEI_SEL_DIST) %>%
  mutate(volume_share = VOLUME.SALES/sum(VOLUME.SALES, na.rm = T)*100) %>% 
  arrange(desc(volume_share)) %>% 
  mutate(volume_cumsum = cumsum(volume_share)) %>% 
  mutate(abc_group = factor(case_when(volume_cumsum <= 80 ~ "A",
                               volume_cumsum > 80 & volume_cumsum <= 95 ~ "B",
                               volume_cumsum > 95 ~ "C")))
yog_dp_abc

##визуализация abc-анализа
ggplot(data = yog_dp_abc, aes(x = WEI_SEL_DIST, y = VOLUME.SALES, color = abc_group))+
  geom_point(alpha = I(0.5), na.rm = T)+
  scale_y_continuous(breaks = seq(0, 600, by = 100), limits = c(0, 600))+
  labs(x = "Средневзвешенная дистрибуция", y = "Объем продаж в тоннаже",
       title = "ABC-анализ", color = "Группа")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
  
yog_dp_abc_gr <- yog_dp_abc %>% 
  select(abc_group, volume_share) %>% 
  group_by(abc_group) %>% 
  summarise(tot_volume_share = sum(volume_share)) %>% 
  arrange(desc(tot_volume_share))

ggplot(data = yog_dp_abc_gr, aes(x = abc_group, y = tot_volume_share, fill = tot_volume_share))+
  geom_col()+
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,100))+
  labs(x = "Доля рынка", y = "Группа",
       title = "ABC-анализ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
