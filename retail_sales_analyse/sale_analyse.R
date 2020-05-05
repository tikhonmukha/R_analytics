library(dplyr)
library(data.table)
library(ggplot2)

sales <- fread("sale_analyse.csv", stringsAsFactors = T, dec = ",")
head(sales)
str(sales)

# Убираем лишний столбец Наименование
sales <- sales[,-6]

# Преобразуем некоторые целочисленные переменные в факторные
sales$`Основной поставщик` <- as.factor(sales$`Основной поставщик`)
sales$Артикул <- as.factor(sales$Артикул)

# Убираем лишние символы * и пробелы из столбца Наименование
sales$Наименование <- as.character(sales$Наименование)
s <- as.matrix(sales$Наименование)
s <- apply(s, 1, function(x) ifelse(grep("*", x)==1, gsub("[*]", " ",x), x))
s <- trimws(s, which = c("right"))
s <- trimws(s, which = c("left"))
s <- as.data.frame(s)
colnames(s) <- "Наименование"
sales <- cbind(sales, s)
sales <- sales[,-8]
sales <- sales[,c(1:7,20,8:19)]

# Преобразование в dplyr
sales_dp <- as_tibble(sales)
sales_dp <- sales_dp %>% 
  filter(Магазин != "AUCHAN.UA-MAG") %>%
  mutate("Доля_рынка" = `Товарооборот с НДС (грн.)`/sum(`Товарооборот с НДС (грн.)`,
                                                        na.rm = T)*100,
         "Товарные_запасы" = `Количество товаров (шт.)` * `Тариф закупки (грн.)`) %>% 
  rename("Номенклатура" = `Название уровня номенклатуры`, 
         "Упаковка" = `Формат упаковки`,
         "Поставщик" = `Основной поставщик`,
         "Артикул GICA" = `Статус артикула в GICA`)

# Категории с наибольшим товарооборотом
sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Количество_продаж" = n(), 
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(Товарооборот_млн_грн)) %>% 
  head(10)

sales_dp_cat <- sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Количество_продаж" = n(), 
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(Товарооборот_млн_грн)) %>% 
  head(10)

sales_dp_cat$Номенклатура <- reorder(sales_dp_cat$Номенклатура, sales_dp_cat$Товарооборот_млн_грн, fun = max)

ggplot(sales_dp_cat, aes(x = Номенклатура, y = Товарооборот_млн_грн, fill = Товарооборот_млн_грн))+
  geom_col()+
  xlab("Товарные категории")+
  ylab("Товарооборот (млн. грн.)")+
  labs(fill = "Товарооборот (млн. грн.)")+
  ggtitle("Топ-10 категорий по товарообороту")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.75)))

# Наиболее продаваемые категории
sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Количество_продаж" = n(), 
            "Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(Количество_продаж)) %>% 
  head(10)

sales_dp_sal <- sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Количество_продаж" = n(), 
            "Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>% 
  arrange(desc(Количество_продаж)) %>% 
  head(10)

sales_dp_sal$Номенклатура <- reorder(sales_dp_sal$Номенклатура, sales_dp_sal$Количество_продаж, fun = max)

ggplot(sales_dp_sal, aes(x = Номенклатура, y = Количество_продаж, fill = Количество_продаж))+
  geom_col()+
  xlab("Товарные категории")+
  ylab("Количество продаж")+
  labs(fill = "Количество продаж")+
  ggtitle("Топ-10 категорий по количеству продаж")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

# Наиболее маржинальные категории
sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100,
            "Количество_продаж" = n(), 
            "Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>% 
  arrange(desc(`Средняя_маржа_%`)) %>% 
  head(10)

sales_dp_mar <- sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100,
    "Количество_продаж" = n(), 
            "Товарооборот_млн_грн" = sum(`Товарооборот с НДС (грн.)`, na.rm = T) / 1000,
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>% 
  arrange(desc(`Средняя_маржа_%`)) %>% 
  head(10)

sales_dp_mar$Номенклатура <- reorder(sales_dp_mar$Номенклатура, sales_dp_mar$`Средняя_маржа_%`, fun = max)

ggplot(sales_dp_mar, aes(x = Номенклатура, y = `Средняя_маржа_%`, fill = `Средняя_маржа_%`))+
  geom_col()+
  xlab("Товарные категории")+
  ylab("Средняя маржинальность, %")+
  labs(fill = "Средняя маржинальность, %")+
  ggtitle("Топ-10 категорий по средней маржинальности")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

#Корреляционный тест
sales_dp_cor <- sales_dp %>% 
  group_by(Номенклатура) %>% 
  summarise("Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100,
            "Количество_продаж" = n()) %>% 
  arrange(desc(`Средняя_маржа_%`))

cor.test(sales_dp_cor$`Средняя_маржа_%`, sales_dp_cor$Количество_продаж)

# Продажи по периодам
sales_dp %>% 
  group_by(Период) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Количество_продаж" = n(), 
            "Средний_чек" = (sum(`Товарооборот с НДС (грн.)`, 
                                na.rm = T)) / n()) %>% 
  arrange(desc(Товарооборот_грн))

sales_dp_per <- sales_dp %>% 
  group_by(Период) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Количество_продаж" = n(), 
            "Средний_чек" = (sum(`Товарооборот с НДС (грн.)`, 
                                 na.rm = T)) / n()) %>% 
  arrange(desc(Товарооборот_грн))

sales_dp_per$Период <- reorder(sales_dp_per$Период, sales_dp_per$Товарооборот_грн, fun = max)

ggplot(sales_dp_per, aes(x = Период, y = Товарооборот_грн, fill = Период))+
  geom_col(width = 0.5)+
  xlab("Период")+
  ylab("Товарооборот (грн.)")+
  labs(fill = "Период")+
  ggtitle("Сравнение по периодам")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

# Продажи по магазинам
sales_dp %>% 
  group_by(Магазин) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                                 na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>% 
  arrange(desc(Товарооборот_грн))

sales_dp_shop <- sales_dp %>% 
  group_by(Магазин) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>% 
  arrange(desc(Товарооборот_грн))

sales_dp_shop$Доля_рынка <- round(sales_dp_shop$Доля_рынка, digits = 2)
sales_dp_shop$Кумулятивная_доля <- cumsum(sales_dp_shop$Доля_рынка)
sales_dp_shop$ymin <- c(0, head(sales_dp_shop$Кумулятивная_доля, n=-1))
sales_dp_shop$labelPosition <- (sales_dp_shop$Кумулятивная_доля + sales_dp_shop$ymin) / 2
sales_dp_shop$label <- paste0(sales_dp_shop$Магазин, "\n Доля: ", sales_dp_shop$Доля_рынка, "%")

ggplot(sales_dp_shop, aes(ymax=Кумулятивная_доля, ymin=ymin, xmax=4, xmin=3, fill=Магазин)) +
  geom_rect() +
  geom_text( x=2.25, aes(y=labelPosition, label=label, color=Магазин), size=3.5) + # x here controls label position (inner / outer)
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  ggtitle("Доли магазинов в совокупном товарообороте")+
  theme(legend.position = "none")

# Анализ категорий "Пельмени" и "Вареники"
sales_dt <- data.table(sales_dp)
sales_dt[, Номенклатура := as.character(Номенклатура)]
sales_dt_v <- sales_dt[Номенклатура == "ВАРЕНИКИ",]
sales_dt_p <- sales_dt[Номенклатура == "ПЕЛЬМЕНІ",]
sales_dp_pv <- as_tibble(rbind(sales_dt_p, sales_dt_v))
sales_dp_v <- as_tibble(sales_dt_v)
sales_dp_p <- as_tibble(sales_dt_p)
sales_dp_pv <- mutate_each(sales_dp_pv, funs(as.factor(.)), Номенклатура)
sales_dp_v <- mutate_each(sales_dp_v, funs(as.factor(.)), Номенклатура)
sales_dp_p <- mutate_each(sales_dp_p, funs(as.factor(.)), Номенклатура)

sales_dp_pv %>% 
  group_by(Номенклатура) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Количество_продаж" = n(),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>%
  arrange(desc(Товарооборот_грн))

sales_dp_pv_d <- sales_dp_pv %>% 
  group_by(Номенклатура) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T),
            "Количество_продаж" = n(),
            "Средняя_маржа_%" = mean(`Маржа FIFO`, na.rm = T) * 100) %>%
  arrange(desc(Товарооборот_грн))

ggplot(sales_dp_pv_d, aes(x = Номенклатура, y = Товарооборот_грн, fill = Номенклатура))+
  geom_col(width = 0.5)+
  xlab("Категории")+
  ylab("Товарооборот (грн.)")+
  labs(fill = "Категории")+
  ggtitle("Сравнение категорий")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))

sales_dp_p %>% 
  group_by(Номенклатура, Наименование) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>%
  arrange(desc(Товарооборот_грн)) %>% 
  top_n(10, Доля_рынка)

sales_dp_p_d <- sales_dp_p %>% 
  group_by(Номенклатура, Наименование) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>%
  arrange(desc(Товарооборот_грн)) %>% 
  top_n(10, Доля_рынка)

sales_dp_p_d$Наименование <- reorder(sales_dp_p_d$Наименование, sales_dp_p_d$Товарооборот_грн, fun = max)

ggplot(sales_dp_p_d, aes(x = Наименование, y = Товарооборот_грн, fill = Товарооборот_грн))+
  geom_col()+
  xlab("Наименования")+
  ylab("Товарооборот (грн.)")+
  labs(fill = "Товарооборот (грн.)")+
  ggtitle("Топ-10 товаров в категории Пельмени")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.8)))

sales_dp_v %>% 
  group_by(Номенклатура, Наименование) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>%
  arrange(desc(Товарооборот_грн)) %>% 
  top_n(10, Доля_рынка)

sales_dp_v_d <- sales_dp_v %>% 
  group_by(Номенклатура, Наименование) %>% 
  summarise("Товарооборот_грн" = sum(`Товарооборот с НДС (грн.)`, 
                                     na.rm = T),
            "Доля_рынка" = sum(Доля_рынка, na.rm = T)) %>%
  arrange(desc(Товарооборот_грн)) %>% 
  top_n(10, Доля_рынка)

sales_dp_v_d$Наименование <- reorder(sales_dp_v_d$Наименование, sales_dp_v_d$Товарооборот_грн, fun = max)

ggplot(sales_dp_v_d, aes(x = Наименование, y = Товарооборот_грн, fill = Товарооборот_грн))+
  geom_col()+
  xlab("Наименования")+
  ylab("Товарооборот (грн.)")+
  labs(fill = "Товарооборот (грн.)")+
  ggtitle("Топ-10 товаров в категории Вареники")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)))



# Регрессионный анализ

sales_dp_regr <- sales_dp[,c(16,18,22)]

sales_dp
sales_dp_regr <- sales_dp %>% 
  group_by(Магазин, Номенклатура) %>% 
  summarise("Товарооборот" = sum(`Товарооборот с НДС (грн.)`, na.rm = T),
            "Запасы" = sum(Товарные_запасы, na.rm = T),
            "Медианная_маржинальность" = median(`Маржа FIFO`, na.rm = T))
            
filter(sales_dp_regr, funs(is.na(.)))


fit <- lm(Товарооборот ~ ., sales_dp_regr)
summary(fit)
step(fit, direction = "backward")
pairs(sales_dp[,c(16,17,18,22)])
