library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)

my_data <- read_excel("curds abc xyz analysis.xlsx", sheet = "DATA", na = "NA", trim_ws = T)
my_data <- my_data[,c(1:6,8,11:19)]

my_data <- my_data %>% 
  rename("period" = PERIOD, "manufacturer" = MANUFACTURER, "brand" = BRAND,
         "segment" = SEGMENT, "type" = TYPE, "weight" = WEIGHT,
         "flavour" = FLAVOUR, "sku" = SKU,
         "fat_level" = `FAT LEVEL`, "pack_type" = `PACK TYPE`, 
         "volume_sales" = `VOLUME SALES`, "value_sales" = `VALUE SALES (1000 BYN)`,
         "wei_dist" = `WEIGHTED SELLING DISTRIBIUTION (AVG)`,
         "num_dist" = `NUMERIC SELLING DISTRIBIUTION (AVG)`,
         "price_per_unit" = `PRICE PER UNIT`, "vol_sale_per_wei_dist" = `VOL.SALE/W.DIST`) %>% 
  mutate(weight = str_replace_all(weight, " KG", ""),
         weight = as.double(ifelse(weight == "PRIVATE LABEL", 0, weight))) %>% 
  mutate("fat_group" = factor(case_when(fat_level > 0 & fat_level <= 0.05 ~ "0.1-5.0%",
                              fat_level > 0.05 & fat_level <= 0.1 ~ "5.1-10.0%",
                              fat_level > 0.1 & fat_level <= 0.15 ~ "10.1-15.0%",
                              fat_level > 0.15 & fat_level <= 0.2 ~ "15.1-20.0%",
                              fat_level > 0.2 & fat_level <= 0.25 ~ "20.1-25.0%",
                              fat_level == "PRIVATE LABEL" ~ "PRIVATE LABEL",
                              fat_level == "NO FAT/FAT FREE" ~ "NO FAT")),
         "weight_group" = factor(case_when(weight > 0 & weight <= 0.1 ~ "1-100 g",
                                           weight > 0.1 & weight <= 0.2 ~ "101-200 g",
                                           weight > 0.2 & weight <= 0.3 ~ "201-300 g",
                                           weight > 0.3 & weight <= 0.4 ~ "301-400 g",
                                           weight > 0.4 & weight <= 0.5 ~ "401-500 g",
                                           weight > 0.5 & weight <= 0.6 ~ "501-600 g",
                                           weight > 0.6 & weight <= 0.7 ~ "601-700 g",
                                           weight > 0.7 & weight <= 0.8 ~ "701-800 g",
                                           weight > 0.8 & weight <= 0.9 ~ "801-900 g",
                                           weight == 0 ~ "PRIVATE LABEL"))) %>% 
  mutate_at(c("period", "manufacturer", "brand", "segment", "type", "pack_type", "flavour", "sku"), list(~ factor(.)))

#Preparing data for abc xyz analysis
data_abcxyz <- my_data %>% 
  select(period, manufacturer, brand, sku, volume_sales)

data_abcxyz <- pivot_wider(data_abcxyz, c("manufacturer", "brand", "sku"), names_from = "period", values_from = "volume_sales")
data_abcxyz <- data_abcxyz %>% 
  select(-`APR'19`)

data_abcxyz$new <- ifelse(apply(data_abcxyz[,c(4:13,16:17)],1,sum) == 0, 1, 0)
table(data_abcxyz$new)
data_abcxyz$spring <- ifelse(apply(data_abcxyz[,c(4,14:15)],1,sum) != 0 & apply(data_abcxyz[,-c(1:3,4,14:19)],1,sum) == 0, 1, 0)
table(data_abcxyz$spring)
data_abcxyz$summer <- ifelse(apply(data_abcxyz[,c(5:7)],1,sum) != 0 & apply(data_abcxyz[,-c(1:3,5:7,16:20)],1,sum) == 0, 1, 0)
table(data_abcxyz$summer)
data_abcxyz$autumn <- ifelse(apply(data_abcxyz[,c(8:10)],1,sum) != 0 & apply(data_abcxyz[,-c(1:3,8:10,16:21)],1,sum) == 0, 1, 0)
table(data_abcxyz$autumn)
data_abcxyz$winter <- ifelse(apply(data_abcxyz[,c(11:13)],1,sum) != 0 & apply(data_abcxyz[,-c(1:3,11:13,16:22)],1,sum) == 0, 1, 0)
table(data_abcxyz$winter)
data_abcxyz$empty_months <- ifelse(apply(data_abcxyz[,-c(1:3,16:23)],1, function(x) sum(x == 0)) > 9, 1, 0)
table(data_abcxyz$empty_months)

data_abcxyz_pure <- data_abcxyz %>%
  filter(new != 1, spring != 1, summer != 1, autumn != 1, winter != 1, empty_months != 1) %>% 
  select(-c(new,spring,summer,autumn,winter,empty_months,`MAT APR'18`,`MAT APR'19`)) %>% 
  arrange(desc(`MAT APR'20`)) %>% 
  mutate("value_share" = `MAT APR'20`/sum(`MAT APR'20`)*100) %>% 
  mutate("share_cumsum" = cumsum(value_share))

data_abcxyz_pure <- data_abcxyz_pure %>% 
  mutate("variation_coef" = (apply(data_abcxyz_pure[4:15],1,sd)/apply(data_abcxyz_pure[4:15],1,mean))*100) %>% 
  mutate("category_abc" = as.factor(case_when(
    share_cumsum >= 0 & share_cumsum <= 80 ~ "A",
    share_cumsum > 80 & share_cumsum <= 95 ~ "B",
    share_cumsum > 95 ~ "C"))) %>% 
  mutate("category_xyz" = as.factor(case_when(
    variation_coef >= 0 & variation_coef <= 10 ~ "X",
    variation_coef > 10 & variation_coef <= 25 ~ "Y",
    variation_coef > 25 ~ "Z"))) %>% 
  mutate("category_abcxyz" = as.factor(str_c(category_abc, category_xyz)))

data_abcxyz_pure <- data_abcxyz_pure %>% 
  select(manufacturer, brand, sku, value_share, share_cumsum, variation_coef,
         category_abc, category_xyz, category_abcxyz)

#ABC analysis

data_abcxyz_pure_ABCgroups <- data_abcxyz_pure %>% 
  group_by(category_abc) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("all"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_ABCour <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abc) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("we"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_ABC <- rbind(data_abcxyz_pure_ABCgroups, data_abcxyz_pure_ABCour)

ggplot(data_abcxyz_pure_ABC, aes(x = category_abc, y = value_share, fill = manufacturer, label = round(value_share, digits = 1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  scale_fill_discrete(labels = c("Доля рынка", "Доля Савушкин продукт"))+
  labs(title = "Доля рынка каждой группы", x = "", y = "")

data_abcxyz_pure_ABCbrands <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abc, brand) %>% 
  summarise(sku_count = n())

data_abcxyz_pure_ABCbrandsagg <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abc) %>% 
  summarise(sku_count = n()) %>% 
  mutate(brand = factor("TOTAL")) %>% 
  select(c(1,3,2))

data_abcxyz_pure_ABCbrands <- rbind(data_abcxyz_pure_ABCbrands, data_abcxyz_pure_ABCbrandsagg)

ggplot(data_abcxyz_pure_ABCbrands, aes(x = brand, y = sku_count, fill = category_abc, label = sku_count))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  scale_y_continuous(limits = c(0, 30))+
  scale_fill_brewer()+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Количество SKU Савушкин продукт по брендам", x = "", y = "")

#XYZ analysis

data_abcxyz_pure_XYZgroups <- data_abcxyz_pure %>% 
  group_by(category_xyz) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("all"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_XYZour <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_xyz) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("we"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_XYZ <- rbind(data_abcxyz_pure_XYZgroups, data_abcxyz_pure_XYZour)

ggplot(data_abcxyz_pure_XYZ, aes(x = category_xyz, y = value_share, fill = manufacturer, label = round(value_share, digits = 1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  scale_fill_discrete(labels = c("Доля рынка", "Доля Савушкин продукт"))+
  labs(title = "Доля рынка каждой группы", x = "", y = "")

data_abcxyz_pure_XYZbrands <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_xyz, brand) %>% 
  summarise(sku_count = n())

data_abcxyz_pure_XYZbrandsagg <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_xyz) %>% 
  summarise(sku_count = n()) %>% 
  mutate(brand = factor("TOTAL")) %>% 
  select(c(1,3,2))

data_abcxyz_pure_XYZbrands <- rbind(data_abcxyz_pure_XYZbrands, data_abcxyz_pure_XYZbrandsagg)

ggplot(data_abcxyz_pure_XYZbrands, aes(x = brand, y = sku_count, fill = category_xyz, label = sku_count))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  scale_fill_brewer()+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Количество SKU Савушкин продукт по брендам", x = "", y = "")

#ABCXYZ analysis

data_abcxyz_pure_ABCXYZgroups <- data_abcxyz_pure %>% 
  group_by(category_abcxyz) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("all"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_ABCXYZour <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abcxyz) %>% 
  summarise(value_share = sum(value_share)) %>% 
  mutate(manufacturer = factor(c("we"))) %>% 
  select(c(3,1,2))

data_abcxyz_pure_ABCXYZ <- rbind(data_abcxyz_pure_ABCXYZgroups, data_abcxyz_pure_ABCXYZour)

ggplot(data_abcxyz_pure_ABCXYZ, aes(x = category_abcxyz, y = value_share, fill = manufacturer, label = round(value_share, digits = 1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  scale_fill_discrete(labels = c("Доля рынка", "Доля Савушкин продукт"))+
  labs(title = "Доля рынка каждой группы", x = "", y = "")

data_abcxyz_pure_matrix_all <- data_abcxyz_pure %>% 
  group_by(category_abc, category_xyz) %>% 
  summarise(sku_count = n()) %>% 
  mutate(manufacturer = factor(c("all"))) %>% 
  select(c(4,1,2,3))

data_abcxyz_pure_matrix_our <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abc, category_xyz) %>% 
  summarise(sku_count = n()) %>% 
  mutate(manufacturer = factor(c("we"))) %>% 
  select(c(4,1,2,3))

data_abcxyz_pure_matrix <- rbind(data_abcxyz_pure_matrix_all, data_abcxyz_pure_matrix_our)
data_abcxyz_pure_matrix$manufacturer <- str_replace_all(as.character(data_abcxyz_pure_matrix$manufacturer), "all", "Весь рынок")
data_abcxyz_pure_matrix$manufacturer <- str_replace_all(as.character(data_abcxyz_pure_matrix$manufacturer), "we", "Савушкин продукт")
data_abcxyz_pure_matrix$manufacturer <- factor(data_abcxyz_pure_matrix$manufacturer)

ggplot(data_abcxyz_pure_matrix, aes(x = category_xyz, y = category_abc, label = round(sku_count, digits = 1)))+
  geom_point(aes(size = sku_count, colour = manufacturer))+
  geom_text(position = position_dodge(width=0.9), angle=0, vjust=-1, hjust=0.5)+
  facet_wrap(~manufacturer)+
  scale_y_discrete(limits = c("C", "B", "A"))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Количество SKU каждой группы", x = "", y = "")

data_abcxyz_pure_ABCXYZbrands <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abcxyz, brand) %>% 
  summarise(sku_count = n())

data_abcxyz_pure_ABCXYZbrandsagg <- data_abcxyz_pure %>% 
  filter(manufacturer == "SAVUSHKIN PRODUCT" | manufacturer == "SANTA BREMOR") %>% 
  group_by(category_abcxyz) %>% 
  summarise(sku_count = n()) %>% 
  mutate(brand = factor("TOTAL")) %>% 
  select(c(1,3,2))

data_abcxyz_pure_ABCXYZbrands <- rbind(data_abcxyz_pure_ABCXYZbrands, data_abcxyz_pure_ABCXYZbrandsagg)

ggplot(data_abcxyz_pure_ABCXYZbrands, aes(x = brand, y = sku_count, fill = category_abcxyz, label = sku_count))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width= 0.9), angle=0, vjust=-1, hjust=0.5)+
  scale_fill_brewer()+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Количество SKU Савушкин продукт по брендам", x = "", y = "")

#ДОБАВИТЬ КО ВСЕМ ГИСТОГРАММАМ С ДОЛЯМИ НАКОПИТЕЛЬНУЮ ЛИНИЮ, СДЕЛАТЬ ДИАГРАММЫ ДЛЯ ABC И XYZ АНАЛИЗОВ