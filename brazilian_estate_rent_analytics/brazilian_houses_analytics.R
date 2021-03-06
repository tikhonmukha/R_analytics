library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(maps)
library(mapproj)
library(lmtest)
library(MASS)
library(caret)

house <- read.csv("houses_to_rent_v2.csv", header = T, stringsAsFactors = T, encoding = "UTF-8", na.strings = "-")
str(house)
house <- na.omit(house)

house_dp <- tibble(house)
house_dp <- house_dp %>% 
  rename("parking_spaces" = "parking.spaces", "homeowners_tax" = "hoa..R..",
         "rent_amount" = "rent.amount..R..", "property_tax" = "property.tax..R..", 
         "fire_insurance" = "fire.insurance..R..", "total_cost" = "total..R..")

sapply(na.omit(house_dp[,sapply(house_dp, function(col) is.numeric(col))==T]), function(col) summary(col))

ggpairs(na.omit(house_dp[sapply(house_dp, function(col) is.numeric(col))==T]))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Correlation matrix")

#rent_amount linear model

ggplot(house_dp, aes(x = area, y = total_cost))+
  geom_point(size = 2)

out_total <- order(house_dp$total_cost, decreasing = T)[1:6]
out_area <- order(house_dp$area,decreasing = T)[1:5]

house_dp <- house_dp[-c(out_total,out_area),]

plot(rent_amount ~ area,data = house_dp,pch=16)

value_selection <- function(col){
  house_dp %>% 
    dplyr::select(col) %>% 
    mutate(index = as.factor(col)) %>% 
    rename("value" = col)
}

i1 <- value_selection("area")
i2 <- value_selection("rent_amount")
i <- rbind(i1, i2)

ggplot(i, aes(x = index, y = value))+
  geom_boxplot(aes(fill = index))+
  facet_wrap(~ index, scales = "free")

ggplot(house_dp, aes(x = area, y = rent_amount))+
  geom_point(size = 2)

levels(house_dp$city)
house_dp$BeloHorizonte <- ifelse(house_dp$city == "Belo Horizonte", 1, 0)
house_dp$Campinas <- ifelse(house_dp$city == "Campinas", 1, 0)
house_dp$PortoAlegre <- ifelse(house_dp$city == "Porto Alegre", 1, 0)
house_dp$Rio <- ifelse(house_dp$city == "Rio de Janeiro", 1, 0)
house_dp$animalLog <- ifelse(house_dp$animal == "acept", 1, 0)
house_dp$furnitureLog <- ifelse(house_dp$furniture == "furnished", 1, 0)

house_dp_lm <- house_dp[c(1,14:19,2:6,10)]
house_dp_lm <- house_dp_lm %>% 
  mutate_each(factor, BeloHorizonte, Campinas, PortoAlegre, Rio, animalLog, furnitureLog)

ggplot(house_dp_lm, aes(x = animalLog, y = rent_amount))+
  geom_boxplot(aes(fill = animalLog))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from animal accept dependance", x = "Animal", y = "Rent amount")

ggplot(house_dp_lm, aes(x = furnitureLog, y = rent_amount))+
  geom_boxplot(aes(fill = furnitureLog))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from furniture dependance", x = "Furniture", y = "Rent amount")

t.test(house_dp_lm$rent_amount ~ house_dp_lm$animalLog)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$furnitureLog)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$BeloHorizonte)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$Campinas)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$PortoAlegre)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$Rio)

ggplot(house_dp_lm, aes(x = city, y = rent_amount))+
  geom_boxplot(aes(fill = city))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from city dependance", x = "City", y = "Rent amount")

ggplot(house_dp_lm, aes(x = as.factor(rooms), y = rent_amount))+
  geom_boxplot(aes(fill = as.factor(rooms)))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of rooms dependance", x = "Number of rooms", y = "Rent amount")
  
ggplot(house_dp_lm, aes(x = as.factor(bathroom), y = rent_amount))+
  geom_boxplot(aes(fill = as.factor(bathroom)))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of bathrooms dependance", x = "Number of bathrooms", y = "Rent amount")

ggplot(house_dp_lm, aes(x = as.factor(parking_spaces), y = rent_amount))+
  geom_boxplot(aes(fill = as.factor(parking_spaces)))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of parking spaces dependance", x = "Number of parking spaces", y = "Rent amount")

ggplot(house_dp_lm, aes(x = as.factor(floor), y = rent_amount))+
  geom_boxplot(aes(fill = as.factor(floor)))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from floor number dependance", x = "Floor number", y = "Rent amount")

fit_aov <- aov(rent_amount ~ rooms+bathroom+parking_spaces+floor, data = house_dp_lm)
summary(fit_aov)

hist(house_dp_lm$rent_amount, main = "rent.amount", 
     breaks = 30, xlab = "rent.amount")

hist(log(house_dp_lm$rent_amount), main = "rent.amount", 
     breaks = 30, xlab = "ln(rent.amount)")

ggpairs(house_dp_lm[sapply(house_dp_lm, function(col) is.numeric(col))==T])+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Correlation matrix")

0.8*nrow(house_dp_lm)
house_dp_lm_train <- house_dp_lm[1:6500,]

fit_lm <- lm(log(rent_amount) ~ (BeloHorizonte + Campinas + PortoAlegre + Rio + area + rooms + bathroom + parking_spaces + floor + furnitureLog + animalLog)^2, data = house_dp_lm_train)
sum_fit_lm <- summary(fit_lm)
sum_fit_lm$r.squared

house_dp_lm_test <- house_dp_lm[6501:nrow(house_dp_lm),]

house_dp_lm_test$predicted_rent <- exp(predict(object = fit_lm, newdata = house_dp_lm_test, type = "response"))

ggplot(house_dp_lm_test, aes(x = rent_amount, y = predicted_rent))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")

#Most expensive cities

map <- tibble(map_data("world"))
map <- map %>% 
  filter(region == "Brazil")

data <- tibble(world.cities)
data <- data %>% 
  filter(country.etc == "Brazil")

house_map <- house_dp_lm %>% 
  select(city, rent_amount) %>% 
  group_by(city) %>% 
  summarise(Rent_amount = median(rent_amount, na.rm = T)) %>% 
  arrange(desc(Rent_amount))

house_map <- left_join(house_map, data, by = c("city" = "name"))
house_map$city <-  as.factor(house_map$city)
house_map[1,]$lat <- -23.533773
house_map[1,]$long <- -46.625290

ggplot() +
  geom_polygon(data = map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_point(data = house_map, aes(x=long, y=lat, color=city, size = Rent_amount), na.rm = T) +
  geom_text_repel(data = house_map, aes(x=long, y=lat, label = city), size = 3.25) +
  scale_fill_distiller(palette = "Spectral") +
  scale_size_continuous(range=c(1,12)) +
  theme_minimal() + 
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_map() +
  guides(colour = FALSE, size = guide_legend(title = "Median rent amount")) +
  labs(title = "Most expensive cities", x = "Longitude", y = "Latitude")

#Most popular supply
house_data <- gather(house_dp_lm, "index", "value", c(3:5,7,8))
house_data <- house_data %>% 
  mutate(index = as.factor(index), value = as.factor(value))
str(house_data)

ggplot(house_data, aes(x = value))+
  geom_bar(aes(fill = index))+
  facet_wrap(~index, scales = "free")+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = FALSE)

house_data_floor <- gather(house_dp_lm, "index", "value", 6)
house_data_floor <- house_data_floor %>% 
  mutate(index = as.factor(index), value = as.integer(value))

ggplot(house_data_floor, aes(x = value))+
  geom_bar(aes(fill = index), na.rm = T)+
  scale_x_continuous(limits = c(0,35), breaks = seq(0, 35, 1))+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = FALSE)+
  labs(x = "Floor number", y = "Count")

#area-rent cluster analysis
house_cluster <- house_dp_lm %>% 
  select(area, rent_amount)

fit_clust <- kmeans(house_cluster, 3)
house_cluster$clusters <- factor(fit_clust$cluster)

ggplot(house_cluster, aes(x = area, y = rent_amount, col = clusters))+
  geom_point(size = 2)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  labs(title = "Houses cluster plot", x = "Rent amount", y = "Area")

fit_cl <- lm(rent_amount ~ area, data = house_cluster)  
bptest(fit_cl)
rent_amount_bcmod <- BoxCoxTrans(house_cluster$rent_amount)
house_cluster <- cbind(house_cluster, rent_new = predict(rent_amount_bcmod, house_cluster$rent_amount))

ftl_cl_bcmod <- lm(rent_new ~ area, data = house_cluster)
bptest(ftl_cl_bcmod)
ggplot(house_cluster, aes(x = area, y = rent_new))+
  geom_point(size = 2)

area_bcmod <- BoxCoxTrans(house_cluster$area)
house_cluster <- cbind(house_cluster, area_new = predict(area_bcmod, house_cluster$area))
ftl_cl_bcmod2 <- lm(rent_new ~ area_new, data = house_cluster)
bptest(ftl_cl_bcmod2)
ggplot(house_cluster, aes(x = area_new, y = rent_new))+
  geom_point(size = 2)
