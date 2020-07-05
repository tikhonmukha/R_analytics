library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(maps)
library(mapproj)

house <- read.csv("houses_to_rent_v2.csv", header = T, stringsAsFactors = T, encoding = "UTF-8")
str(house)

house_dp <- tibble(house)
house_dp <- house_dp %>% 
  mutate(rooms = as.factor(rooms), bathroom = as.factor(bathroom), 
         parking.spaces = as.factor(parking.spaces), floor = as.factor(floor)) %>% 
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
    select(col) %>% 
    mutate(index = as.factor(col)) %>% 
    rename("value" = col)
}

house_dp <- na.omit(house_dp)

i1 <- value_selection("area")
i2 <- value_selection("rent_amount")
i <- rbind(i1, i2)

ggplot(i, aes(x = index, y = value))+
  geom_boxplot(aes(fill = index))+
  facet_wrap(~ index, scales = "free")

ggplot(house_dp, aes(x = area, y = rent_amount))+
  geom_point(size = 2)

house_dp_lm <- house_dp[c(1:8,10)]

ggplot(house_dp_lm, aes(x = animal, y = rent_amount))+
  geom_boxplot(aes(fill = animal))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from animal accept dependance", x = "Animal", y = "Rent amount")

ggplot(house_dp_lm, aes(x = furniture, y = rent_amount))+
  geom_boxplot(aes(fill = furniture))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from furniture dependance", x = "Furniture", y = "Rent amount")

t.test(house_dp_lm$rent_amount ~ house_dp_lm$animal)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$furniture)

hist(house_dp_lm$rent_amount, main = "rent.amount", 
     breaks = 30, xlab = "rent.amount")

hist(log(house_dp_lm$rent_amount), main = "rent.amount", 
     breaks = 30, xlab = "ln(rent.amount)")

ggplot(house_dp_lm, aes(x = city, y = rent_amount))+
  geom_boxplot(aes(fill = city))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from city dependance", x = "City", y = "Rent amount")

ggplot(house_dp_lm, aes(x = rooms, y = rent_amount))+
  geom_boxplot(aes(fill = rooms))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of rooms dependance", x = "Number of rooms", y = "Rent amount")
  
ggplot(house_dp_lm, aes(x = bathroom, y = rent_amount))+
  geom_boxplot(aes(fill = bathroom))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of bathrooms dependance", x = "Number of bathrooms", y = "Rent amount")

ggplot(house_dp_lm, aes(x = parking_spaces, y = rent_amount))+
  geom_boxplot(aes(fill = parking_spaces))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from number of parking spaces dependance", x = "Number of parking spaces", y = "Rent amount")

ggplot(house_dp_lm, aes(x = floor, y = rent_amount))+
  geom_boxplot(aes(fill = floor))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Rent amount from floor number dependance", x = "Floor number", y = "Rent amount")

fit_aov <- aov(rent_amount ~ city+rooms+bathroom+parking_spaces+floor, data = house_dp_lm)
summary(fit_aov)

0.8*nrow(house_dp_lm)
house_dp_lm_train <- house_dp_lm[1:6500,]

fit_lm <- lm(log(rent_amount) ~ area + city + rooms + bathroom + parking_spaces + floor + furniture, data = house_dp_lm_train)
sum_fit_lm <- summary(fit_lm)
sum_fit_lm$r.squared

house_dp_lm_test <- house_dp_lm[6501:nrow(house_dp_lm),]

house_dp_lm_test <- house_dp_lm_test %>% 
  filter(as.integer(rooms) != 8 & as.integer(rooms) != 9) %>% 
  filter(as.integer(bathroom) != 8)

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
house_data <- gather(house_dp_lm, "index", "value", 3:8)
house_data <- house_data %>% 
  mutate(index = as.factor(index), value = as.factor(value))
str(house_data)

ggplot(house_data, aes(x = value))+
  geom_bar(aes(fill = index))+
  facet_wrap(~index, scales = "free")+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.9)))+
  guides(fill = FALSE)
