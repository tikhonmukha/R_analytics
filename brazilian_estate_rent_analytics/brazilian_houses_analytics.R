library(dplyr)
library(ggplot2)
library(GGally)

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

t.test(house_dp_lm$rent_amount ~ house_dp_lm$animal)

t.test(house_dp_lm$rent_amount ~ house_dp_lm$furniture)

hist(house_dp_lm$rent_amount, main = "rent.amount", 
     breaks = 30, xlab = "rent.amount")

hist(log(house_dp_lm$rent_amount), main = "rent.amount", 
     breaks = 30, xlab = "ln(rent.amount)")

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