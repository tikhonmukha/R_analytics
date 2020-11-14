library(dplyr) #data manipulating
library(tidyr) #data manipulating
library(stringr) #string operations
library(ggplot2) #visualizations
library(lmtest) #stat tests
library(car) #stat tests
library(caret) #stat tests
library(sandwich) #stat tests
library(nortest) #stat test
 
#reading and transforming data
airbnb <- tibble(read.csv("AB_US_2020.csv", header = T, na.strings = "", stringsAsFactors = T))
head(airbnb)
tail(airbnb)
glimpse(airbnb)
summary(airbnb)

airbnb <- airbnb %>% 
  rename(id = п.їid) %>% 
  mutate(day = str_sub(last_review,1,2), month = str_sub(last_review,4,5),
         year = factor(str_sub(last_review,7,8))) %>% 
  mutate(across(c("id","host_id"), factor))
  
levels(airbnb$year)

airbnb <- airbnb %>% 
  mutate(year = case_when(as.integer(year) == 10 ~ "2010",
                          as.integer(year) == 11 ~ "2011",
                          as.integer(year) == 12 ~ "2012",
                          as.integer(year) == 13 ~ "2013",
                          as.integer(year) == 14 ~ "2014",
                          as.integer(year) == 15 ~ "2015",
                          as.integer(year) == 16 ~ "2016",
                          as.integer(year) == 17 ~ "2017",
                          as.integer(year) == 18 ~ "2018",
                          as.integer(year) == 19 ~ "2019",
                          as.integer(year) == 20 ~ "2020")) %>% 
  mutate(last_review = as.Date(str_c(year,month,day,sep = "-"))) %>% 
  mutate(cur_date = Sys.Date()) %>% 
  mutate(days_since_last_review = as.integer(cur_date-last_review)) %>% 
  select(-c("day","month","year"))

#exploring the variables
##main variable
hist(airbnb$price)
boxplot(airbnb$price)

##deleting outliers
Q <- quantile(airbnb$price, probs = c(0.25,0.75), na.rm = F)
Q
iqr <- IQR(airbnb$price)
iqr
up_border <- Q[2]+1.5*iqr
low_border <- ifelse((Q[1]-1.5*iqr)<0,0,Q[1]-1.5*iqr)
eliminated <- subset(airbnb, airbnb$price>low_border & airbnb$price<up_border)

hist(eliminated$price)
boxplot(eliminated$price)
ad.test(eliminated$price) #Anderson-Darling normality test

##factor variables
levels(eliminated$neighbourhood_group)
tapply(eliminated$price, eliminated$neighbourhood_group, var)
leveneTest(data = eliminated, price ~ neighbourhood_group) #checking out for groups variation homogeneity
kruskal.test(data = eliminated, price ~ neighbourhood_group) #we can't use ANOVA cause there are no homogeneity between groups variations

levels(eliminated$room_type)
tapply(eliminated$price, eliminated$room_type, var)
leveneTest(data = eliminated, price ~ room_type)
kruskal.test(data = eliminated, price ~ room_type)

levels(eliminated$city)
tapply(eliminated$price, eliminated$city, var)
leveneTest(data = eliminated, price ~ city)
kruskal.test(data = eliminated, price ~ city)

##continuous variables
sapply(na.omit(eliminated[,sapply(eliminated, function(col) is.numeric(col))==T]), function(col) summary(col))
cor(na.omit(eliminated[,c(10,11,12,14,15,16,19)]))

hist(eliminated$minimum_nights) 
summary(eliminated$minimum_nights) #it can't be 1 million minimum nights...
Q_min_nights <- quantile(eliminated$minimum_nights, probs = c(0.25,0.75), na.rm = F)
Q_min_nights
iqr_min_nights <- IQR(eliminated$minimum_nights)
iqr_min_nights
up_border_min_nights <- Q_min_nights[2]+1.5*iqr_min_nights
low_border_min_nights <- ifelse((Q_min_nights[1]-1.5*iqr_min_nights)<0,1,Q_min_nights[1]-1.5*iqr_min_nights)
eliminated <- subset(eliminated, eliminated$minimum_nights>=low_border_min_nights & eliminated$minimum_nights<=up_border_min_nights)
hist(eliminated$minimum_nights)
boxplot(eliminated$minimum_nights)
summary(eliminated$minimum_nights)
cor.test(x = eliminated$minimum_nights, y = eliminated$price, method = "spearman")

hist(eliminated$number_of_reviews)
boxplot(eliminated$number_of_reviews)
summary(eliminated$number_of_reviews)
cor.test(x = eliminated$number_of_reviews, y = eliminated$price, method = "spearman")

hist(eliminated$reviews_per_month)
boxplot(eliminated$reviews_per_month)
summary(eliminated$reviews_per_month)
cor.test(x = eliminated$reviews_per_month, y = eliminated$price, method = "spearman")

hist(eliminated$calculated_host_listings_count)
boxplot(eliminated$calculated_host_listings_count)
summary(eliminated$calculated_host_listings_count)
cor.test(x = eliminated$calculated_host_listings_count, y = eliminated$price, method = "spearman")

hist(eliminated$availability_365)
boxplot(eliminated$availability_365)
summary(eliminated$availability_365)
cor.test(x = eliminated$availability_365, y = eliminated$price, method = "spearman")

hist(eliminated$days_since_last_review)
boxplot(eliminated$days_since_last_review)
summary(eliminated$days_since_last_review)
cor.test(x = eliminated$days_since_last_review, y = eliminated$price, method = "spearman")

cor(eliminated[,c(11,12,14,15,16,19)], method = "spearman", use = "complete.obs")

#Dividing data on train and test
in_train <- createDataPartition(y = eliminated$price, p = 0.9, list = F)
eliminated_train <- eliminated[in_train,]
eliminated_test <- eliminated[-in_train,]

#Creating linear regression
model_0 <- lm(data = eliminated_train, price ~ minimum_nights+number_of_reviews+reviews_per_month+
                availability_365+calculated_host_listings_count+days_since_last_review)
summary(model_0)
plot(model_0)
hist(model_0$residuals)
ad.test(model_0$residuals) #checking for residuals normality - not ok
bptest(model_0) #checking for heteroscedasticity - not ok
dwtest(model_0) #checking for autocorrelation - not ok

##boxcox transforming
bc <- boxCox(model_0)
bc_opt <- bc$x[which.max(bc$y)]

model_0_bc <- lm(data = eliminated_train, price^bc_opt ~ minimum_nights+number_of_reviews+reviews_per_month+
                   availability_365+calculated_host_listings_count+days_since_last_review)
summary(model_0_bc)
plot(model_0_bc)
hist(model_0_bc$residuals)
ad.test(model_0_bc$residuals) #checking for residuals normality - not ok
bptest(model_0_bc) #checking for heteroscedasticity - not ok
dwtest(model_0_bc) #checking for autocorrelation - not ok

model_1 <- lm(data = eliminated_train, price ~ minimum_nights+number_of_reviews+reviews_per_month+
                availability_365+calculated_host_listings_count+days_since_last_review+room_type)
summary(model_1)
plot(model_1)
hist(model_1$residuals)
ad.test(model_1$residuals) #checking for residuals normality - not ok
bptest(model_1) #checking for heteroscedasticity - not ok
dwtest(model_1) #checking for autocorrelation - not ok

model_2 <- lm(data = eliminated_train, price ~ minimum_nights+number_of_reviews+reviews_per_month+
                availability_365+calculated_host_listings_count+days_since_last_review+room_type+city)
summary(model_2)
plot(model_2)
hist(model_2$residuals)
ad.test(model_2$residuals) #checking for residuals normality - not ok
bptest(model_2) #checking for heteroscedasticity - not ok
dwtest(model_2) #checking for autocorrelation - not ok

model_3 <- lm(data = eliminated_train, price ~ minimum_nights+number_of_reviews+reviews_per_month+
                availability_365+calculated_host_listings_count+
                days_since_last_review+room_type+city+neighbourhood_group)
summary(model_3)
plot(model_3)
hist(model_3$residuals)
ad.test(model_3$residuals) #checking for residuals normality - not ok
bptest(model_3) #checking for heteroscedasticity - not ok
dwtest(model_2) #checking for autocorrelation - not ok

model_4 <- lm(data = eliminated_train, price ~ room_type*minimum_nights+room_type*number_of_reviews+room_type*reviews_per_month+
                room_type*availability_365+calculated_host_listings_count+
                days_since_last_review)
summary(model_4)
plot(model_4)
hist(model_4$residuals)
ad.test(model_4$residuals) #checking for residuals normality - not ok
bptest(model_4) #checking for heteroscedasticity - not ok
dwtest(model_4) #checking for autocorrelation - not ok

#let's try quantille regression
