library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(maps)
library(mapproj)
library(lsr)

#Importing and transforming data

shootings_data <- tibble(read.csv("shootings.csv", header = T, sep = ",", dec = ".",
                           na.strings = c("Unknown", "unknown", "undetermined"), stringsAsFactors = T))

summary(shootings_data)

shootings_data <- shootings_data %>% 
  mutate(date = as.character(date)) %>% 
  mutate(year = factor(str_sub(date, 1, 4)),
         month = factor(str_sub(date, 6, 7)),
         day = factor(str_sub(date, 9, 10))) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

#Exploring variables

ggplot(data = shootings_data, aes(x = manner_of_death, fill = manner_of_death))+
  geom_bar()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Manner of death distribution", x = "", y = "Total number")

armed <- shootings_data %>% 
  select(armed) %>%
  na.omit(armed) %>% 
  group_by(armed) %>% 
  count(armed) %>% 
  arrange(desc(n)) %>%
  head(20)

ggplot(data = armed, aes(x = reorder(armed,-n), y = n, fill = armed))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(title = "Was the killed armed?", x = "", y = "Total number")

ggplot(data = shootings_data, aes(x = age, fill = "red"))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks = seq(0,100,by=10))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = "Total number", title = "Age distribution")

race_data <- shootings_data %>% 
  select(race) %>%
  na.omit(race) %>% 
  group_by(race) %>% 
  count(race) %>% 
  arrange(desc(n))

ggplot(data = race_data, aes(x = reorder(race,-n), y = n, fill = race))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Race distribution", x = "", y = "Total number")

city_data <- shootings_data %>% 
  select(city) %>%
  group_by(city) %>% 
  count(city) %>% 
  arrange(desc(n)) %>%
  head(10)

map <- tibble(map_data("world"))
map <- map %>% 
  filter(region == "USA")

data <- tibble(world.cities)
data <- data %>% 
  filter(country.etc == "USA")

city_data <- left_join(city_data, data, by = c("city" = "name"))
city_data <- city_data %>% 
  select(-c(country.etc,pop,capital)) %>% 
  mutate(city = factor(city)) %>% 
  distinct(city, .keep_all = TRUE)

ggplot() +
  geom_polygon(data = map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_point(data = city_data, aes(x=long, y=lat, color=city, size = 0.1*n), na.rm = T) +
  geom_text_repel(data = city_data, aes(x=long, y=lat, label = city), size = 2.5) +
  scale_x_continuous(limits = c(-200,0))+
  scale_fill_distiller(palette = "Spectral") +
  scale_size_continuous(range=c(1,12)) +
  theme_minimal() + 
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "none", plot.title = element_text(hjust = 0.5))+
  coord_map() +
  labs(title = "Top-10 cities by the number of killed", x = "Longitude", y = "Latitude")

state_data <- shootings_data %>% 
  select(state) %>%
  group_by(state) %>% 
  count(state) %>% 
  arrange(desc(n)) %>%
  head(20)

ggplot(data = state_data, aes(x = reorder(state,-n), y = n, fill = state))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "State distribution", x = "", y = "Total number")

ggplot(data = shootings_data, aes(x = signs_of_mental_illness, fill = signs_of_mental_illness))+
  geom_bar()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Did the killed has any signs of mental illness?", x = "", y = "Total number")

threat_level_data <- shootings_data %>% 
  select(threat_level) %>%
  na.omit(threat_level) %>% 
  group_by(threat_level) %>% 
  count(threat_level) %>% 
  arrange(desc(n))

ggplot(data = threat_level_data, aes(x = reorder(threat_level,-n), y = n, fill = threat_level))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Threat level distribution", x = "", y = "Total number")

flee_data <- shootings_data %>% 
  select(flee) %>%
  na.omit(flee) %>% 
  group_by(flee) %>% 
  count(flee) %>% 
  arrange(desc(n))

ggplot(data = flee_data, aes(x = reorder(flee,-n), y = n, fill = flee))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Flee method distribution", x = "", y = "Total number")

ggplot(data = shootings_data, aes(x = body_camera, fill = body_camera))+
  geom_bar()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Did the killed has body camera?", x = "", y = "Total number")

arms_category_data <- shootings_data %>% 
  select(arms_category) %>%
  na.omit(arms_category) %>% 
  group_by(arms_category) %>% 
  count(arms_category) %>% 
  arrange(desc(n))

ggplot(data = arms_category_data, aes(x = reorder(arms_category,-n), y = n, fill = arms_category))+
  geom_col()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(title = "Arm category distribution", x = "", y = "Total number")

date_by_year_data <- shootings_data %>% 
  select(date) %>% 
  group_by(date) %>% 
  count()

date_by_year_data <- xts::xts(date_by_year_data[,-1], order.by=date_by_year_data$date)
forecast::tsdisplay(date_by_year_data, main = "Kills daily dynamics", xlab = "Day", ylab = "Kills number") #no seasonality in kills daily dynamics

date_data <- shootings_data %>% 
  select(day, year, month) %>% 
  group_by(day, year, month) %>% 
  count()

ggplot(data = date_data, aes(x = as.numeric(day), y = n, colour = month))+
  geom_line()+
  facet_grid(month~year)+
  scale_y_continuous(breaks = seq(0,8,by=2))+
  scale_x_continuous(breaks = seq(0,30,by=5))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Kills dynamics by months and years", x = "Month number", y = "Total kills")

#Testing factor variables

cramersV(table(shootings_data$armed, shootings_data$manner_of_death)) #correlation between two factor variables
table(shootings_data$armed, shootings_data$manner_of_death)
fisher.test(table(shootings_data$armed, shootings_data$manner_of_death), simulate.p.value = T) #very few observations (less than 5) in many groups

cramersV(table(shootings_data$gender, shootings_data$manner_of_death))
table(shootings_data$gender, shootings_data$manner_of_death)
chisq.test(table(shootings_data$gender, shootings_data$manner_of_death), correct = F) #very weak correlation and enough observations, can try chi-squared test

cramersV(table(shootings_data$race, shootings_data$manner_of_death))
table(shootings_data$race, shootings_data$manner_of_death)
fisher.test(table(shootings_data$race, shootings_data$manner_of_death))

cramersV(table(shootings_data$city, shootings_data$manner_of_death))
table(shootings_data$city, shootings_data$manner_of_death)
fisher.test(table(shootings_data$city, shootings_data$manner_of_death), simulate.p.value = T)

cramersV(table(shootings_data$state, shootings_data$manner_of_death))
table(shootings_data$state, shootings_data$manner_of_death)
fisher.test(table(shootings_data$state, shootings_data$manner_of_death), simulate.p.value = T)

cramersV(table(shootings_data$signs_of_mental_illness, shootings_data$manner_of_death))
table(shootings_data$signs_of_mental_illness, shootings_data$manner_of_death)
chisq.test(table(shootings_data$signs_of_mental_illness, shootings_data$manner_of_death), correct = F)

cramersV(table(shootings_data$threat_level, shootings_data$manner_of_death))
table(shootings_data$threat_level, shootings_data$manner_of_death)
chisq.test(table(shootings_data$threat_level, shootings_data$manner_of_death), correct = F)

cramersV(table(shootings_data$flee, shootings_data$manner_of_death))
table(shootings_data$flee, shootings_data$manner_of_death)
chisq.test(table(shootings_data$flee, shootings_data$manner_of_death), correct = F)

cramersV(table(shootings_data$body_camera, shootings_data$manner_of_death))
table(shootings_data$body_camera, shootings_data$manner_of_death)
chisq.test(table(shootings_data$body_camera, shootings_data$manner_of_death), correct = F)

cramersV(table(shootings_data$arms_category, shootings_data$manner_of_death))
table(shootings_data$arms_category, shootings_data$manner_of_death)
fisher.test(table(shootings_data$arms_category, shootings_data$manner_of_death), simulate.p.value = T)

#Testing continuous variable

ggplot(data = shootings_data, aes(x = manner_of_death, y = age, fill = manner_of_death))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Does the age affect on the manner of death?", x = "", y = "Age")

ggplot(data = shootings_data, aes(x = age, fill = "red"))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~manner_of_death)+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Age distribution by the manner of death", x = "Age", y = "Total cases")

ggplot(data = shootings_data)+
  geom_qq(aes(sample = age))+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Age QQ-plot", x = "Theoretical quantilles", y = "Age")

ggplot(data = shootings_data)+
  geom_qq(aes(sample = age))+
  facet_wrap(~manner_of_death)+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(title = "Age QQ-plot divided by the manner of death", x = "Theoretical quantilles", y = "Age")

nortest::ad.test(shootings_data$age) #not normal distribution in general population
nortest::ad.test(shootings_data[shootings_data$manner_of_death=="shot",]$age) #not normal distribution in a sample
nortest::ad.test(shootings_data[shootings_data$manner_of_death=="shot and Tasered",]$age) #not normal distribution in a sample
car::leveneTest(data = shootings_data, age ~ manner_of_death) #homogeneity of variance in groups - ok

#trying Mann-Whitney test and bootstrapping for testing Age variable

wilcox.test(data = shootings_data, age ~ manner_of_death, paired = F) #no statistically significant difference between groups
bootstrap_ttest <- MKinfer::boot.t.test(shootings_data[shootings_data$manner_of_death=='shot', 'age'], 
                     shootings_data[shootings_data$manner_of_death=='shot and Tasered', 'age'], reps=1000)
bootstrap_ttest #no statistically significant difference between groups
