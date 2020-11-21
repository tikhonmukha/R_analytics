library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(maps)
library(mapproj)

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

ggplot(data = shootings_data, aes(x = age))+
  geom_histogram(binwidth = 1, aes(fill = "red"))+
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
