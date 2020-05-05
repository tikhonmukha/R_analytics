library(dplyr)
library(tidyr)
library(ggplot2)

netflix <- read.csv("netflix_titles.csv", stringsAsFactors = T,encoding = "UTF-8")
head(netflix)
str(netflix)
netflix$show_id <- as.factor(netflix$show_id)
netflix$release_year <- as.factor(netflix$release_year)
netflix <- netflix[,-12]
netflix_dp <- as_tibble(netflix)

netflix_dp$date_added <- as.character(netflix_dp$date_added)
datadd <- as.matrix(netflix_dp$date_added)
datadd <- trimws(datadd, which = c("left"))
datadd <- as.data.frame(datadd)
colnames(datadd) <- "date_added"
datadd <- separate(datadd, date_added, c("dm_added", "year_added"), sep = ", ")
datadd <- separate(datadd, dm_added, c("month_added", "day_added"), sep = " ")
netflix_dp <- cbind(netflix_dp, datadd)
netflix_dp <- as_tibble(netflix_dp)
netflix_dp <- netflix_dp[,c(1,2,3,4,5,6,7,12,13,14,8,9,10,11)]
netflix_dp$month_added <- as.factor(netflix_dp$month_added)
netflix_dp$day_added <- as.factor(netflix_dp$day_added)
netflix_dp$year_added <- as.factor(netflix_dp$year_added)
netflix_dp$rating <- as.character(netflix_dp$rating)
netflix_dp$rating <- gsub("TV-G","G", netflix_dp$rating)
netflix_dp$rating <- gsub("TV-PG","PG", netflix_dp$rating)
netflix_dp$rating <- gsub("TV-14","PG-13", netflix_dp$rating)
netflix_dp$rating <- gsub("TV-MA","R", netflix_dp$rating)
netflix_dp$rating <- as.factor(netflix_dp$rating)


netflix_dp_type <- netflix_dp %>%
  filter(as.character(year_added) != "2020") %>% 
  group_by(year_added, type) %>% 
  summarise(num = n())

ggplot(netflix_dp_type, aes(x = year_added, y = num, group = type, color = type))+
  geom_line()+
  geom_point()+
  xlab("year added in Netflix")+
  ylab("number")+
  ggtitle("The dynamics of TV-shows and films releases")

netflix_dp_rating_film <- netflix_dp %>%
  filter(as.character(year_added) != "2020") %>%
  filter(as.character(type) == "Movie") %>%
  select(c("type", "year_added", "rating")) %>% 
  group_by(type, year_added, rating) %>% 
  summarise(num = n())

netflix_dp_rating_tvshow <- netflix_dp %>%
  filter(as.character(year_added) != "2020") %>%
  filter(as.character(type) == "TV Show") %>%
  select(c("type", "year_added", "rating")) %>% 
  group_by(type, year_added, rating) %>% 
  summarise(num = n())

netflix_dp_rating <- union(netflix_dp_rating_film, netflix_dp_rating_tvshow)

ggplot(netflix_dp_rating, aes(x = year_added, y = num, group = rating, color = rating))+
  geom_line()+
  geom_point()+
  facet_wrap( ~ type)+
  xlab("year added in Netflix")+
  ylab("Number")+
  ggtitle("The dynamics of movies and TV-shows ratings")

netflix_dp_genre <- netflix_dp %>%
  select(c("show_id","type","listed_in")) %>% 
  separate_rows(listed_in, sep = ",")

netflix_dp_genre$listed_in <- trimws(netflix_dp_genre$listed_in)
netflix_dp_genre$listed_in <- as.factor(netflix_dp_genre$listed_in)

netflix_dp_genre <- netflix_dp_genre %>% 
  group_by(type, listed_in) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num))

ggplot(netflix_dp_genre, aes(x = reorder(listed_in, -num), y = num))+
  geom_col()+
  facet_wrap(~ type, scales = "free_x")+
  xlab("category")+
  ylab("total amount")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.85)))

netflix_dp_date <- netflix_dp %>% 
  group_by(year_added, type) %>%
  filter(as.character(year_added) != "2020") %>% 
  summarise(add_today = n()) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  mutate(total_num = cumsum(add_today))

ggplot(netflix_dp_date, aes(x = year_added, y = total_num, group = type, color = type))+
  geom_line(size = 1.25, na.rm = T)+
  geom_point(size = 2, na.rm = T)+
  scale_x_discrete(name = "Year added",breaks = c("2009","2011","2013","2015","2017","2019"))+
  scale_y_continuous(name = "Cumulative amount")+
  ggtitle("Increase dynamics of movies and TV-shows")+
  theme(legend.position = "top")

netflix_dp_country_film <- netflix_dp %>%
  filter(as.character(type) == "Movie") %>% 
  group_by(country, type) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num)) %>%
  filter(as.character(country) != "") %>%
  head(10)

netflix_dp_country_tvshow <- netflix_dp %>%
  filter(as.character(type) == "TV Show") %>% 
  group_by(country, type) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num)) %>%
  filter(as.character(country) != "") %>%
  head(10)

netflix_dp_country <- union(netflix_dp_country_film, netflix_dp_country_tvshow)

netflix_dp_country$country <- reorder(netflix_dp_country$country, netflix_dp_country$num, fun = max)

ggplot(netflix_dp_country, aes(x = reorder(country, -num), y = num))+
  geom_col()+
  facet_wrap( ~ type, scales = "free_x")+
  xlab("country")+
  ylab("total amount")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.85)))


netflix_dp_persons_movie <- netflix_dp %>% 
  select(show_id, type, cast, director) %>%
  filter(as.character(type) == "Movie") %>% 
  mutate_at(c("cast", "director"), as.character) %>% 
  separate_rows(cast, sep = ",") %>% 
  separate_rows(director, sep = ",")

netflix_dp_persons_movie$cast <- trimws(netflix_dp_persons_movie$cast)
netflix_dp_persons_movie$director <- trimws(netflix_dp_persons_movie$director)
netflix_dp_persons_movie$cast <- as.factor(netflix_dp_persons_movie$cast)
netflix_dp_persons_movie$director <- as.factor(netflix_dp_persons_movie$director)

netflix_dp_persons_movie_cor <- netflix_dp_persons_movie %>%
  filter(as.character(director) != "") %>% 
  filter(as.character(cast) != "") %>% 
  group_by(type, cast, director) %>% 
  summarise(matches = n()) %>% 
  arrange(desc(matches)) %>% 
  head(25)

netflix_dp_persons_tvshow <- netflix_dp %>% 
  select(show_id, type, cast, director) %>%
  filter(as.character(type) == "TV Show") %>% 
  mutate_at(c("cast", "director"), as.character) %>% 
  separate_rows(cast, sep = ",") %>% 
  separate_rows(director, sep = ",")

netflix_dp_persons_tvshow$cast <- trimws(netflix_dp_persons_tvshow$cast)
netflix_dp_persons_tvshow$director <- trimws(netflix_dp_persons_tvshow$director)
netflix_dp_persons_tvshow$cast <- as.factor(netflix_dp_persons_tvshow$cast)
netflix_dp_persons_tvshow$director <- as.factor(netflix_dp_persons_tvshow$director)

netflix_dp_persons_tvshow_cor <- netflix_dp_persons_tvshow %>%
  filter(as.character(director) != "") %>% 
  filter(as.character(cast) != "") %>% 
  group_by(type, cast, director) %>% 
  summarise(matches = n()) %>% 
  arrange(desc(matches)) %>% 
  head(25)

netflix_dp_persons_cor <- union(netflix_dp_persons_movie_cor, netflix_dp_persons_tvshow_cor)
netflix_dp_persons_cor$cast <- as.factor(netflix_dp_persons_cor$cast)
netflix_dp_persons_cor$director <- as.factor(netflix_dp_persons_cor$director)

ggplot(netflix_dp_persons_cor, aes(x = reorder(director, matches), y = reorder(cast, matches), fill = matches))+
  geom_tile()+
  scale_fill_distiller(palette = "Spectral")+
  facet_wrap( ~ type, scales = "free", nrow = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.85)), axis.text.y = element_text(size = rel(0.85)))+
  xlab("director")+
  ylab("actor")+
  ggtitle("Pairs most frequently working together")


netflix_dp_country_genre_movie <- netflix_dp %>% 
  filter(as.character(type) == "Movie") %>% 
  select(c("show_id", "type","country", "listed_in")) %>% 
  mutate_at(c("country", "listed_in"), as.character) %>% 
  separate_rows(country, sep = ",") %>% 
  separate_rows(listed_in, sep = ",") %>% 
  mutate_at(c("country", "listed_in"), trimws) %>% 
  mutate_at(c("country", "listed_in"), as.factor)

netflix_dp_country_genre_movie_cor <- netflix_dp_country_genre_movie %>%
  filter(as.character(country) != "") %>% 
  filter(as.character(listed_in) != "") %>% 
  group_by(type, country, listed_in) %>% 
  summarise(matches = n()) %>% 
  arrange(desc(matches)) %>% 
  head(50)

netflix_dp_country_genre_tvshow <- netflix_dp %>% 
  filter(as.character(type) == "TV Show") %>% 
  select(c("show_id", "type","country", "listed_in")) %>% 
  mutate_at(c("country", "listed_in"), as.character) %>% 
  separate_rows(country, sep = ",") %>% 
  separate_rows(listed_in, sep = ",") %>% 
  mutate_at(c("country", "listed_in"), trimws) %>% 
  mutate_at(c("country", "listed_in"), as.factor)

netflix_dp_country_genre_tvshow_cor <- netflix_dp_country_genre_tvshow %>%
  filter(as.character(country) != "") %>% 
  filter(as.character(listed_in) != "") %>% 
  group_by(type, country, listed_in) %>% 
  summarise(matches = n()) %>%
  arrange(desc(matches)) %>% 
  head(50)

netflix_dp_country_genre <- union(netflix_dp_country_genre_movie_cor, netflix_dp_country_genre_tvshow_cor)
netflix_dp_country_genre$country <- as.factor(netflix_dp_country_genre$country)
netflix_dp_country_genre$listed_in <- as.factor(netflix_dp_country_genre$listed_in)

ggplot(netflix_dp_country_genre, aes(x = reorder(listed_in, matches), y = reorder(country, matches), fill = matches))+
  geom_tile()+
  scale_fill_distiller(palette = "Spectral")+
  facet_wrap( ~ type, scales = "free", nrow = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.85)), axis.text.y = element_text(size = rel(0.85)))+
  xlab("genre")+
  ylab("country")+
  ggtitle("Countries and genres they frequently working in")
