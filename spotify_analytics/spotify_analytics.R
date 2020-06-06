library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(lmtest)
library(MASS)
library(wordcloud)

spotify <- read.csv("top10s.csv", stringsAsFactors = T, na.strings = "Inf")
head(spotify)
str(spotify)
spotify$title <- gsub("й","e", spotify$title)

spotify_dp <- as_tibble(spotify)
spotify_dp <- spotify_dp %>% 
  rename("song_id" = X, "top_genre" = top.genre) %>% 
  mutate_at(c("song_id", "title"), as.factor)

nrow(spotify_dp)
nrow(spotify_dp[complete.cases(spotify_dp),])

sapply(spotify_dp[6:15], function(col) summary(col))

spotcor <- cor(spotify_dp[6:14])
spotcor <- round(spotcor, 2)
corrplot(spotcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45)

spotcoradv <- spotify_dp %>%
  select(spch, dur, val, live, dB, dnce, nrgy, bpm) 
ggpairs(spotcoradv)

spot_selection <- function(col){
  spotify_dp %>% 
    select(year, col) %>% 
    mutate_at("year", as.factor) %>% 
    mutate("index" = as.factor(col)) %>% 
    rename("value" = col)
}

spotify_dp_bpm <- spot_selection("bpm")
spotify_dp_nrgy <- spot_selection("nrgy")
spotify_dp_dnce <- spot_selection("dnce")
spotify_dp_dB <- spot_selection("dB")
spotify_dp_live <- spot_selection("live")
spotify_dp_val <- spot_selection("val")
spotify_dp_dur <- spot_selection("dur")
spotify_dp_acous <- spot_selection("acous")
spotify_dp_spch <- spot_selection("spch")


spotify_dp_trend <- rbind(spotify_dp_bpm, spotify_dp_nrgy, spotify_dp_dnce, 
                          spotify_dp_dB, spotify_dp_live, spotify_dp_val,
                          spotify_dp_dur, spotify_dp_acous, spotify_dp_spch)

spotify_dp_trend <- spotify_dp_trend %>% 
  group_by(year, index) %>% 
  mutate("median_value" = median(value))

ggplot(spotify_dp_trend, aes(x = year, y = median_value, group = index, color = index))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  facet_wrap(~ index, scales = "free")+
  theme(axis.text.x = element_text(size = 7, angle = 30),legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(y = "median value",
       title = "10-years trends in Spotify best songs")

spotify_dp_genres <- spotify_dp %>% 
  select(top_genre, year, pop) %>% 
  mutate_at("year", as.factor)

ggplot(spotify_dp_genres, aes(x = year, y = reorder(top_genre,pop), fill = pop))+
  geom_tile()+
  scale_fill_distiller(palette = "Spectral")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = rel(0.9)), axis.text.y = element_text(size = rel(0.9)))+
  labs(y = "genres",
       title = "Genres popularity dynamics")

spotify_dp_artists <- spotify_dp %>% 
  select(artist) %>% 
  group_by(artist) %>% 
  summarise(number = n()) %>% 
  arrange(desc(number)) %>% 
  top_n(15, number)
  
ggplot(spotify_dp_artists, aes(x = reorder(artist, -number), y = number, fill = number))+
  geom_col()+
  theme_minimal()+
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_text(aes(label = number), size = 4, vjust = -2)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = rel(0.9)))+
  labs(x = "artist",
       title = "Top-15 most popular artists")

spotify_dp_title <- spotify_dp %>% 
  select(song_id, title, year, pop) %>% 
  mutate_at("year", as.factor) %>% 
  group_by(year) %>%
  top_n(1, pop) %>% 
  filter(as.integer(song_id) != 270 & as.integer(song_id) != 271)

ggplot(spotify_dp_title, aes(x = year, y = pop, fill = pop))+
  geom_col()+
  theme_minimal()+
  scale_fill_gradient(low = "yellow", high = "red")+
  geom_text(aes(label = title), size = 3, vjust = -1)+
  geom_text(aes(label = pop), size = 4, vjust = 2)+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  labs(y = "song",
       title = "Each year most popular song")

##########################################################################
spotify_dp_num <- spotify_dp[6:15]
fit <- lm(pop ~ ., spotify_dp_num)
summary(fit)
hist(fit$residuals)
shapiro.test(fit$residuals)
plot(fit)

bptest(fit)

fit2 <- lm(pop ~ bpm + nrgy + dnce + dB + live + val + dur + acous + spch, spotify_dp_num)
summary(fit2)
hist(fit2$residuals)
shapiro.test(fit2$residuals)

qplot(x = bpm, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test1 <- lm(log(pop+0.01) ~ bpm, spotify_dp_num)
summary(fit_test1)

qplot(x = nrgy^2, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test2 <- lm(log(pop+0.01) ~ nrgy^2, spotify_dp_num)
summary(fit_test2)
shapiro.test(fit_test2$residuals)
hist(fit_test2$residuals)
hist(spotify_dp_num$pop^2)
hist(log(spotify_dp_num$nrgy))

qplot(x = dnce, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test3 <- lm(log(pop+0.01) ~ dnce, spotify_dp_num)
summary(fit_test3)
shapiro.test(fit_test3$residuals)
hist(fit_test3$residuals)

qplot(x = dB, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test4 <- lm(log(pop+0.01) ~ dB, spotify_dp_num)
summary(fit_test4)
shapiro.test(fit_test4$residuals)
hist(fit_test4$residuals)

qplot(x = live, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test5 <- lm(log(pop+0.01) ~ live, spotify_dp_num)
summary(fit_test5)
shapiro.test(fit_test5$residuals)
hist(fit_test5$residuals)

qplot(x = val, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test6 <- lm(log(pop+0.01) ~ val, spotify_dp_num)
summary(fit_test6)
shapiro.test(fit_test6$residuals)
hist(fit_test6$residuals)

qplot(x = dur, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test7 <- lm(log(pop+0.01) ~ dur, spotify_dp_num)
summary(fit_test7)
shapiro.test(fit_test7$residuals)
hist(fit_test7$residuals)

qplot(x = acous^0.5, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test8 <- lm(log(pop+0.01) ~ sqrt(acous), spotify_dp_num)
summary(fit_test8)
shapiro.test(fit_test8$residuals)
hist(fit_test8$residuals)

qplot(x = log(spch+1), y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

fit_test9 <- lm(log(pop+0.01) ~ log(spch+1), spotify_dp_num)
summary(fit_test9)
shapiro.test(fit_test9$residuals)
hist(fit_test9$residuals)

fit3 <- lm(log(pop+0.01) ~ bpm + nrgy + dnce + dB + live + val + dur + sqrt(acous) + log(spch+1), spotify_dp_num)
summary(fit3)
hist(fit3$residuals)
shapiro.test(fit3$residuals)

qplot(x = fit3$residuals, y = log(pop+0.01), data = spotify_dp_num)+
  geom_smooth(method = "lm")

spotify_dp_num$predicted <- exp(predict(fit3))


spotify_num <- spotify[6:15]

spotify_num$bpm <- as.numeric(gsub("\\.", "", spotify_num$bpm))
spotify_num$pop <- as.numeric(gsub("\\.", "", spotify_num$pop))
head(spotify_num)
fit_test <- lm(log(pop+1) ~ bpm, spotify_num1, na.action = na.exclude)
summary(fit_test)
shapiro.test(fit_test$residuals)
