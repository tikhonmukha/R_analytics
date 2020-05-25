library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(lmtest)
library(MASS)

spotify <- read.csv("top10s.csv", stringsAsFactors = T, na.strings = "Inf")
head(spotify)
str(spotify)
spotify$title <- gsub("é","e", spotify$title)

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

spotify_dp_bpm <- spotify_dp %>% 
  select(year, bpm) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("bpm")) %>% 
  rename("value" = bpm)

spotify_dp_nrgy <- spotify_dp %>% 
  select(year, nrgy) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("nrgy")) %>% 
  rename("value" = nrgy)

spotify_dp_dnce <- spotify_dp %>% 
  select(year, dnce) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("dnce")) %>% 
  rename("value" = dnce)

spotify_dp_dB <- spotify_dp %>% 
  select(year, dB) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("dB")) %>% 
  rename("value" = dB)

spotify_dp_live <- spotify_dp %>% 
  select(year, live) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("live")) %>% 
  rename("value" = live)

spotify_dp_val <- spotify_dp %>% 
  select(year, val) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("val")) %>% 
  rename("value" = val)

spotify_dp_dur <- spotify_dp %>% 
  select(year, dur) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("dur")) %>% 
  rename("value" = dur)

spotify_dp_acous <- spotify_dp %>% 
  select(year, acous) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("acous")) %>% 
  rename("value" = acous)

spotify_dp_spch <- spotify_dp %>% 
  select(year, spch) %>% 
  mutate_at("year", as.factor) %>% 
  mutate("index" = as.factor("spch")) %>% 
  rename("value" = spch)

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

spotify_num <- spotify[6:15]

spotify_num$bpm <- as.numeric(gsub("\\.", "", spotify_num$bpm))
spotify_num$pop <- as.numeric(gsub("\\.", "", spotify_num$pop))
head(spotify_num)
fit_test <- lm(log(pop+1) ~ bpm, spotify_num1, na.action = na.exclude)
summary(fit_test)
shapiro.test(fit_test$residuals)
