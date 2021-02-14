library(dplyr)
library(magrittr)
library(ggplot2)
library(caret)
library(nortest)
library(pscl) #for zero-inflated glm
library(mgcv) #for gam

# Uploading data

spotify_df <- read.csv('data.csv', encoding = 'UTF-8', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  mutate(across(c('explicit','key','mode'), as.factor)) %>% 
  mutate(song_age = 2021 - year) %>% 
  select(-c('id', 'release_date')) %>% 
  filter(valence != 0 & popularity != 0 & acousticness != 0 & danceability != 0 & liveness != 0 & speechiness != 0 & tempo != 0)

head(spotify_df)
tail(spotify_df)
glimpse(spotify_df)
summary(spotify_df)

# Exploratory data analysis

## Correlation between numeric variables

mcor <- cor(spotify_df %>% select_if(., is.numeric), method = 'spearman') %>% round(., digits = 3)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot::corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
                   tl.cex = 0.75, number.cex = 0.75, col = col(200), addCoef.col = "black",
                   order = "AOE", title = "Spearman correlation matrix")

spotify_num <- spotify_df %>% select_if(., is.numeric) %>% select(-c('year')) %>% as.data.frame()

names(spotify_num)[findCorrelation(cor(spotify_num), cutoff = 0.75)] #looking for variables with high average correlation level

names(spotify_num)[findLinearCombos(spotify_num)$remove] #looking for variables linear combinations

## checking for multicollinearity

top.mat <- function(X, level = 0.5, N = 10, values = TRUE){
  X.nam <- row.names(X)
  X.tri <- as.vector(lower.tri(X))
  X.rep.g <- rep(X.nam, length(X.nam))[X.tri]
  X.rep.e <- rep(X.nam, each = length(X.nam))[X.tri]
  X.vec <- as.vector(X)[X.tri]
  X.df <- data.frame(Var1 = X.rep.g, Var2 = X.rep.e, Value = X.vec)
  {if (values){
    X.df <- X.df[abs(X.df$Value) >= level, ]
    X.df <- X.df[order(-abs(X.df$Value)), ]
  }else {
    X.df <- X.df[order(-abs(X.df$Value)), ]
    X.df <- X.df[1:N, ]
  }
  }
  row.names(X.df) <- seq(1, along = X.df$Value)
  return(X.df)
}

top.mat(cor(spotify_num))

for (i in 1:length(spotify_num)){
  print(names(which(car::vif(lm(spotify_num[,i]~., data = spotify_num[,-i]))>5)))
}

for (i in 1:length(spotify_num)){
  print(names(which(car::vif(lm(spotify_num[,i]~., data = spotify_num[,-i]))>2)))
}

## Testing linearity of numeric variables 

ggplot(data = spotify_df, aes(x = valence, y = popularity))+
  geom_point()+
  geom_smooth()+
  theme_minimal() #подобрать степень сглаживания

ggplot(data = spotify_df, aes(x = acousticness, y = popularity))+
  geom_point()+
  geom_smooth()+
  theme_minimal() #не идет в модель

ggplot(data = spotify_df, aes(x = danceability, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "bs", k = 5))+
  theme_minimal()

ggplot(data = spotify_df, aes(x = duration_ms, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 8))+
  theme_minimal()

ggplot(data = spotify_df, aes(x = energy, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 3))+
  theme_minimal()

ggplot(data = spotify_df, aes(x = instrumentalness, y = popularity))+
  geom_point()+
  geom_smooth()+
  theme_minimal() #не идет в модель

ggplot(data = spotify_df, aes(x = liveness, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 3))+
  theme_minimal()

ggplot(data = spotify_df, aes(x = loudness, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "bs", k = 5))+
  theme_minimal() #подобрать степень сглаживания

ggplot(data = spotify_df, aes(x = speechiness, y = popularity))+
  geom_point()+
  geom_smooth()+
  theme_minimal() #подобрать степень сглаживания

ggplot(data = spotify_df, aes(x = tempo, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 3))+
  theme_minimal() #подобрать степень сглаживания

ggplot(data = spotify_df, aes(x = song_age, y = popularity))+
  geom_point()+
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 11))+
  theme_minimal()

## Analysis of numeric variables distribution

ggplot(data = spotify_df, aes(x = valence, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Valence distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = acousticness, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Acousticness distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = danceability, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Danceability distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = duration_ms, fill = 'red'))+
  geom_histogram(binwidth = 50000, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Duration distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = energy, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Energy distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = instrumentalness, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Instrumentalness distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = liveness, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Liveness distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = loudness, fill = 'red'))+
  geom_histogram(binwidth = 1, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Loudness distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = speechiness, fill = 'red'))+
  geom_histogram(binwidth = 0.025, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Speechiness distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = tempo, fill = 'red'))+
  geom_histogram(binwidth = 5, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Tempo distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = song_age, fill = 'red'))+
  geom_histogram(binwidth = 5, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Song age distribution', y = 'Frequency')

ggplot(data = spotify_df, aes(x = popularity, fill = 'red'))+
  geom_histogram(binwidth = 5, position = 'identity', colour = 'black')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Popularity distribution', y = 'Frequency')

### Testing numeric variables distribution normality

x <- apply(spotify_num,2,ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Finding outliers in numeric variables

ggplot(data = spotify_df, aes(y = valence, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Valence boxplot')

ggplot(data = spotify_df, aes(y = acousticness, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Acousticness boxplot')

ggplot(data = spotify_df, aes(y = danceability, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Danceability boxplot')

ggplot(data = spotify_df, aes(y = duration_ms, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Duration boxplot')

ggplot(data = spotify_df, aes(y = energy, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Energy boxplot')

ggplot(data = spotify_df, aes(y = instrumentalness, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Instrumentalness boxplot')

ggplot(data = spotify_df, aes(y = liveness, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Liveness boxplot')

ggplot(data = spotify_df, aes(y = loudness, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Loudness boxplot')

ggplot(data = spotify_df, aes(y = popularity, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Popularity boxplot')

ggplot(data = spotify_df, aes(y = speechiness, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Speechiness boxplot')

ggplot(data = spotify_df, aes(y = tempo, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Tempo boxplot')

ggplot(data = spotify_df, aes(y = song_age, fill = 'red'))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Song age boxplot')

### Testing numeric variables for outliers

y <- apply(spotify_num,2,function(col) outliers::grubbs.test(col, type=10))

for (i in 1:length(y)){
  print(y[[i]]$p.value)
} #duration_ms, loudness and speechiness have outliers

Q_dur <- quantile(spotify_df$duration_ms, probs = c(0.25, 0.75))
iqr_dur <- IQR(spotify_df$duration_ms)
up_border_dur <- Q_dur[2]+1.5*iqr_dur
spotify_df %<>% filter(duration_ms < up_border_dur)

duration_trans <- preProcess(spotify_df[, 'duration_ms'], method = c('scale', 'center'), verbose = FALSE)
duration_var <- predict(duration_trans, spotify_df[, 'duration_ms'])
spotify_df[, 'duration_ms'] <- duration_var
summary(spotify_df$duration_ms)

Q_loud <- quantile(spotify_df$loudness, probs = c(0.25, 0.75))
iqr_loud <- IQR(spotify_df$loudness)
low_border_loud <- Q_loud[1]-1.5*iqr_loud
spotify_df %<>% filter(loudness > low_border_loud)

## Checking numeric variables variance level

spotify_num <- spotify_df %>% select_if(., is.numeric) %>% select(-c('year')) %>% as.data.frame()
spotify_num %>% apply(., 2, var) %>% as.data.frame() %>% arrange(desc(.))

for(i in 1:length(spotify_num)){
  print(c(colnames(spotify_num)[i], length(unique(spotify_num[,i]))/nrow(spotify_num)*100,
          as.vector(sort(table(spotify_num[,i]), decreasing = TRUE)[1]/sort(table(spotify_num[,i]), decreasing = TRUE)[2])))
}

colnames(spotify_num)[nearZeroVar(spotify_num)]

## Factor variables analysis

ggplot(data = spotify_df %>% 
         select(explicit, popularity) %>% 
         group_by(popularity, explicit) %>% 
         count(), aes(x = popularity, y = n, fill = explicit))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Song popularity rate', y = 'Share')

ggplot(data = spotify_df %>% 
         select(mode, popularity) %>% 
         group_by(popularity, mode) %>% 
         count(), aes(x = popularity, y = n, fill = mode))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Song mode', y = 'Share')

ggplot(data = spotify_df %>% 
         select(key, popularity) %>% 
         group_by(popularity, key) %>% 
         count(), aes(x = popularity, y = n, fill = key))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Song key', y = 'Share')

## Testing factor variables for significant influence on popularity

ad.test(spotify_df[spotify_df$explicit=="1",]$popularity)
ad.test(spotify_df[spotify_df$explicit=="0",]$popularity)
car::leveneTest(popularity~explicit, data = spotify_df)
wilcox.test(popularity~explicit, data = spotify_df)

ad.test(spotify_df[spotify_df$mode=="1",]$popularity)$p.value
ad.test(spotify_df[spotify_df$mode=="0",]$popularity)
car::leveneTest(popularity~mode, data = spotify_df)
wilcox.test(popularity~mode, data = spotify_df)

for(i in 1:nlevels(spotify_df$key)){
  print(ad.test(spotify_df[spotify_df$key==(i-1),]$popularity)$p.value)
}
car::leveneTest(popularity~key, data = spotify_df)
kruskal.test(popularity~key, data = spotify_df) 

## Checking factor variables variance level

spotify_fct <- spotify_df %>% select_if(., is.factor) %>% select(-c('artists', 'name')) %>% as.data.frame()

spotify_fct %>% apply(., 2, var) %>% as.data.frame() %>% arrange(desc(.))

for(i in 1:length(spotify_fct)){
  print(c(colnames(spotify_fct)[i], length(unique(spotify_fct[,i]))/nrow(spotify_fct)*100,
          as.vector(sort(table(spotify_fct[,i]), decreasing = TRUE)[1]/sort(table(spotify_fct[,i]), decreasing = TRUE)[2])))
}

colnames(spotify_fct)[nearZeroVar(spotify_fct)]

# Splitting data into train and test

in_train <- createDataPartition(spotify_df$popularity, p = 0.8, list = FALSE)
spotify_train <- spotify_df[in_train, ]
spotify_test <- spotify_df[-in_train, ]

# Modeling

## First create a simple linear model to analyze residuals

vars <- spotify_train %>% 
  select_if(., is.numeric) %>% 
  select(-c('popularity', 'acousticness', 'instrumentalness', 'year')) %>% 
  colnames()

varformula <- as.formula(paste('popularity',
                               paste(vars, collapse = ' + '), sep = ' ~ '))

model_lm <- gam(varformula, data = spotify_train)
summary(model_lm)

hist(model_lm$residuals)
hist(model_lm$fitted.values)

par(mfrow = c(2,2))
gam.check(model_lm)

## Now create a basic general linear null-model

model_glm_null <- glm(popularity ~ 1, family = 'poisson', data = spotify_train)
summary(model_glm_null)

## Basic general linear model

model_glm <- glm(varformula, family = 'poisson', data = spotify_train)
summary(model_glm)
anova(model_glm_null, model_glm, test = 'Chisq')
anova(model_glm)
plot(model_glm)

## General linear model with explicit

model_glm_fct <- glm(popularity ~ explicit*(valence + danceability + duration_ms + 
                       energy + liveness + loudness + speechiness + 
                       tempo + song_age), family = 'poisson', data = spotify_train)
summary(model_glm_fct)
plot(model_glm_fct)
anova(model_glm_null, model_glm, model_glm_fct, test = 'Chisq')
c(AIC(model_glm_null),AIC(model_glm),AIC(model_glm_fct))

## General linear model with mode

model_glm_fct_mode <- glm(popularity ~ mode*explicit*(valence + danceability + duration_ms + 
                                                        energy + liveness + loudness + speechiness + 
                                                        tempo + song_age), family = 'poisson', data = spotify_train)

summary(model_glm_fct_mode)
summary(model_glm_fct_mode$fitted.values)
plot(model_glm_fct_mode)
anova(model_glm_null, model_glm, model_glm_fct, model_glm_fct_mode, test = 'Chisq')
c(AIC(model_glm_null),AIC(model_glm),AIC(model_glm_fct), AIC(model_glm_fct_mode))

## Basic general addictive model

model_gam <- gam(popularity ~ s(song_age, bs = "cs", k = 11) + s(duration_ms, bs = "cs", k = 8) +
                   s(danceability, bs = "bs", k = 5) + s(energy, bs = "cs", k = 3) + s(liveness, bs = "cs", k = 3) +
                   s(tempo, bs = "cs", k = 3) + s(valence) + s(loudness) + s(speechiness), data = spotify_train)
oop <- summary(model_gam)
oop$deviance
gam.check(model_gam)
RMSE(model_lm$fitted.values, spotify_train$popularity)
anova(model_lm, model_gam, test = 'Chisq')

## Choosing the best model

data.frame(model = c('model_lm', 'model_glm', 'model_glm_fct', 'model_glm_fct_mode', 'model_gam'),
           aic = c(AIC(model_lm), AIC(model_glm), AIC(model_glm_fct), AIC(model_glm_fct_mode), AIC(model_gam)),
           rmse = c(RMSE(model_lm$fitted.values, spotify_train$popularity), RMSE(model_glm$fitted.values, spotify_train$popularity),
                    RMSE(model_glm_fct$fitted.values, spotify_train$popularity), RMSE(model_glm_fct_mode$fitted.values, spotify_train$popularity),
                    RMSE(model_gam$fitted.values, spotify_train$popularity)))

# Using GAM for prediction

spotify_test$pred_pop <- predict.gam(object = model_gam, newdata = spotify_test)
RMSE(spotify_test$pred_pop, spotify_test$popularity)

ggplot(data = spotify_test, aes(x = pred_pop, y = popularity))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method = 'lm')+
  labs(x = 'Predicted values', y = 'Popularity')
