library(tidyverse)
library(magrittr)
library(DMwR)
library(caret)
library(rpart)
library(party)
library(randomForest)

# Importing data

price_data <- read.csv('housing.csv', na.strings = '', stringsAsFactors = TRUE) %>% 
  tibble()

summary(price_data)

apply(price_data, 2, anyNA)
price_data[complete.cases(price_data), ]

# Exploratory data analysis

## Correlation between continuous variables

Mcor <- cor(price_data %>% select_if(is.numeric) %>% na.omit(.))
corrplot::corrplot(Mcor, method = 'color', addCoef.col = 'green', addgrid.col = 'gray33',
                   tl.col = 'black')

GGally::ggpairs(price_data %>% select_if(is.numeric) %>% na.omit(.))

## Filling continuous variables NA-values 

price_num <- price_data %>% select_if(is.numeric) %>% as.data.frame(.)
ind <- apply(price_num, 1, function(x) sum(is.na(x))) > 0
price_num[ind, ]
pPmI <- preProcess(price_num, method = 'medianImpute')
price_num <- predict(pPmI, newdata = price_num)
Imp.Med <- price_num[ind, ]

price_num <- price_data %>% select_if(is.numeric) %>% as.data.frame(.)
pPbI <- preProcess(price_num, method = 'bagImpute')
price_num <- predict(pPbI, newdata = price_num)
Imp.Bag <- price_num[ind, ]

price_num <- price_data %>% select_if(is.numeric) %>% as.data.frame(.)
pPkI <- preProcess(price_num, method = 'knnImpute')
alg.stand <- predict(pPkI, newdata = price_num)
m <- pPkI$mean
sd <- pPkI$std
price_num <- t(apply(alg.stand, 1, function(r) m + r * sd))
Imp.Knn <- price_num[ind, ]

ImpVal <- rbind(Imp.Med, Imp.Bag, Imp.Knn)
Imp.Method <- as.factor(c(rep('Med', 207), rep('Bag', 207), rep('Knn', 207)))
library(RANN)
library(vegan)
Imp.M <- rda(ImpVal ~ Imp.Method, ImpVal)
plot(Imp.M, display = 'sites', type = 'p')
ordihull(Imp.M, Imp.Method, draw = 'polygon', alpha = 67, 
         lty = 2, col = c(1,2,3), label = TRUE)

price_data[ind, colnames(price_num)] <- Imp.Bag

## Checking multicollinearity of continuous variables

top.mat <- function(X, level, N, values = TRUE){
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

top.mat(cor(price_num), level = 0.75, N = ncol(price_num))

highcor <- colnames(price_num[, findCorrelation(cor(price_num), cutoff = 0.75)])

findLinearCombos(price_num)

## Checking distribution of continuous variables

ggplot(data = price_data, aes(x = longitude))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'longitude distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = latitude))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'latitude distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = housing_median_age))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'housing median age distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = total_rooms))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'total_rooms distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = population))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'population distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = households))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'households distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = median_income))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'median income distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = median_house_value))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'median house value distribution', x = '', y = 'Frequency')

ggplot(data = price_data, aes(x = median_house_value, y = ocean_proximity, fill = ocean_proximity))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'median house value boxplots with ocean proximity', y = '', x = 'median house value')

ggplot(data = price_data, aes(x = median_house_value, y = ocean_proximity, fill = ocean_proximity))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'median house value distribution dependance from ocean proximity', y = '', x = 'Frequency')

## Testing continuous variables distribution normality

x <- apply(price_num, 2, nortest::ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Distribution of factor variables analysis

ggplot(price_data %>% 
         select(ocean_proximity) %>% 
         group_by(ocean_proximity) %>% 
         count(), aes(x = n, y = reorder(ocean_proximity, n)))+
  geom_col(aes(fill = ocean_proximity))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'ocean proximity distribution', x = '', y = '')

pie(x = price_data %>% 
      select(ocean_proximity) %>% 
      group_by(ocean_proximity) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,2],
    labels = price_data %>% 
      select(ocean_proximity) %>% 
      group_by(ocean_proximity) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,1],
    main = "Pie Chart of ocean proximity")

## Checking factor variables variance level

price_fct <- price_data %>% select_if(is.factor) %>% as.data.frame()

c(colnames(price_fct), nrow(unique(price_fct))/nrow(price_fct)*100,
          as.vector(sort(table(price_fct), decreasing = TRUE)[1]/sort(table(price_fct), decreasing = TRUE)[2]))

colnames(price_fct)[nearZeroVar(price_fct)]

## Transforming continuous variables 

#price_data %<>% select(-highcor)

pstand <- preProcess(price_data[, 1:8], method = c('center', 'scale'))

price_data[, 1:8] <- predict(pstand, newdata = price_data[, 1:8])

# Modelling

ind <- createDataPartition(price_data$median_house_value, p = 0.8, list = FALSE)
price_data_train <- price_data[ind, ]
price_data_test <- price_data[-ind, ]

## basic regression tree with recursive partition

rt.val <- rpart(median_house_value ~ ., data = price_data_train)
rt.val

prettyTree(rt.val)

printcp(rt.val) # checking how tree's criterion are changing

rtp.val <- rpart(median_house_value ~ ., data = price_data_train, 
                control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.val)
with(rtp.val, {lines(cptable[,2]+1, cptable[,3],
                    type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.val <- prune(rtp.val, cp = 0.0053) # we have minimal error at cp = 0.0053
rtp.val
prettyTree(rtp.val)

## training recursive partition tree with train() function

rt.val.train <- train(median_house_value ~ ., data = price_data_train, method = 'rpart', tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.val.train
rtt.val <- rt.val.train$finalModel

## basic regression tree with conditional inference

ctree.val <- ctree(median_house_value ~ ., data = price_data_train)
ctree.val

## optimizing mincriterion with train()

ctree.val.train <- train(median_house_value ~ ., data = price_data_train, method = 'ctree', tuneLength = 10, trControl = trainControl(method = 'cv'))
ctree.val.train
ctreet.val <- ctree.val.train$finalModel

## bagging tree 

x <- as.data.frame(model.matrix(median_house_value ~ ., data = price_data_train)[, -1])
bag_val <- randomForest(x, price_data_train$median_house_value, mtry = ncol(x))

## boosting tree

library(gbm)  
set.seed(1)

boost.val <- gbm(median_house_value ~ ., data = price_data_train, distribution = 'gaussian',
                interaction.depth = 3, n.trees = 1000)
summary(boost.val)

## optimizing gbm() parameters with train()

gbmFit.val <- train(median_house_value ~ ., data = price_data_train, method = 'gbm', trControl = trainControl(method = 'cv'),
                   tuneGrid = expand.grid(.shrinkage = c(0.1, 0.05, 0.02), .interaction.depth = 2:5, .n.trees = 50, .n.minobsinnode = 10),
                   verbose = FALSE) 
gbmFit.val
gbmFit.val.final <- gbmFit.val$finalModel

## comparing models

ModCrit <- function(pred, fact){
  mae <- mean(abs(pred-fact))
  rmse <- sqrt(mean((pred-fact)^2))
  Rsq <- 1-sum((fact-pred)^2)/sum((mean(fact)-fact)^2)
  c(MAE=mae, RMSE=rmse, Rsq=Rsq)
}

y <- price_data_train$median_house_value
PriceF <- as.data.frame(model.matrix(y ~ ., price_data_train[, c(1:8,10)])[, -1])

Result <- rbind(
  rpart_prune = ModCrit(predict(rtp.val, price_data_train), price_data_train$median_house_value),
  rpart_train = ModCrit(predict(rtt.val, PriceF), price_data_train$median_house_value),
  ctree_train = ModCrit(predict(ctreet.val, PriceF), price_data_train$median_house_value),
  bagging = ModCrit(predict(bag_val, PriceF), price_data_train$median_house_value),
  boosting = ModCrit(predict(boost.val, price_data_train), price_data_train$median_house_value),
  boost_train = ModCrit(predict(gbmFit.val.final, price_data_train), price_data_train$median_house_value)
)

Result # bagging is the most accurate 

# Prediction

y <- price_data_test$median_house_value
PriceF <- as.data.frame(model.matrix(y ~ ., price_data_test[, c(1:8,10)])[, -1])
price_data_test$price_pred <- predict(bag_val, PriceF)
fact <- price_data_test$median_house_value
pred <- price_data_test$price_pred
mae <- mean(abs(pred-fact))
rmse <- sqrt(mean((pred-fact)^2))
Rsq <- 1-sum((fact-pred)^2)/sum((mean(fact)-fact)^2)
round(c(MAE=mae, RMSE=rmse, Rsq=Rsq),5)

ggplot(data = price_data_test, aes(x = median_house_value, y = price_pred))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_minimal()+
  labs(x = 'fact value', y = 'pred value')
