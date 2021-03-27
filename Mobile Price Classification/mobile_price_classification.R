library(tidyverse)
library(magrittr)
library(caret)
library(DMwR)
library(rpart)
library(party)
library(randomForest)
library(gbm) 
library(bst)

# Importing training data

mobile_train <- read.csv('train.csv', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  mutate(across(.cols = c('blue', 'dual_sim', 'four_g', 'three_g',
                          'touch_screen', 'wifi', 'price_range'), .fns = factor)) %>% 
  mutate_if(.predicate = is.integer, .funs = as.double)

summary(mobile_train)
anyNA(mobile_train)

# Exploratory data analysis

## Correlation between continuous variables

Mcor <- cor(mobile_train %>% select_if(is.numeric))
corrplot::corrplot(Mcor, method = 'color', addCoef.col = 'green', addgrid.col = 'gray33',
                   tl.col = 'black')

GGally::ggpairs(mobile_train %>% select_if(is.numeric))

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

mobile_num <- mobile_train %>% select_if(is.numeric)

top.mat(cor(mobile_num), level = 0.75, N = ncol(mobile_num))

highcor <- colnames(mobile_num[, findCorrelation(cor(mobile_num), cutoff = 0.75)])

findLinearCombos(mobile_num)

## Checking distribution of continuous variables

ggplot(mobile_num, aes(x = battery_power))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Battery power (mAh) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = clock_speed))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Microprocessor speed distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = fc))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Front camera mega pixels amount distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = int_memory))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Internal memory amount (GB) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = m_dep))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Mobile depth (cm) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = mobile_wt))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Mobile weight distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = n_cores))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Number of cores of processor distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = pc))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Primary camera mega pixels amount distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = px_height))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Pixel resolution height distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = px_width))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Pixel resolution width distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = ram))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Random access memory amount (MB) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = sc_h))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Screen height (cm) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = sc_w))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Screen width (cm) distribution', x = '', y = 'Frequency')

ggplot(mobile_num, aes(x = talk_time))+
  geom_bar(alpha = 0.75, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Longest talking time with a single battery charge distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = battery_power, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Battery power (mAh) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = clock_speed, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Microprocessor speed by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = fc, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Front camera mega pixels amount by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = int_memory, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Internal memory amount (GB) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = m_dep, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Mobile depth (cm) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = mobile_wt, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Mobile weight by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = n_cores, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Number of cores of processor by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = pc, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Primary camera mega pixels amount by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = px_height, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Pixel resolution height by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = px_width, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Pixel resolution width by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = ram, fill = price_range))+
  geom_density(alpha = 0.5)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Random access memory amount (MB) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = sc_h, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Screen height (cm) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = sc_w, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Screen width (cm) by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = talk_time, fill = price_range))+
  geom_bar(alpha = 0.75)+
  facet_wrap(~ price_range)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Longest talking time with a single battery charge by price range distribution', x = '', y = 'Frequency')

ggplot(mobile_train, aes(x = price_range, y = battery_power, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Battery power (mAh) by price range boxplots', x = '', y = 'Battery power (mAh)')

ggplot(mobile_train, aes(x = price_range, y = clock_speed, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Microprocessor speed by price range boxplots', x = '', y = 'Microprocessor speed')

ggplot(mobile_train, aes(x = price_range, y = fc, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Front camera mega pixels amount by price range boxplots', x = '', y = 'Front camera mega pixels amount')

ggplot(mobile_train, aes(x = price_range, y = int_memory, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Internal memory amount (GB) by price range boxplots', x = '', y = 'Internal memory amount (GB)')

ggplot(mobile_train, aes(x = price_range, y = m_dep, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Mobile depth (cm) by price range boxplots', x = '', y = 'Mobile depth (cm)')

ggplot(mobile_train, aes(x = price_range, y = mobile_wt, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Mobile weight by price range boxplots', x = '', y = 'Mobile weight')

ggplot(mobile_train, aes(x = price_range, y = n_cores, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Number of cores of processor by price range boxplots', x = '', y = 'Number of cores of processor')

ggplot(mobile_train, aes(x = price_range, y = pc, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Primary camera mega pixels amount by price range boxplots', x = '', y = 'Primary camera mega pixels amount')

ggplot(mobile_train, aes(x = price_range, y = px_height, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Pixel resolution height by price range boxplots', x = '', y = 'Pixel resolution height')

ggplot(mobile_train, aes(x = price_range, y = px_width, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Pixel resolution width by price range boxplots', x = '', y = 'Pixel resolution width')

ggplot(mobile_train, aes(x = price_range, y = ram, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Random access memory amount (MB) by price range boxplots', x = '', y = 'Random access memory amount (MB)')

ggplot(mobile_train, aes(x = price_range, y = sc_h, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Screen height (cm) by price range boxplots', x = '', y = 'Screen height (cm)')

ggplot(mobile_train, aes(x = price_range, y = sc_w, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Screen width (cm) by price range boxplots', x = '', y = 'Screen width (cm)')

ggplot(mobile_train, aes(x = price_range, y = talk_time, fill = price_range))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Longest talking time with a single battery charge by price range boxplots', x = '', y = 'Longest talking time with a single battery charge')

## Testing continuous variables distribution normality

x <- apply(mobile_num, 2, nortest::ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Testing numeric variables for outliers

y <- apply(mobile_num,2,function(col) outliers::grubbs.test(col, type=10))

for (i in 1:length(y)){
  print(y[[i]]$p.value)
} #variables don't have outliers

## Distribution of factor variables analysis

ggplot(data = mobile_train %>% 
         select(blue, price_range) %>% 
         group_by(blue, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = blue))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Bluetooth availability by price range', y = 'Share', x = '')

ggplot(data = mobile_train %>% 
         select(four_g, price_range) %>% 
         group_by(four_g, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = four_g))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = '4G availability by price range', y = 'Share', x = '')

ggplot(data = mobile_train %>% 
         select(dual_sim, price_range) %>% 
         group_by(dual_sim, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = dual_sim))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Dual sim availability by price range', y = 'Share', x = '')

ggplot(data = mobile_train %>% 
         select(three_g, price_range) %>% 
         group_by(three_g, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = three_g))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = '3G availability by price range', y = 'Share', x = '')

ggplot(data = mobile_train %>% 
         select(touch_screen, price_range) %>% 
         group_by(touch_screen, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = touch_screen))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Touch screen availability by price range', y = 'Share', x = '')

ggplot(data = mobile_train %>% 
         select(wifi, price_range) %>% 
         group_by(wifi, price_range) %>% 
         count(), aes(x = price_range, y = n, fill = wifi))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Wi-Fi screen availability by price range', y = 'Share', x = '')

## Checking factor variables variance level

mobile_fct <- mobile_train %>% select_if(., is.factor) %>% select(-'price_range') %>% as.data.frame()

for(i in 1:length(mobile_fct)){
  print(c(colnames(mobile_fct)[i], length(unique(mobile_fct[,i]))/nrow(mobile_fct)*100,
          as.vector(sort(table(mobile_fct[,i]), decreasing = TRUE)[1]/sort(table(mobile_fct[,i]), decreasing = TRUE)[2])))
}

colnames(mobile_fct)[nearZeroVar(mobile_fct)]

## Transforming continuous variables 

pstand <- preProcess(mobile_train[, c(1,3,5,7:17)], method = c('center', 'scale'))

mobile_model <- mobile_train

mobile_model[, c(1,3,5,7:17)] <- predict(pstand, newdata = mobile_model[, c(1,3,5,7:17)])

## Splitting data into train and test

ind <- createDataPartition(y = mobile_model$price_range, p = 0.8, list = FALSE)
mobile_model_train <- mobile_model[ind, ]
mobile_model_test <- mobile_model[-ind, ]

# Modeling

## recursive partition tree

rt.range <- rpart(price_range ~ ., data = mobile_model_train)
rt.range
prettyTree(rt.range)

printcp(rt.range) # checking how tree's criterion are changing

rtp.range <- rpart(price_range ~ ., data = mobile_model_train, 
                    control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.range)
with(rtp.range, {lines(cptable[,2]+1, cptable[,3],
                        type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.range <- prune(rtp.range, cp = 0.0058) # we have minimal error at cp = 0.0058
rtp.range

## training recursive partition tree with train() function

rt.range.train <- train(price_range ~ ., data = mobile_model_train, method = 'rpart', tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.range.train

## basic regression tree with conditional inference

ctree.range <- ctree(price_range ~ ., data = mobile_model_train)
ctree.range

## optimizing mincriterion with train()

ctree.range.train <- train(price_range ~ ., data = mobile_model_train, method = 'ctree', tuneLength = 10, trControl = trainControl(method = 'cv'))
ctree.range.train

## bagging tree 

x <- model.matrix(price_range ~ ., data = mobile_model_train)[, -1]

bag.range <- randomForest(x, mobile_model_train$price_range, mtry = ncol(x))
bag.range

## optimizing bagging model with train()
bag.range.train <- train(x, mobile_model_train$price_range,
                          method = 'rf', trControl = trainControl(method = 'cv'),
                          tuneGrid = expand.grid(.mtry = ncol(x)))
bag.range.train

## random forest model and mtry parameter optimizing with train()

ranfor.range.train <- train(x, mobile_model_train$price_range,
                             method = 'rf', trControl = trainControl(method = 'cv'),
                             tuneGrid = expand.grid(.mtry = 2:10), importance = TRUE)
ranfor.range.train
varImpPlot(ranfor.range.train$finalModel) # predictors importance
# let's see the trees number when model error becomes constant
plot(ranfor.range.train$finalModel, col='blue', lwd=2, main = '')
plot(bag.range.train$finalModel, col='green', lwd=2, add=TRUE)
legend('topright', c('Bagging', 'RandomForest'), col = c('green','blue'), lwd = 2)

## boosting

boost.range <- gbm(price_range ~ ., data = mobile_model_train, distribution = 'multinomial',
                    interaction.depth = 3, n.trees = 1000)
summary(boost.range)

## optimizing gbm() parameters with train()

gbmFit.range <- train(price_range ~ ., data = mobile_model_train, method = 'gbm', trControl = trainControl(method = 'cv'),
                       tuneGrid = expand.grid(.shrinkage = c(0.1, 0.05, 0.02), .interaction.depth = 2:5, .n.trees = 50, .n.minobsinnode = 1),
                       verbose = FALSE)
gbmFit.range

## boosting with bstTree() function

modelLookup('bstTree')

boostFit.range <- train(price_range ~ ., data = mobile_model_train, method = 'bstTree', trControl = trainControl(method = 'cv'),
                        tuneGrid = expand.grid(.mstop = c(50, 100, 150), .maxdepth = 1:4, .nu = 0.1))
boostFit.range
plot(boostFit.range)

# Preparing test data

mobile_test <- read.csv('test.csv', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  select(-id) %>% 
  mutate(across(.cols = c('blue', 'dual_sim', 'four_g', 'three_g',
                          'touch_screen', 'wifi'), .fns = factor)) %>% 
  mutate_if(.predicate = is.integer, .funs = as.double)

mobile_test[, c(1,3,5,7:17)] <- predict(pstand, newdata = mobile_test[, c(1,3,5,7:17)])

# comparing models

ModCrit <- function(pred, fact){
  cM <- table(Fact = fact, Predict = pred)
  accuracy <- (cM[1,1]+cM[2,2]+cM[3,3]+cM[4,4])/sum(cM)
  c(Accuracy=accuracy)
}

Result_fact <- rbind(
  rpart_prune = ModCrit(predict(rtp.range, mobile_model_train, type = 'class'), mobile_model_train$price_range),
  rpart_train = ModCrit(predict(rt.range.train$finalModel, as.data.frame(x), type = 'class'), mobile_model_train$price_range),
  ctree = ModCrit(predict(ctree.range, mobile_model_train, type = 'response'), mobile_model_train$price_range),
  ctree_train = ModCrit(predict(ctree.range.train$finalModel, as.data.frame(x), type = 'response'), mobile_model_train$price_range),
  bagging = ModCrit(predict(bag.range, x), mobile_model_train$price_range),
  bagging_train = ModCrit(predict(bag.range.train$finalModel, x), mobile_model_train$price_range),
  randomforest = ModCrit(predict(ranfor.range.train$finalModel, x), mobile_model_train$price_range),
  boosting = ModCrit(apply(as.data.frame(predict(boost.range, mobile_model_train, type = 'response')) %>% 
                             `colnames<-`(., c('0','1','2','3')), 1, function(i) which.max(i)-1), mobile_model_train$price_range),
  boost_train_gbm = ModCrit(predict(gbmFit.range, mobile_model_train, type = 'raw'), mobile_model_train$price_range),
  boost_train_bst = ModCrit(predict(boostFit.range, mobile_model_train, type = 'raw'), mobile_model_train$price_range)
)

apply(as.data.frame(predict(boost.range, mobile_model_train, type = 'response')) %>% `colnames<-`(., c('0','1','2','3')), 1, function(i) which.max(i)-1)

Result_fact <- as.data.frame(Result_fact)

x_test <- model.matrix(price_range ~ ., data = mobile_model_test)[, -1]

Result_test <- rbind(
  rpart_prune = ModCrit(predict(rtp.range, mobile_model_test, type = 'class'), mobile_model_test$price_range),
  rpart_train = ModCrit(predict(rt.range.train$finalModel, as.data.frame(x_test), type = 'class'), mobile_model_test$price_range),
  ctree = ModCrit(predict(ctree.range, mobile_model_test, type = 'response'), mobile_model_test$price_range),
  ctree_train = ModCrit(predict(ctree.range.train$finalModel, as.data.frame(x_test), type = 'response'), mobile_model_test$price_range),
  bagging = ModCrit(predict(bag.range, x_test), mobile_model_test$price_range),
  bagging_train = ModCrit(predict(bag.range.train$finalModel, x_test), mobile_model_test$price_range),
  randomforest = ModCrit(predict(ranfor.range.train$finalModel, x_test), mobile_model_test$price_range),
  boosting = ModCrit(apply(as.data.frame(predict(boost.range, mobile_model_test, type = 'response')) %>% 
                             `colnames<-`(., c('0','1','2','3')), 1, function(i) which.max(i)-1), mobile_model_test$price_range),
  boost_train_gbm = ModCrit(predict(gbmFit.range, mobile_model_test, type = 'raw'), mobile_model_test$price_range),
  boost_train_bst = ModCrit(predict(boostFit.range, mobile_model_test, type = 'raw'), mobile_model_test$price_range)
)

Result_test <- as.data.frame(Result_test)

Result_compare <- Result_fact %>% 
  mutate(model = rownames(.) %>% factor) %>% 
  tibble %>% 
  rename('Accuracy_train' = 'Accuracy') %>% 
  left_join(., Result_test %>% mutate(model = rownames(.) %>% factor) %>% tibble, by = 'model') %>% 
  rename('Accuracy_test' = 'Accuracy')

ggplot(data = Result_compare, aes(x = Accuracy_train, y = Accuracy_test, color = model))+
  geom_point(size = 3)+
  scale_x_continuous(breaks = seq(0.4, 1, by = 0.025))+
  scale_y_continuous(breaks = seq(0.4, 1, by = 0.025), limits = c(0.4, 1))+
  theme_minimal()+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))+
  labs(title = 'Models accuracy on train and test data', x = 'Accuracy on train data', y = 'Accuracy on test data')
