library(tidyverse)
library(magrittr)
library(DMwR)
library(caret)
library(rpart)
library(party)
library(randomForest)
library(gbm) 

# Importing data

insurance_data <- read.csv('insurance.csv', stringsAsFactors = TRUE) %>% 
  tibble()

summary(insurance_data)
anyNA(insurance_data)

# Exploratory data analysis

## Correlation between continuous variables

Mcor <- cor(insurance_data %>% select_if(is.numeric))
corrplot::corrplot(Mcor, method = 'color', addCoef.col = 'green', addgrid.col = 'gray33',
                   tl.col = 'black')

GGally::ggpairs(insurance_data %>% select_if(is.numeric))

## Checking multicollinearity of continuous variables

insurance_num <- insurance_data %>% select_if(is.numeric) %>% as.data.frame(.)

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

top.mat(cor(insurance_num), level = 0.5, N = ncol(insurance_num))

highcor <- colnames(insurance_num[, findCorrelation(cor(insurance_num), cutoff = 0.5)])

findLinearCombos(insurance_num)

## Checking distribution of continuous variables

ggplot(data = insurance_data, aes(x = age, fill = sex))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'age distribution', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = bmi, fill = sex))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'bmi distribution', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = children, fill = sex))+
  geom_bar(alpha = 0.75, col = 'black')+
  scale_x_continuous(breaks = seq(0, 6, by = 1))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'number of children distribution', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = charges, fill = sex))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'insurance size distribution', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = sex, y = age, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'age by gender boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = sex, y = bmi, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'bmi by gender boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = sex, y = charges, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'insurance size by gender boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = smoker, y = age, fill = smoker))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'age by smoker violinplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = smoker, y = bmi, fill = smoker))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'bmi by smoker violinplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = smoker, y = charges, fill = smoker))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'insurance size by smoker violinplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = age, y = region, fill = region))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'age by region violinplots', x = 'Frequency', y = '')

ggplot(data = insurance_data, aes(x = bmi, y = region, fill = region))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'bmi by region violinplots', x = 'Frequency', y = '')

ggplot(data = insurance_data, aes(x = charges, y = region, fill = region))+
  geom_violin()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'insurance size by region violinplots', x = 'Frequency', y = '')

ggplot(data = insurance_data, aes(x = sex, y = age, fill = region))+
  geom_boxplot()+
  facet_wrap(~ region)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'age by region and sex boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = sex, y = bmi, fill = region))+
  geom_boxplot()+
  facet_wrap(~ region)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'bmi by region and sex boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = sex, y = charges, fill = region))+
  geom_boxplot()+
  facet_wrap(~ region)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'insurance size by region and sex boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = smoker, y = charges, fill = region))+
  geom_boxplot()+
  facet_wrap(~ region)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'insurance size by region and smoker boxplots', x = '', y = 'Frequency')

ggplot(data = insurance_data, aes(x = smoker, y = charges, fill = sex))+
  geom_boxplot()+
  facet_wrap(~ sex)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'insurance size by sex and smoker boxplots', x = '', y = 'Frequency')

## Testing continuous variables distribution normality

x <- apply(insurance_num, 2, nortest::ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Distribution of factor variables analysis

ggplot(insurance_data %>% 
         select(sex) %>% 
         group_by(sex) %>% 
         count(), aes(x = reorder(sex, n), y = n))+
  geom_col(aes(fill = sex))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'gender distribution', x = '', y = '')

pie(x = insurance_data %>% 
      select(sex) %>% 
      group_by(sex) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,2],
    labels = insurance_data %>% 
      select(sex) %>% 
      group_by(sex) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,1],
    main = "Pie Chart of gender")

ggplot(insurance_data %>% 
         select(smoker) %>% 
         group_by(smoker) %>% 
         count(), aes(x = reorder(smoker, n), y = n))+
  geom_col(aes(fill = smoker))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'smoker distribution', x = '', y = '')

pie(x = insurance_data %>% 
      select(smoker) %>% 
      group_by(smoker) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,2],
    labels = insurance_data %>% 
      select(smoker) %>% 
      group_by(smoker) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,1],
    main = "Pie Chart of smoker")

ggplot(insurance_data %>% 
         select(region) %>% 
         group_by(region) %>% 
         count(), aes(x = reorder(region, n), y = n))+
  geom_col(aes(fill = region))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'region distribution', x = '', y = '')

pie(x = insurance_data %>% 
      select(region) %>% 
      group_by(region) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,2],
    labels = insurance_data %>% 
      select(region) %>% 
      group_by(region) %>% 
      count() %>% ungroup() %>% as.data.frame(.) %>% .[,1],
    main = "Pie Chart of smoker")

## Checking factor variables variance level

insurance_fct <- insurance_data %>% select_if(is.factor) %>% as.data.frame()

for(i in 1:length(insurance_fct)){
  print(c(colnames(insurance_fct)[i], length(unique(insurance_fct[,i]))/nrow(insurance_fct)*100,
          as.vector(sort(table(insurance_fct[,i]), decreasing = TRUE)[1]/sort(table(insurance_fct[,i]), decreasing = TRUE)[2])))
}

colnames(insurance_fct)[nearZeroVar(insurance_fct)]

## Transforming continuous variables 

pstand <- preProcess(insurance_data[, c(1,3,4)], method = c('center', 'scale'))

insurance_data[, c(1,3,4)] <- predict(pstand, newdata = insurance_data[, c(1,3,4)])

# Modelling

ind <- createDataPartition(insurance_data$charges, p = 0.8, list = FALSE)
insurance_train <- insurance_data[ind, ]
insurance_test <- insurance_data[-ind, ]

## standard linear model

lm.charge <- lm(charges ~ ., data = insurance_train)
summary(lm.charge)
plot(lm.charge)

## step predictors elimination

lm_step.charge <- step(lm.charge, trace = 0)
summary(lm_step.charge)
anova(lm.charge, lm_step.charge) # step model is not statistically significant

## training lm() with train()

lm.charge.cv <- train(charges ~ ., data = insurance_train, method = 'lm', trControl = trainControl(method = 'cv'))
summary(lm.charge.cv$finalModel) # same results as standard lm()

## lasso regression

x <- model.matrix(charges ~ ., data = insurance_train)[, -1]
grid <- 10^seq(10,-2,length=100)
library(glmnet)
lasso.charge <- glmnet(x, insurance_train$charges, alpha = 1, lambda = grid)
plot(lasso.charge, xvar = 'lambda', label = TRUE, lwd = 2)

## optimizing lambda with train()

grid.train <- seq(0.5, 6, length = 15)
lasso.charge.train <- train(as.data.frame(x), insurance_train$charges, method = 'glmnet',
                        tuneGrid = expand.grid(.lambda = grid.train, .alpha = 1),
                        trControl = trainControl(method = 'cv'))
coef(lasso.charge.train$finalModel, lasso.charge.train$bestTune$lambda)

## private least squares model

library(pls)
M.pls <- plsr(insurance_train$charges ~ x, scale = TRUE, Validation = 'CV', method = 'oscorespls')
summary(M.pls)

## optimizing components number with train()

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10)
plsTune.charge <- train(x, insurance_train$charges, method = 'pls', tuneLength = 14, trControl = ctrl)
plsTune.charge

## principal components regression

pcrTune.charge <- train(x, insurance_train$charges, method = 'pcr', tuneLength = 14,
                    trControl = ctrl)
pcrTune.charge

## kNN regression

knnTune.charge <- train(x, insurance_train$charges, method = 'knn',
                    trControl = ctrl, tuneGrid = data.frame(.k = 4:25))
knnTune.charge
plot(knnTune.charge)

## recursive partition tree

rt.charge <- rpart(charges ~ ., data = insurance_train)
rt.charge

prettyTree(rt.charge)

printcp(rt.charge) # checking how tree's criterion are changing

rtp.charge <- rpart(charges ~ ., data = insurance_train, 
                 control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.charge)
with(rtp.charge, {lines(cptable[,2]+1, cptable[,3],
                     type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.charge <- prune(rtp.charge, cp = 0.0055) # we have minimal error at cp = 0.0055
rtp.charge
prettyTree(rtp.charge)

## training recursive partition tree with train() function

rt.charge.train <- train(charges ~ ., data = insurance_train, method = 'rpart', tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.charge.train

## basic regression tree with conditional inference

ctree.charge <- ctree(charges ~ ., data = insurance_train)
ctree.charge

## optimizing mincriterion with train()

ctree.charge.train <- train(charges ~ ., data = insurance_train, method = 'ctree', tuneLength = 10, trControl = trainControl(method = 'cv'))
ctree.charge.train

## bagging tree 

bag.charge <- randomForest(x, insurance_train$charges, mtry = ncol(x))
bag.charge

## optimizing bagging model with train()
bag.charge.train <- train(x, insurance_train$charges,
                method = 'rf', trControl = trainControl(method = 'cv'),
                tuneGrid = expand.grid(.mtry = ncol(x)))
bag.charge.train

## random forest model and mtry parameter optimizing with train()

ranfor.charge.train <- train(x, insurance_train$charges,
                   method = 'rf', trControl = trainControl(method = 'cv'),
                   tuneGrid = expand.grid(.mtry = 2:8), importance = TRUE)
ranfor.charge.train
varImpPlot(ranfor.charge.train$finalModel) # predictors importance
# let's see the trees number when model error becomes constant
plot(ranfor.charge.train$finalModel, col='blue', lwd=2, main = '')
plot(bag.charge.train$finalModel, col='green', lwd=2, add=TRUE)
legend('topright', c('Bagging', 'RandomForest'), col = c('green','blue'), lwd = 2)

## boosting

boost.charge <- gbm(charges ~ ., data = insurance_train, distribution = 'gaussian',
                interaction.depth = 3, n.trees = 1000)
summary(boost.charge)

## optimizing gbm() parameters with train()

gbmFit.charge <- train(charges ~ ., data = insurance_train, method = 'gbm', trControl = trainControl(method = 'cv'),
                   tuneGrid = expand.grid(.shrinkage = c(0.1, 0.05, 0.02), .interaction.depth = 2:5, .n.trees = 50, .n.minobsinnode = 10),
                   verbose = FALSE)
gbmFit.charge

## boosting with bstTree() function

library(bst)
boostFit.charge <- train(charges ~ ., data = insurance_train, method = 'bstTree', trControl = trainControl(method = 'cv'))
boostFit.charge
plot(boostFit.charge)

# comparing all models

ModCrit <- function(pred, fact){
  mae <- mean(abs(pred-fact))
  rmse <- sqrt(mean((pred-fact)^2))
  Rsq <- 1-sum((fact-pred)^2)/sum((mean(fact)-fact)^2)
  c(MAE=mae, RMSE=rmse, Rsq=Rsq)
}

x <- as.data.frame(x)

Result_fact <- rbind(
  lm = ModCrit(predict(lm.charge, insurance_train), insurance_train$charges),
  lasso_train = ModCrit(predict(lasso.charge.train, as.matrix(x)), insurance_train$charges),
  pls = ModCrit(predict(plsTune.charge, x), insurance_train$charges),
  pcr = ModCrit(predict(pcrTune.charge, x), insurance_train$charges),
  kNN = ModCrit(predict(knnTune.charge, x), insurance_train$charges),
  rpart_prune = ModCrit(predict(rtp.charge, insurance_train), insurance_train$charges),
  rpart_train = ModCrit(predict(rt.charge.train$finalModel, x), insurance_train$charges),
  ctree = ModCrit(predict(ctree.charge, insurance_train), insurance_train$charges),
  ctree_train = ModCrit(predict(ctree.charge.train$finalModel, x), insurance_train$charges),
  bagging = ModCrit(predict(bag.charge, x), insurance_train$charges),
  bagging_train = ModCrit(predict(bag.charge.train$finalModel, x), insurance_train$charges),
  randomforest = ModCrit(predict(ranfor.charge.train$finalModel, x), insurance_train$charges),
  boosting = ModCrit(predict(boost.charge, insurance_train), insurance_train$charges),
  boost_train_gbm = ModCrit(predict(gbmFit.charge$finalModel, x), insurance_train$charges),
  boost_train_bst = ModCrit(predict(boostFit.charge$finalModel, x), insurance_train$charges)
)

Result_fact <- as.data.frame(Result_fact)
Result_fact[order(Result_fact$RMSE),]

x_test <- as.data.frame(model.matrix(charges ~ ., insurance_test)[, -1])

Result_test <- rbind(
  lm = ModCrit(predict(lm.charge, insurance_test), insurance_test$charges),
  lasso_train = ModCrit(predict(lasso.charge.train, as.matrix(x_test)), insurance_test$charges),
  pls = ModCrit(predict(plsTune.charge, x_test), insurance_test$charges),
  pcr = ModCrit(predict(pcrTune.charge, x_test), insurance_test$charges),
  kNN = ModCrit(predict(knnTune.charge, x_test), insurance_test$charges),
  rpart_prune = ModCrit(predict(rtp.charge, insurance_test), insurance_test$charges),
  rpart_train = ModCrit(predict(rt.charge.train$finalModel, x_test), insurance_test$charges),
  ctree = ModCrit(predict(ctree.charge, insurance_test), insurance_test$charges),
  ctree_train = ModCrit(predict(ctree.charge.train$finalModel, x_test), insurance_test$charges),
  bagging = ModCrit(predict(bag.charge, x_test), insurance_test$charges),
  bagging_train = ModCrit(predict(bag.charge.train$finalModel, x_test), insurance_test$charges),
  randomforest = ModCrit(predict(ranfor.charge.train$finalModel, x_test), insurance_test$charges),
  boosting = ModCrit(predict(boost.charge, insurance_test), insurance_test$charges),
  boost_train_gbm = ModCrit(predict(gbmFit.charge$finalModel, x_test), insurance_test$charges),
  boost_train_bst = ModCrit(predict(boostFit.charge$finalModel, x_test), insurance_test$charges)
)

Result_test

Result_test <- as.data.frame(Result_test)
Result_test[order(Result_test$RMSE),]

Result_compare <- Result_fact %>% 
  mutate(model = rownames(.) %>% factor) %>% 
  tibble %>% 
  rename('Rsq_fact' = 'Rsq') %>% 
  left_join(., Result_test %>% mutate(model = rownames(.) %>% factor) %>% tibble, by = 'model') %>% 
  select(model, Rsq_fact, Rsq) %>% 
  rename('Rsq_pred' = 'Rsq')

ggplot(data = Result_compare, aes(x = Rsq_fact, y = Rsq_pred, color = model))+
  geom_point(size = 3)+
  scale_x_continuous(breaks = seq(0.75, 1, by = 0.025))+
  scale_y_continuous(breaks = seq(0.75, 1, by = 0.025), limits = c(0.725, 0.9))+
  theme_minimal()+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))+
  labs(title = 'Models accuracy on train and test data', x = 'Rsquared on train data', y = 'Rsquared on test data')
