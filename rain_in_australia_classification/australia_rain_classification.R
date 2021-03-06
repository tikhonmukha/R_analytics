library(tidyverse)
library(magrittr)
library(DMwR)
library(caret)
library(rpart)
library(party)
library(pROC)

# Importing data

rain_data <- read.csv('weatherAUS.csv', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  mutate(Date = Date %>% factor(.) %>% as.Date(., format('%Y-%m-%d'))) %>% 
  mutate(weekday = weekdays(Date) %>% factor(.), month = months(Date) %>% factor(.))

rain_int <- rain_data %>% 
  select_if(is.integer) %>% 
  mutate_all(as.double)

rain_data[, sapply(rain_data, function (col) is.integer(col))] <- rain_int

summary(rain_data)

# Exploratory data analysis

## Correlation between continuous variables

apply(rain_data, 2, anyNA)

Mcor <- cor(rain_data %>% select_if(is.numeric) %>% na.omit(.))
corrplot::corrplot(Mcor, method = 'color', addCoef.col = 'green', addgrid.col = 'gray33',
         tl.col = 'black')

pairs(Mcor)

## Filling continuous variables NA-values 

rain_num <- rain_data %>% select_if(is.numeric) %>% as.data.frame(.)
ind <- apply(rain_num, 1, function(x) sum(is.na(x))) > 0
rain_num[ind, ]
pPmI <- preProcess(rain_num, method = 'medianImpute')
rain_num <- predict(pPmI, newdata = rain_num)
Imp.Med <- rain_num[ind, ]

rain_num <- rain_data %>% select_if(is.numeric) %>% as.data.frame(.)
pPbI <- preProcess(rain_num, method = 'bagImpute')
rain_num <- predict(pPbI, newdata = rain_num)
Imp.Bag <- rain_num[ind, ]

ImpVal <- rbind(Imp.Med, Imp.Bag)
Imp.Method <- as.factor(c(rep('Med', 87224), rep('Bag', 87224)))
library(RANN)
library(vegan)
Imp.M <- rda(ImpVal ~ Imp.Method, ImpVal)
plot(Imp.M, display = 'sites', type = 'p')
ordihull(Imp.M, Imp.Method, draw = 'polygon', alpha = 67, 
         lty = 2, col = c(1,2), label = TRUE)

rain_data[ind, colnames(rain_num)] <- Imp.Bag

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

top.mat(cor(rain_num), level = 0.75, N = ncol(rain_num))

highcor <- names(rain_num[, findCorrelation(cor(rain_num), cutoff = 0.75)])

findLinearCombos(rain_num)

## Checking distribution of continuous variables

ggplot(data = na.omit(rain_data), aes(x = MinTemp, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Minimal temperature distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = MaxTemp, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Maximal temperature distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Rainfall))+
  geom_density(fill = 'red', alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Rainfall distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Evaporation, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Evaporation distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Sunshine, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Sunshine hours per day distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = WindGustSpeed, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Wind gust speed distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = WindSpeed9am, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Wind speed at 9am distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = WindSpeed3pm, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Wind speed at 3pm distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Humidity9am, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Humidity at 9am distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Humidity3pm, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Humidity at 3pm distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Pressure9am, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Pressure at 9am distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Pressure3pm, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Pressure at 3pm distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Cloud9am, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Fraction of sky obscured by cloud at 9am distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Cloud3pm, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Fraction of sky obscured by cloud at 3pm distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Temp9am, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Temperature at 9am distribution', x = '', y = 'Frequency')

ggplot(data = na.omit(rain_data), aes(x = Temp3pm, fill = RainTomorrow))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Temperature at 3pm distribution', x = '', y = 'Frequency')

rain_data %<>% 
  mutate(month = case_when(month == 'Январь' ~ 'January',
                   month == 'Февраль' ~ 'February',
                   month == 'Март' ~ 'March',
                   month == 'Апрель' ~ 'April',
                   month == 'Май' ~ 'May',
                   month == 'Июнь' ~ 'June',
                   month == 'Июль' ~ 'July',
                   month == 'Август' ~ 'August',
                   month == 'Сентябрь' ~ 'September',
                   month == 'Октябрь' ~ 'October',
                   month == 'Ноябрь' ~ 'November',
                   month == 'Декабрь' ~ 'December') %>% factor(., levels = month.name))

ggplot(na.omit(rain_data), aes(x = MinTemp, y = month, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Minimal temperature per month distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = MaxTemp, y = month, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Maximal temperature per month distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = Temp9am, y = month, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Daily temperature at 9am per month distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = Temp3pm, y = month, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Daily temperature at 3pm per month distribution', x = '', y = 'Frequency')

rain_data %<>% 
  mutate(weekday = case_when(weekday == 'понедельник' ~ 'monday',
                             weekday == 'вторник' ~ 'tuesday',
                             weekday == 'среда' ~ 'wednesday',
                             weekday == 'четверг' ~ 'thursday',
                             weekday == 'пятница' ~ 'friday',
                             weekday == 'суббота' ~ 'saturday',
                             weekday == 'воскресенье' ~ 'sunday') %>% factor)

ggplot(na.omit(rain_data), aes(x = MinTemp, y = weekday, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Minimal temperature per day distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = MaxTemp, y = weekday, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Maximal temperature per day distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = Temp9am, y = weekday, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Daily temperature at 9am per day distribution', x = '', y = 'Frequency')

ggplot(na.omit(rain_data), aes(x = Temp3pm, y = weekday, fill = RainTomorrow)) +
  ggridges::geom_density_ridges(alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Daily temperature at 3pm per day distribution', x = '', y = 'Frequency')

## Testing continuous variables distribution normality

x <- apply(rain_num, 2, nortest::ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Testing continuous variables significance

y <- apply(rain_data %>% na.omit(.) %>% select_if(., is.numeric),
           2, function(col) wilcox.test(col ~ RainTomorrow, data = na.omit(rain_data)))

for (i in 1:length(y)){
  print(y[[i]]$p.value)
} #all variables are significant

## Distribution of factor variables analysis

ggplot(rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(Location) %>% 
         group_by(Location) %>% 
         count(), aes(x = n, y = reorder(Location, n)))+
  geom_col(aes(fill = Location))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Location distribution', x = '', y = '')

ggplot(rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindGustDir) %>% 
         group_by(WindGustDir) %>% 
         count(), aes(x = reorder(WindGustDir, -n), y = n))+
  geom_col(aes(fill = WindGustDir))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Wind gust direction distribution', x = '', y = '')

ggplot(rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindDir9am) %>% 
         group_by(WindDir9am) %>% 
         count(), aes(x = reorder(WindDir9am, -n), y = n))+
  geom_col(aes(fill = WindDir9am))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Wind direction at 9am distribution', x = '', y = '')

ggplot(rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindDir3pm) %>% 
         group_by(WindDir3pm) %>% 
         count(), aes(x = reorder(WindDir3pm, -n), y = n))+
  geom_col(aes(fill = WindDir3pm))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Wind direction at 3pm distribution', x = '', y = '')

## Looking for differences of factor variables distribution by RainTomorrow

ggplot(data = rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindGustDir, RainTomorrow) %>% 
         group_by(WindGustDir, RainTomorrow) %>% 
         count() %>% arrange(RainTomorrow, desc(n)), aes(x = RainTomorrow, y = n, fill = reorder(WindGustDir,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Wind gust direction difference', x = '', y = 'Share')

ggplot(data = rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindDir9am, RainTomorrow) %>% 
         group_by(WindDir9am, RainTomorrow) %>% 
         count() %>% arrange(RainTomorrow, desc(n)), aes(x = RainTomorrow, y = n, fill = reorder(WindDir9am,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Wind direction at 9 am difference', x = '', y = 'Share')

ggplot(data = rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(WindDir3pm, RainTomorrow) %>% 
         group_by(WindDir3pm, RainTomorrow) %>% 
         count() %>% arrange(RainTomorrow, desc(n)), aes(x = RainTomorrow, y = n, fill = reorder(WindDir3pm,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Wind direction at 3 pm difference', x = '', y = 'Share')

ggplot(data = rain_data %>% 
         filter(complete.cases(.)) %>% 
         select(RainToday, RainTomorrow) %>% 
         group_by(RainToday, RainTomorrow) %>% 
         count() %>% arrange(RainTomorrow, desc(n)), aes(x = RainTomorrow, y = n, fill = reorder(RainToday,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'RainToday difference', x = '', y = 'Share')

## Checking factor variables variance level

rain_fct <- rain_data %>% 
  select_if(is.factor) %>% 
  filter(complete.cases(.))

for(i in 1:length(rain_fct)){
  print(c(colnames(rain_fct)[i], length(unique(rain_fct[,i]))/nrow(rain_fct)*100,
          as.vector(sort(table(rain_fct[,i]), decreasing = TRUE)[1]/sort(table(rain_fct[,i]), decreasing = TRUE)[2])))
}

colnames(rain_fct)[nearZeroVar(rain_fct)]

## Transforming continuous variables 

rain_data %<>% select(-highcor)

pstand <- preProcess(rain_data %>% select_if(is.numeric), method = c('center', 'scale'))

rain_data %<>% as.data.frame(.)

nums <- unlist(lapply(rain_data, is.numeric))  

rain_data[, nums] <- predict(pstand, newdata = rain_data[, nums])

# Modelling

rain_data <- na.omit(rain_data)[,-1]

rain_data$rain_fct <- ifelse(rain_data$RainTomorrow == 'Yes', 1, 0) %>% factor

rain_data$RainTomorrow <- NULL

ind <- createDataPartition(rain_data$rain_fct, p = 0.8, list = FALSE)
rain_data_train <- rain_data[ind, ]
rain_data_test <- rain_data[-ind, ]

## logit model

model_glm <- train(rain_fct ~ ., data = rain_data_train, method = 'glm', family = 'binomial', trControl = trainControl(method = 'cv'))
model_glm
summary(model_glm$finalModel)
plot(varImp(model_glm, scale = FALSE))

## basic regression tree with recursive partition

rt.a1 <- rpart(rain_fct ~ ., data = rain_data_train)
rt.a1

prettyTree(rt.a1)

printcp(rt.a1) # checking how tree's criterion are changing

rtp.a1 <- rpart(rain_fct ~ ., data = rain_data_train, 
                control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.a1)
with(rtp.a1, {lines(cptable[,2]+1, cptable[,3],
                    type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.a1 <- prune(rtp.a1, cp = 0.0065) # we have minimal error at cp = 0.0065
rtp.a1
prettyTree(rtp.a1)

## training recursive partition tree with train() function

rt.a1.train <- train(rain_fct ~ ., data = rain_data_train, method = 'rpart', tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.a1.train
rtt.a1 <- rt.a1.train$finalModel

# Predicting

ModCrit <- function(pred, fact){
  cM <- table(Fact = fact, Predict = pred)
  accuracy <- (cM[1,1]+cM[2,2])/sum(cM)
  sensitivity <- cM[1,1]/(cM[1,1]+cM[2,1])
  specificity <- cM[2,2]/(cM[2,2]+cM[1,2])
  c(Accuracy=accuracy, Sensitivity=sensitivity, Specificity=specificity)
}

y <- rain_data_test$rain_fct
rain_data_testF <- as.data.frame(model.matrix(y ~ ., rain_data_test)[, -c(1,124)])

Result <- rbind(
  logit_model = ModCrit(predict(model_glm, rain_data_test), rain_data_test[,19]),
  rpart_full = ModCrit(predict(rt.a1, rain_data_test, type = 'class'), rain_data_test[,19]),
  rpart_prune = ModCrit(predict(rtp.a1, rain_data_test, type = 'class'), rain_data_test[,19]),
  rpart_train = ModCrit(predict(rt.a1.train$finalModel, rain_data_testF, type = 'class'), rain_data_test[,19])
)
Result #train logit-model is the best for predicting

# Analysis of the final model

rain_data_test$rain_pred <- predict(model_glm, rain_data_test, type = 'raw')

confusionMatrix(data = rain_data_test$rain_fct, reference = rain_data_test$rain_pred)

m_ROC.roc <- roc(rain_data_test$rain_fct, as.numeric(predict(model_glm, rain_data_test, type = 'prob')$`1`))
plot(m_ROC.roc, grid.col = c('green', 'red'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc), col = 'blue', add = TRUE, print.auc = FALSE)
