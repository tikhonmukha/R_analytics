library(tidyverse)
library(magrittr)
library(caret)
library(DMwR)
library(vegan)
library(rpart)
library(party)
library(randomForest)
library(gbm) 
library(bst)

# Importing data

student_math <- read.csv('student-mat.csv', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  mutate(across(c('Medu', 'Fedu', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health'), as.factor))

student_portuguese <- read.csv('student-por.csv', stringsAsFactors = TRUE) %>% 
  tibble() %>% 
  mutate(across(c('Medu', 'Fedu', 'famrel', 'freetime', 'goout', 'Dalc', 'Walc', 'health'), as.factor))

student_data <- student_math %>% 
  mutate("Course" = as.factor('math')) %>% 
  union(student_portuguese %>% mutate("Course" = as.factor('portuguese')))

summary(student_data)
anyNA(student_data)

# Exploratory data analysis

## Correlation between continuous variables

Mcor <- cor(student_data %>% select_if(is.numeric))
corrplot::corrplot(Mcor, method = 'color', addCoef.col = 'green', addgrid.col = 'gray33',
                   tl.col = 'black')

GGally::ggpairs(student_data %>% select_if(is.numeric))

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

student_num <- student_data %>% select_if(is.numeric) %>% as.data.frame()

top.mat(cor(student_num), level = 0.75, N = ncol(student_num))

highcor <- colnames(student_num[, findCorrelation(cor(student_num), cutoff = 0.75)]) #G1 and G2 are highly correlated

findLinearCombos(student_num)

for (i in 1:length(student_num)){
  print(names(which(car::vif(lm(student_num[, i] ~ ., data = student_num[,-i]))>5)))
}

apply(as.data.frame(student_num), 2, function(col) 
  car::vif(lm(student_num[, col] ~ ., data = student_num)))

car::vif(lm(as.data.frame(student_num)[, 1] ~ ., data = as.data.frame(student_num)[,-1]))

## Checking factor variables importance level

student_fct <- student_data %>% select_if(., is.factor) %>% as.data.frame()

for(i in 1:length(student_fct)){
  print(c(colnames(student_fct)[i], length(unique(student_fct[,i]))/nrow(student_fct)*100,
          as.vector(sort(table(student_fct[,i]), decreasing = TRUE)[1]/sort(table(student_fct[,i]), decreasing = TRUE)[2])))
}

colnames(student_fct)[nearZeroVar(student_fct)]

## decreasing variables amount

student_clean <- student_data %>% select(-G2) %>% as.data.frame()

## Checking distribution of continuous variables

ggplot(student_num, aes(x = age))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students age distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = traveltime))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students traveltime distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = studytime))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students weekly studytime distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = failures))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students failures amount distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = absences))+
  geom_density(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students absences amount distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = G1))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students first grade distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = G2))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students second grade distribution', x = '', y = 'Frequency')

ggplot(student_num, aes(x = G3))+
  geom_bar(alpha = 0.5, fill = 'red')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students third grade distribution', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = school))+
  geom_bar()+
  facet_wrap(~ school)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = school, y = G3, fill = school))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = sex))+
  geom_bar()+
  facet_wrap(~ sex)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by gender', x = '', y = 'Frequency')

ggplot(student_data, aes(x = sex, y = G3, fill = sex))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by gender', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = address))+
  geom_bar()+
  facet_wrap(~ address)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by address', x = '', y = 'Frequency')

ggplot(student_data, aes(x = address, y = G3, fill = address))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by address', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = famsize))+
  geom_bar()+
  facet_wrap(~ famsize)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by family size', x = '', y = 'Frequency')

ggplot(student_data, aes(x = famsize, y = G3, fill = famsize))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by family size', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Pstatus))+
  geom_bar()+
  facet_wrap(~ Pstatus)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by parents status', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Pstatus, y = G3, fill = Pstatus))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by parents status', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Medu))+
  geom_bar()+
  facet_wrap(~ Medu)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by mothers education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Medu, y = G3, fill = Medu))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by mothers education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Fedu))+
  geom_bar()+
  facet_wrap(~ Fedu)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by fathers education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Fedu, y = G3, fill = Fedu))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by fathers education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Mjob))+
  geom_bar()+
  facet_wrap(~ Mjob)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by mothers job', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Mjob, y = G3, fill = Mjob))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by mothers job', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Fjob))+
  geom_bar()+
  facet_wrap(~ Fjob)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by fathers job', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Mjob, y = G3, fill = Mjob))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by fathers job', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = reason))+
  geom_bar()+
  facet_wrap(~ reason)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by reason of school choosing', x = '', y = 'Frequency')

ggplot(student_data, aes(x = reason, y = G3, fill = reason))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by reason of school choosing', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = guardian))+
  geom_bar()+
  facet_wrap(~ guardian)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by guardian', x = '', y = 'Frequency')

ggplot(student_data, aes(x = guardian, y = G3, fill = guardian))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by guardian', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = schoolsup))+
  geom_bar()+
  facet_wrap(~ schoolsup)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having extra educational support', x = '', y = 'Frequency')

ggplot(student_data, aes(x = schoolsup, y = G3, fill = schoolsup))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having extra educational support', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = famsup))+
  geom_bar()+
  facet_wrap(~ famsup)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having family educational support', x = '', y = 'Frequency')

ggplot(student_data, aes(x = famsup, y = G3, fill = famsup))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having family educational support', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = paid))+
  geom_bar()+
  facet_wrap(~ paid)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having paid courses', x = '', y = 'Frequency')

ggplot(student_data, aes(x = paid, y = G3, fill = paid))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having paid courses', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = activities))+
  geom_bar()+
  facet_wrap(~ activities)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having extra-curricular activities', x = '', y = 'Frequency')

ggplot(student_data, aes(x = activities, y = G3, fill = activities))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having extra-curricular activities', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = nursery))+
  geom_bar()+
  facet_wrap(~ nursery)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by nursery school attendance', x = '', y = 'Frequency')

ggplot(student_data, aes(x = nursery, y = G3, fill = nursery))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by nursery school attendance', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = higher))+
  geom_bar()+
  facet_wrap(~ higher)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by planning to have a higher education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = higher, y = G3, fill = higher))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by planning to have a higher education', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = internet))+
  geom_bar()+
  facet_wrap(~ internet)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having Internet access at home', x = '', y = 'Frequency')

ggplot(student_data, aes(x = internet, y = G3, fill = internet))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having Internet access at home', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = romantic))+
  geom_bar()+
  facet_wrap(~ romantic)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by having romantic relationships', x = '', y = 'Frequency')

ggplot(student_data, aes(x = romantic, y = G3, fill = romantic))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by having romantic relationships', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = famrel))+
  geom_bar()+
  facet_wrap(~ famrel)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by family relationships', x = '', y = 'Frequency')

ggplot(student_data, aes(x = famrel, y = G3, fill = famrel))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by family relationships', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = freetime))+
  geom_bar()+
  facet_wrap(~ freetime)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by free time after school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = freetime, y = G3, fill = freetime))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by free time after school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = goout))+
  geom_bar()+
  facet_wrap(~ goout)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by going out after school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = goout, y = G3, fill = goout))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by going out after school', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Dalc))+
  geom_bar()+
  facet_wrap(~ Dalc)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by workday alcohol consumption', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Dalc, y = G3, fill = Dalc))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by workday alcohol consumption', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Walc))+
  geom_bar()+
  facet_wrap(~ Walc)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by weekend alcohol consumption', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Walc, y = G3, fill = Walc))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by weekend alcohol consumption', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = health))+
  geom_bar()+
  facet_wrap(~ health)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by health', x = '', y = 'Frequency')

ggplot(student_data, aes(x = health, y = G3, fill = health))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by health', x = '', y = 'Frequency')

ggplot(student_data, aes(x = G3, fill = Course))+
  geom_bar()+
  facet_wrap(~ Course)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade distribution by course', x = '', y = 'Frequency')

ggplot(student_data, aes(x = Course, y = G3, fill = Course))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Students third grade boxplot by course', x = '', y = 'Frequency')

## Distribution of factor variables analysis

ggplot(student_fct, aes(x = school))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students school distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = sex))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students gender distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = address))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students address distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = famsize))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students family size distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Pstatus))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students parents cohabitation status distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Medu))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students mothers education distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Fedu))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students fathers education distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Mjob))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students mothers job distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Fjob))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students fathers job distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = reason))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students reasons to choose the school distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = guardian))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students guardian distribution', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = schoolsup))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have extra educational support?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = famsup))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have family educational support?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = paid))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have extra paid classes?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = activities))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have extra-curricular activities?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = nursery))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student attend nursery school?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = higher))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student want to take higher education?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = internet))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have the Internet access at home?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = romantic))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Does the student have a romantic relationship?', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = famrel))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students quality of family relationships', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = freetime))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students free time after school', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = goout))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students going out with friends', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Dalc))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students workday alcohol consumption', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Walc))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students weekend alcohol consumption', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = health))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students current health status', x = '', y = 'Frequency')

ggplot(student_fct, aes(x = Course))+
  geom_bar(alpha = 0.5, fill = 'blue')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = 'Students courses distribution', x = '', y = 'Frequency')

## Testing numeric variables for distribution normality

for (i in 1:ncol(student_num)){
  print(c(colnames(student_num[i]), nortest::ad.test(student_num[, i])$p.value))
}

## Testing numeric variables for outliers

y <- apply(mobile_num,2,function(col) outliers::grubbs.test(col, type=10))

for (i in 1:ncol(student_num)){
  print(c(colnames(student_num[i]), outliers::grubbs.test(student_num[, i], type = 10)$p.value))
} #age, failures and absences have outliers

# Modelling

## Transforming continuous variables 

pstand <- preProcess(student_clean[, colnames(student_num[, -c(7,8)])], method = c('center', 'scale'))

student_clean[, colnames(student_num[, -c(7,8)])] <- predict(pstand, newdata = student_clean[, colnames(student_num[, -c(7,8)])])

## Splitting data into train and test

ind <- createDataPartition(y = student_clean$G3, p = 0.8, list = FALSE)
student_train <- student_clean[ind, ]
student_test <- student_clean[-ind, ]

## standard linear model

lm.grade <- glm(G3 ~ ., family = poisson, data = student_train)
summary(lm.grade)
par(mfrow = c(2,2))
plot(lm.grade)

## step predictors elimination

lm_step.grade <- step(lm.grade, trace = 0)
summary(lm_step.grade)
anova(lm.grade, lm_step.grade) # step model is not statistically significant

## training glm() with train()

lm.grade.cv <- train(G3 ~ ., data = student_train, method = 'glm', 
                     family = 'poisson', trControl = trainControl(method = 'cv'))

summary(lm.grade.cv$finalModel) # same results as standard glm()

## lasso regression

x <- model.matrix(G3 ~ ., data = student_train)[, -1]
grid <- 10^seq(10,-2,length=100)
library(glmnet)
lasso.grade <- glmnet(x, student_train$G3, alpha = 1, lambda = grid)
par(mfrow = c(1,1))
plot(lasso.grade, xvar = 'lambda', label = TRUE, lwd = 2)

## optimizing lambda with train()

grid.train <- seq(-5, 0, length = 15)
lasso.grade.train <- train(as.data.frame(x), student_train$G3, method = 'glmnet',
                            tuneGrid = expand.grid(.lambda = grid.train, .alpha = 1),
                            trControl = trainControl(method = 'cv'))
lasso.grade.train
coef(lasso.grade.train$finalModel, lasso.grade.train$bestTune$lambda)

## private least squares model

library(pls)
M.pls <- plsr(student_train$G3 ~ x, scale = TRUE, Validation = 'CV', method = 'oscorespls')
summary(M.pls)

## optimizing components number with train()

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10)
plsTune.grade <- train(x, student_train$G3, method = 'pls', tuneLength = 14, trControl = ctrl)
plsTune.grade
plot(plsTune.grade)

## principal components regression

pcrTune.grade <- train(x, student_train$G3, method = 'pcr', tuneLength = 14,
                        trControl = ctrl)
pcrTune.grade
plot(pcrTune.grade)

## kNN regression

knnTune.grade <- train(x, student_train$G3, method = 'knn',
                        trControl = ctrl, tuneGrid = data.frame(.k = 10:35))
knnTune.grade
plot(knnTune.grade)

## recursive partition tree

rt.grade <- rpart(G3 ~ ., data = student_train)
rt.grade
prettyTree(rt.grade)

printcp(rt.grade) # checking how tree's criterion are changing

rtp.grade <- rpart(G3 ~ ., data = student_train, 
                    control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.grade)
with(rtp.grade, {lines(cptable[,2]+1, cptable[,3],
                        type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.grade <- prune(rtp.grade, cp = 0.014) # we have minimal error at cp = 0.014
rtp.grade
prettyTree(rtp.grade)

## training recursive partition tree with train() function

rt.grade.train <- train(G3 ~ ., data = student_train, method = 'rpart', 
                        tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.grade.train
plot(rt.grade.train)
prettyTree(rt.grade.train$finalModel)

## basic regression tree with conditional inference

ctree.grade <- ctree(G3 ~ ., data = student_train)
ctree.grade
plot(ctree.grade)

## optimizing mincriterion with train()

ctree.grade.train <- train(G3 ~ ., data = student_train, method = 'ctree', 
                           tuneLength = 10, trControl = trainControl(method = 'cv'))
ctree.grade.train
plot(ctree.grade.train)
plot(ctree.grade.train$finalModel)

## bagging tree 

bag.grade <- randomForest(x, student_train$G3, mtry = ncol(x))
bag.grade

## optimizing bagging model with train()
bag.grade.train <- train(x, student_train$G3,
                          method = 'rf', trControl = trainControl(method = 'cv'),
                          tuneGrid = expand.grid(.mtry = ncol(x)))
bag.grade.train

## random forest model and mtry parameter optimizing with train()

ranfor.grade.train <- train(x, student_train$G3,
                             method = 'rf', trControl = trainControl(method = 'cv'),
                             tuneGrid = expand.grid(.mtry = 60:70), importance = TRUE)
ranfor.grade.train
varImpPlot(ranfor.grade.train$finalModel) # predictors importance
# let's see the trees number when model error becomes constant
plot(ranfor.grade.train$finalModel, col='blue', lwd=2, main = '')
plot(bag.grade.train$finalModel, col='green', lwd=2, add=TRUE)
legend('topright', c('Bagging', 'RandomForest'), col = c('green','blue'), lwd = 2)

## boosting

boost.grade <- gbm(G3 ~ ., data = student_train, distribution = 'gaussian',
                    interaction.depth = 3, n.trees = 1000)
summary(boost.grade)

## optimizing gbm() parameters with train()

gbmFit.grade <- train(G3 ~ ., data = student_train, method = 'gbm', trControl = trainControl(method = 'cv'),
                       tuneGrid = expand.grid(.shrinkage = c(0.05, 0.025, 0.01, 0.005, 0.001), .interaction.depth = 2:10, .n.trees = 50, .n.minobsinnode = 10),
                       verbose = FALSE)
gbmFit.grade
plot(gbmFit.grade)

## boosting with bstTree() function

boostFit.grade <- train(G3 ~ ., data = student_train, method = 'bstTree', trControl = trainControl(method = 'cv'))
boostFit.grade
plot(boostFit.grade)

# comparing all models

ModCrit <- function(pred, fact){
  mae <- mean(abs(pred-fact))
  rmse <- sqrt(mean((pred-fact)^2))
  Rsq <- 1-sum((fact-pred)^2)/sum((mean(fact)-fact)^2)
  c(MAE=mae, RMSE=rmse, Rsq=Rsq)
}

x <- as.data.frame(x)

Result_fact <- rbind(
  lm = ModCrit(predict(lm.grade, student_train), student_train$G3),
  lasso_train = ModCrit(predict(lasso.grade.train, as.matrix(x)), student_train$G3),
  pls = ModCrit(predict(plsTune.grade, x), student_train$G3),
  pcr = ModCrit(predict(pcrTune.grade, x), student_train$G3),
  kNN = ModCrit(predict(knnTune.grade, x), student_train$G3),
  rpart_prune = ModCrit(predict(rtp.grade, student_train), student_train$G3),
  rpart_train = ModCrit(predict(rt.grade.train$finalModel, x), student_train$G3),
  ctree = ModCrit(predict(ctree.grade, student_train), student_train$G3),
  ctree_train = ModCrit(predict(ctree.grade.train$finalModel, x), student_train$G3),
  bagging = ModCrit(predict(bag.grade, x), student_train$G3),
  bagging_train = ModCrit(predict(bag.grade.train$finalModel, x), student_train$G3),
  randomforest = ModCrit(predict(ranfor.grade.train$finalModel, x), student_train$G3),
  boosting = ModCrit(predict(boost.grade, student_train), student_train$G3),
  boost_train_gbm = ModCrit(predict(gbmFit.grade$finalModel, x), student_train$G3),
  boost_train_bst = ModCrit(predict(boostFit.grade$finalModel, x), student_train$G3)
)

Result_fact <- as.data.frame(Result_fact)
Result_fact[order(Result_fact$RMSE),]

x_test <- as.data.frame(model.matrix(G3 ~ ., student_test)[, -1])

Result_test <- rbind(
  lm = ModCrit(predict(lm.grade, student_test), student_test$G3),
  lasso_train = ModCrit(predict(lasso.grade.train, as.matrix(x_test)), student_test$G3),
  pls = ModCrit(predict(plsTune.grade, x_test), student_test$G3),
  pcr = ModCrit(predict(pcrTune.grade, x_test), student_test$G3),
  kNN = ModCrit(predict(knnTune.grade, x_test), student_test$G3),
  rpart_prune = ModCrit(predict(rtp.grade, student_test), student_test$G3),
  rpart_train = ModCrit(predict(rt.grade.train$finalModel, x_test), student_test$G3),
  ctree = ModCrit(predict(ctree.grade, student_test), student_test$G3),
  ctree_train = ModCrit(predict(ctree.grade.train$finalModel, x_test), student_test$G3),
  bagging = ModCrit(predict(bag.grade, x_test), student_test$G3),
  bagging_train = ModCrit(predict(bag.grade.train$finalModel, x_test), student_test$G3),
  randomforest = ModCrit(predict(ranfor.grade.train$finalModel, x_test), student_test$G3),
  boosting = ModCrit(predict(boost.grade, student_test), student_test$G3),
  boost_train_gbm = ModCrit(predict(gbmFit.grade$finalModel, x_test), student_test$G3),
  boost_train_bst = ModCrit(predict(boostFit.grade$finalModel, x_test), student_test$G3)
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
  scale_x_continuous(breaks = seq(-6, 1, by = 0.5))+
  scale_y_continuous(breaks = seq(-6, 1, by = 0.5), limits = c(-6, 1))+
  theme_minimal()+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))+
  labs(title = 'Models accuracy on train and test data', x = 'Rsquared on train data', y = 'Rsquared on test data')

# the results on test dataset aren't good so we can resolve the task with a new rule: let's take the average of G1, G2 and G3 as a target variable

student_y <- student_data %>% 
  mutate(grade = (G1+G2+G3)/3) %>% 
  select(-c('G1','G2','G3'))

# Modelling

## Transforming continuous variables 

pstand <- preProcess(student_y[, colnames(student_num[, -c(6:8)])], method = c('center', 'scale'))

student_y[, colnames(student_num[, -c(6:8)])] <- predict(pstand, newdata = student_y[, colnames(student_num[, -c(6:8)])])

student_y %<>% as.data.frame

## Splitting data into train and test

ind <- createDataPartition(y = student_y$grade, p = 0.8, list = FALSE)
student_y_train <- student_y[ind, ]
student_y_test <- student_y[-ind, ]

## standard linear model

lm.y <- lm(grade ~ ., data = student_y_train)
summary(lm.y)
par(mfrow = c(2,2))
plot(lm.y)

## step predictors elimination

lm_step.y <- step(lm.y, trace = 0)
summary(lm_step.y)
anova(lm.y, lm_step.y) # step model is not statistically significant

## training lm() with train()

lm.y.cv <- train(grade ~ ., data = student_y_train, method = 'lm', 
                     trControl = trainControl(method = 'cv'))

lm.y.cv
summary(lm.y.cv$finalModel) # same results as standard glm()

## lasso regression

x_y <- model.matrix(grade ~ ., data = student_y_train)[, -1]
grid <- 10^seq(10,-2,length=100)
library(glmnet)
lasso.y <- glmnet(x_y, student_y_train$grade, alpha = 1, lambda = grid)
par(mfrow = c(1,1))
plot(lasso.y, xvar = 'lambda', label = TRUE, lwd = 2)

## optimizing lambda with train()

grid.train <- seq(-5, 0, length = 15)
lasso.y.train <- train(as.data.frame(x_y), student_y_train$grade, method = 'glmnet',
                           tuneGrid = expand.grid(.lambda = grid.train, .alpha = 1),
                           trControl = trainControl(method = 'cv'))
lasso.y.train
coef(lasso.y.train$finalModel, lasso.y.train$bestTune$lambda)

## private least squares model

library(pls)
M.pls_y <- plsr(student_y_train$grade ~ x_y, scale = TRUE, Validation = 'CV', method = 'oscorespls')
summary(M.pls_y)

## optimizing components number with train()

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10)
plsTune.y <- train(x_y, student_y_train$grade, method = 'pls', tuneLength = 14, trControl = ctrl)
plsTune.y
plot(plsTune.y)

## principal components regression

pcrTune.y <- train(x_y, student_y_train$grade, method = 'pcr', tuneLength = 14,
                       trControl = ctrl)
pcrTune.y
plot(pcrTune.y)

## kNN regression

knnTune.y <- train(x_y, student_y_train$grade, method = 'knn',
                       trControl = ctrl, tuneGrid = data.frame(.k = 10:35))
knnTune.y
plot(knnTune.y)

## recursive partition tree

rt.y <- rpart(grade ~ ., data = student_y_train)
rt.y
prettyTree(rt.y)

printcp(rt.y) # checking how tree's criterion are changing

rtp.y <- rpart(grade ~ ., data = student_y_train, 
                   control = rpart.control(cp = .005)) # let's decrease difficulty penalty

plotcp(rtp.y)
with(rtp.y, {lines(cptable[,2]+1, cptable[,3],
                       type = 'b', col = 'red')
  legend('topright', c('Training error',
                       'CV-error',
                       'min(CV-error)+SE'),
         lty = c(1,1,2), col = c('red', 'black', 'black'), bty = 'n')})

rtp.y <- prune(rtp.y, cp = 0.015) # we have minimal error at cp = 0.015
rtp.y
prettyTree(rtp.y)

## training recursive partition tree with train() function

rt.y.train <- train(grade ~ ., data = student_y_train, method = 'rpart', 
                        tuneLength = 30, trControl = trainControl(method = 'cv'))
rt.y.train
plot(rt.y.train)
prettyTree(rt.y.train$finalModel)

## basic regression tree with conditional inference

ctree.y <- ctree(grade ~ ., data = student_y_train)
ctree.y
plot(ctree.y)

## optimizing mincriterion with train()

ctree.y.train <- train(grade ~ ., data = student_y_train, method = 'ctree', 
                           tuneLength = 10, trControl = trainControl(method = 'cv'))
ctree.y.train
plot(ctree.y.train)
plot(ctree.y.train$finalModel)

## bagging tree 

bag.y <- randomForest(x_y, student_y_train$grade, mtry = ncol(x_y))
bag.y

## optimizing bagging model with train()
bag.y.train <- train(x_y, student_y_train$grade,
                         method = 'rf', trControl = trainControl(method = 'cv'),
                         tuneGrid = expand.grid(.mtry = ncol(x_y)))
bag.y.train

## random forest model and mtry parameter optimizing with train()

ranfor.y.train <- train(x_y, student_y_train$grade,
                            method = 'rf', trControl = trainControl(method = 'cv'),
                            tuneGrid = expand.grid(.mtry = 60:70), importance = TRUE)
ranfor.y.train
varImpPlot(ranfor.y.train$finalModel) # predictors importance
# let's see the trees number when model error becomes constant
plot(ranfor.y.train$finalModel, col='blue', lwd=2, main = '')
plot(bag.y.train$finalModel, col='green', lwd=2, add=TRUE)
legend('topright', c('Bagging', 'RandomForest'), col = c('green','blue'), lwd = 2)

## boosting

boost.y <- gbm(grade ~ ., data = student_y_train, distribution = 'gaussian',
                   interaction.depth = 3, n.trees = 1000)
summary(boost.y)

## optimizing gbm() parameters with train()

gbmFit.y <- train(grade ~ ., data = student_y_train, method = 'gbm', trControl = trainControl(method = 'cv'),
                      tuneGrid = expand.grid(.shrinkage = c(0.1, 0.075, 0.05), .interaction.depth = 2:10, .n.trees = 50, .n.minobsinnode = 10),
                      verbose = FALSE)
gbmFit.y
plot(gbmFit.y)

## boosting with bstTree() function

boostFit.y <- train(grade ~ ., data = student_y_train, method = 'bstTree', trControl = trainControl(method = 'cv'))
boostFit.y
plot(boostFit.y)

# comparing all models

x_y <- as.data.frame(x_y)

Result_fact_y <- rbind(
  lm = ModCrit(predict(lm.y, student_y_train), student_y_train$grade),
  lasso_train = ModCrit(predict(lasso.y.train, as.matrix(x_y)), student_y_train$grade),
  pls = ModCrit(predict(plsTune.y, x_y), student_y_train$grade),
  pcr = ModCrit(predict(pcrTune.y, x_y), student_y_train$grade),
  kNN = ModCrit(predict(knnTune.y, x_y), student_y_train$grade),
  rpart_prune = ModCrit(predict(rtp.y, student_y_train), student_y_train$grade),
  rpart_train = ModCrit(predict(rt.y.train$finalModel, x_y), student_y_train$grade),
  ctree = ModCrit(predict(ctree.y, student_y_train), student_y_train$grade),
  ctree_train = ModCrit(predict(ctree.y.train$finalModel, x_y), student_y_train$grade),
  bagging = ModCrit(predict(bag.y, x_y), student_y_train$grade),
  bagging_train = ModCrit(predict(bag.y.train$finalModel, x_y), student_y_train$grade),
  randomforest = ModCrit(predict(ranfor.y.train$finalModel, x_y), student_y_train$grade),
  boosting = ModCrit(predict(boost.y, student_y_train), student_y_train$grade),
  boost_train_gbm = ModCrit(predict(gbmFit.y$finalModel, x_y), student_y_train$grade),
  boost_train_bst = ModCrit(predict(boostFit.y$finalModel, x_y), student_y_train$grade)
)

Result_fact_y <- as.data.frame(Result_fact_y)
Result_fact_y[order(Result_fact_y$RMSE),]

x_y_test <- as.data.frame(model.matrix(grade ~ ., student_y_test)[, -1])

Result_test_y <- rbind(
  lm = ModCrit(predict(lm.y, student_y_test), student_y_test$grade),
  lasso_train = ModCrit(predict(lasso.y.train, as.matrix(x_y_test)), student_y_test$grade),
  pls = ModCrit(predict(plsTune.y, x_y_test), student_y_test$grade),
  pcr = ModCrit(predict(pcrTune.y, x_y_test), student_y_test$grade),
  kNN = ModCrit(predict(knnTune.y, x_y_test), student_y_test$grade),
  rpart_prune = ModCrit(predict(rtp.y, student_y_test), student_y_test$grade),
  rpart_train = ModCrit(predict(rt.y.train$finalModel, x_y_test), student_y_test$grade),
  ctree = ModCrit(predict(ctree.y, student_y_test), student_y_test$grade),
  ctree_train = ModCrit(predict(ctree.y.train$finalModel, x_y_test), student_y_test$grade),
  bagging = ModCrit(predict(bag.y, x_y_test), student_y_test$grade),
  bagging_train = ModCrit(predict(bag.y.train$finalModel, x_y_test), student_y_test$grade),
  randomforest = ModCrit(predict(ranfor.y.train$finalModel, x_y_test), student_y_test$grade),
  boosting = ModCrit(predict(boost.y, student_y_test), student_y_test$grade),
  boost_train_gbm = ModCrit(predict(gbmFit.y$finalModel, x_y_test), student_y_test$grade),
  boost_train_bst = ModCrit(predict(boostFit.y$finalModel, x_y_test), student_y_test$grade)
)

Result_test_y

Result_test_y <- as.data.frame(Result_test_y)
Result_test_y[order(Result_test_y$RMSE),]

Result_compare_y <- Result_fact_y %>% 
  mutate(model = rownames(.) %>% factor) %>% 
  tibble %>% 
  rename('Rsq_fact' = 'Rsq') %>% 
  left_join(., Result_test_y %>% mutate(model = rownames(.) %>% factor) %>% tibble, by = 'model') %>% 
  select(model, Rsq_fact, Rsq) %>% 
  rename('Rsq_pred' = 'Rsq')

ggplot(data = Result_compare_y, aes(x = Rsq_fact, y = Rsq_pred, color = model))+
  geom_point(size = 3)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1))+
  theme_minimal()+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))+
  labs(title = 'Models accuracy on train and test data', x = 'Rsquared on train data', y = 'Rsquared on test data')

# randomforest() model was the best on test data in 2 cases so i chose it for the forward research

## deleting outliers

Q <- quantile(student_data$absences, probs = c(0.25, 0.75), na.rm = FALSE)
iqr <- IQR(student_data$absences)
up <- Q[2]+1.5*iqr
low <- Q[1]-1.5*iqr
eliminated <- subset(student_data, student_data$absences > low & student_data$absences < up)

pstand <- preProcess(eliminated[, colnames(student_num[, -c(6:8)])], method = c('center', 'scale'))

eliminated[, colnames(student_num[, -c(6:8)])] <- predict(pstand, newdata = eliminated[, colnames(student_num[, -c(6:8)])])

eliminated %<>% select(-'G2') %>%  as.data.frame

ind <- createDataPartition(y = eliminated$G3, p = 0.8, list = FALSE)
eliminated_train <- eliminated[ind, ]
eliminated_test <- eliminated[-ind, ]

x_elim <- model.matrix(G3 ~ ., data = eliminated_train)[, -1]

ranfor.elim.train <- train(x_elim, eliminated_train$G3,
                        method = 'rf', trControl = trainControl(method = 'cv'),
                        tuneGrid = expand.grid(.mtry = 60:70), importance = TRUE)
ranfor.elim.train
varImpPlot(ranfor.elim.train$finalModel) # predictors importance

ModCrit(predict(ranfor.elim.train$finalModel, x_elim), eliminated_train$G3)
x_elim_test <- as.data.frame(model.matrix(G3 ~ ., eliminated_test)[, -1])
ModCrit(predict(ranfor.elim.train$finalModel, x_elim_test), eliminated_test$G3)

## selecting the most important variables

ctrl <- rfeControl(functions = rfFuncs, method = 'cv', verbose = FALSE, returnResamp = 'final')
rfe_G3 <- rfe(as.data.frame(x_elim), eliminated_train$G3, sizes = 1:ncol(eliminated_train), rfeControl = ctrl)
rfe_G3$optVariables

ranfor.elim.opt.train <- train(x_elim[, rfe_G3$optVariables], eliminated_train$G3,
                           method = 'rf', trControl = trainControl(method = 'cv'),
                           tuneGrid = expand.grid(.mtry = 60:70), importance = TRUE)
ranfor.elim.opt.train
varImpPlot(ranfor.elim.opt.train$finalModel) # predictors importance

x_elim_opt <- model.matrix(G3 ~ ., data = eliminated_train)[, -1][, rfe_G3$optVariables]
ModCrit(predict(ranfor.elim.opt.train$finalModel, x_elim_opt), eliminated_train$G3)
x_elim_opt_test <- as.data.frame(model.matrix(G3 ~ ., eliminated_test)[, -1][, rfe_G3$optVariables])
ModCrit(predict(ranfor.elim.opt.train$finalModel, x_elim_opt_test), eliminated_test$G3)

# comparing random forest models on test data

rbind(
  ranfor_all_var_G3 = ModCrit(predict(ranfor.grade.train$finalModel, x_test), student_test$G3),
  ranfor_all_var_avg_grade = ModCrit(predict(ranfor.y.train$finalModel, x_y_test), student_y_test$grade),
  ranfor_all_var_outliers_G3 = ModCrit(predict(ranfor.elim.train$finalModel, x_elim_test), eliminated_test$G3),
  ranfor_imp_var = ModCrit(predict(ranfor.elim.opt.train$finalModel, x_elim_opt_test), eliminated_test$G3)
)
