library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(DMwR)
library(caret)
library(lmtest)
library(nortest)
library(pROC)

# Importing data

hr_train <- read.csv('aug_train.csv', na.strings = '', stringsAsFactors = TRUE) %>% 
  tibble %>% 
  mutate(target = as.factor(target))

hr_test <- read.csv('aug_test.csv', na.strings = '', stringsAsFactors = TRUE) %>% 
  tibble

head(hr_train)
tail(hr_train)
glimpse(hr_train)
summary(hr_train)

# Exploratory data analysis

## Correlation between continuous variables

GGally::ggpairs(hr_train %>% select(city_development_index, training_hours))

## Distribution of continuous variables analysis

ggplot(data = hr_train, aes(x = city_development_index))+
  geom_density(fill = 'red', alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'City development index distribution', x = '', y = 'Frequency')

ggplot(data = hr_train, aes(x = training_hours))+
  geom_density(fill = 'blue', alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Completed training hours distribution', x = '', y = 'Frequency')

## Continuous variables linearity

ggplot(data = hr_train, aes(x = training_hours, y = city_development_index))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_minimal()

## Testing continuous variables distribution normality

x <- apply(select_if(hr_train, is.numeric) %>% 
             select(-'enrollee_id'), 2, ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

## Testing continuous variables significance

y <- apply(select_if(hr_train, is.numeric) %>% 
             select(-'enrollee_id'), 2, function(col) wilcox.test(col ~ hr_train$target))

for (i in 1:length(y)){
  print(y[[i]]$p.value)
} #all variables are significant

## Testing significance of continuous variables means in factor groups

hr_fct <- hr_train %>% 
  select_if(is.factor) %>% 
  select(-c('city', 'target')) %>% 
  filter(complete.cases(.))

for (i in 1:length(hr_fct)){
  print(car::leveneTest(hr_train[complete.cases(hr_train),]$city_development_index ~ pull(hr_fct[, i]))$`Pr(>F)`[1])
}

for (i in 1:length(hr_fct)){
  print(kruskal.test(hr_train[complete.cases(hr_train),]$city_development_index ~ pull(hr_fct[, i]))$p.value)
}

for (i in 1:length(hr_fct)){
  print(car::leveneTest(hr_train[complete.cases(hr_train),]$training_hours ~ pull(hr_fct[, i]))$`Pr(>F)`[1])
}

for (i in 1:length(hr_fct)){
  print(summary(aov(hr_train[complete.cases(hr_train),]$city_development_index ~ pull(hr_fct[, i])))[[1]][["Pr(>F)"]][1])
}

## Distribution of factor variables analysis

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(gender) %>% 
         group_by(gender) %>% 
         count(), aes(x = reorder(gender, -n), y = n))+
  geom_col(aes(fill = gender))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Gender distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(relevent_experience) %>% 
         group_by(relevent_experience) %>% 
         count(), aes(x = reorder(relevent_experience, -n), y = n))+
  geom_col(aes(fill = relevent_experience))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Experience distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(enrolled_university) %>% 
         group_by(enrolled_university) %>% 
         count(), aes(x = reorder(enrolled_university, -n), y = n))+
  geom_col(aes(fill = enrolled_university))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Enrolled university distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(education_level) %>% 
         group_by(education_level) %>% 
         count(), aes(x = reorder(education_level, -n), y = n))+
  geom_col(aes(fill = education_level))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Education level distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(major_discipline) %>% 
         group_by(major_discipline) %>% 
         count(), aes(x = reorder(major_discipline, -n), y = n))+
  geom_col(aes(fill = major_discipline))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Major discipline distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(major_discipline) %>% 
         group_by(major_discipline) %>% 
         count(), aes(x = reorder(major_discipline, -n), y = n))+
  geom_col(aes(fill = major_discipline))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Major discipline distribution', x = '', y = '')

x.values = c("<1","1","2",'3','4','5','6','7','8','9','10','11','12',
             '13','14','15','16','17','18','19','20','>20')

exper <- hr_train %>% 
  filter(complete.cases(.)) %>% 
  select(experience) %>% 
  group_by(experience) %>%
  count()

exper <- exper[order(factor(exper$experience, levels = x.values)),]
num <- seq(1, nrow(exper), by = 1)
exper <- cbind(exper, num)

ggplot(exper, aes(x = reorder(experience, ...3), y = n))+
  geom_col(aes(fill = experience))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Experience distribution', x = '', y = '')

x1.values = c('<10','10/49','50-99','100-500','500-999','1000-4999',
              '5000-9999','10000+')

comp_size <- hr_train %>% 
  filter(complete.cases(.)) %>% 
  select(company_size) %>% 
  group_by(company_size) %>%
  count()

comp_size <- comp_size[order(factor(comp_size$company_size, levels = x1.values)),]
num1 <- seq(1, nrow(comp_size), by = 1)
comp_size <- cbind(comp_size, num1)

ggplot(comp_size, aes(x = reorder(company_size, ...3), y = n))+
  geom_col(aes(fill = company_size))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Company size distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(company_type) %>% 
         group_by(company_type) %>% 
         count(), aes(x = reorder(company_type, -n), y = n))+
  geom_col(aes(fill = company_type))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Company type distribution', x = '', y = '')

x2.values = c('never','1','2','3','4','>4')

last_job <- hr_train %>% 
  filter(complete.cases(.)) %>% 
  select(last_new_job) %>% 
  group_by(last_new_job) %>%
  count()

last_job <- last_job[order(factor(last_job$last_new_job, levels = x2.values)),]
num2 <- seq(1, nrow(last_job), by = 1)
last_job <- cbind(last_job, num2)

ggplot(last_job, aes(x = reorder(last_new_job, ...3), y = n))+
  geom_col(aes(fill = last_new_job))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Last job distribution', x = '', y = '')

ggplot(hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(target) %>% 
         group_by(target) %>% 
         count(), aes(x = reorder(target, -n), y = n))+
  geom_col(aes(fill = target))+
  scale_fill_brewer(palette = 'Pastel2')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Target distribution', x = '', y = '')

## Looking for differences of factor variables distribution by target

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(gender, target) %>% 
         group_by(gender, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(gender,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Gender difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(relevent_experience, target) %>% 
         group_by(relevent_experience, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(relevent_experience,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Relevant experience difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(enrolled_university, target) %>% 
         group_by(enrolled_university, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(enrolled_university,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Enrolled university difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(education_level, target) %>% 
         group_by(education_level, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(education_level,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Education level difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(major_discipline, target) %>% 
         group_by(major_discipline, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(major_discipline,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Major discipline difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(experience, target) %>% 
         group_by(experience, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(experience,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Experience difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(company_size, target) %>% 
         group_by(company_size, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(company_size,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Company size difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(company_type, target) %>% 
         group_by(company_type, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(company_type,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Company type difference', x = '', y = 'Share')

ggplot(data = hr_train %>% 
         filter(complete.cases(.)) %>% 
         select(last_new_job, target) %>% 
         group_by(last_new_job, target) %>% 
         count() %>% arrange(target, desc(n)), aes(x = target, y = n, fill = reorder(last_new_job,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Last job difference', x = '', y = 'Share')

## Checking factor variables variance level

for(i in 1:length(hr_fct)){
  print(c(colnames(hr_fct)[i], length(unique(hr_fct[,i]))/nrow(hr_fct)*100,
          as.vector(sort(table(hr_fct[,i]), decreasing = TRUE)[1]/sort(table(hr_fct[,i]), decreasing = TRUE)[2])))
}

colnames(hr_fct)[nearZeroVar(hr_fct)]

# Filling NA values

hr_train %<>% mutate(gender = forcats::fct_explicit_na(gender, na_level = "None"),
                    enrolled_university = forcats::fct_explicit_na(enrolled_university, na_level = "None"),
                    education_level = forcats::fct_explicit_na(education_level, na_level = "None"),
                    experience = forcats::fct_explicit_na(experience, na_level = "None"),
                    company_size = forcats::fct_explicit_na(company_size, na_level = "None"),
                    company_type = forcats::fct_explicit_na(company_type, na_level = "None"),
                    last_new_job = forcats::fct_explicit_na(last_new_job, na_level = "None"))

# Transforming numeric predictors and selecting variables for the model

pP <- preProcess(hr_train[,c(3,13)], method = c('center', 'scale'))
pP

hr_model <- hr_train[,c(3:7,9:14)]
hr_model[,c(1,10)] <- predict(pP, hr_train[,c(3,13)])

# Training the model

ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10)

model_glm <- train(target ~., data = hr_model, method = 'glm', family = 'binomial', trControl = ctrl)
model_glm
summary(model_glm)
plot(varImp(model_glm, scale = FALSE))

hr_model$prob <- predict(model_glm, newdata = hr_model, type = 'prob')
hr_model$pred <- factor(ifelse(hr_model$prob$`1` >= 0.5, '1', '0'))
confusionMatrix(data = hr_model$pred, reference = hr_model$target)

m_ROC.roc <- roc(hr_model$target, as.numeric(hr_model$prob$`1`))
plot(m_ROC.roc, grid.col = c('green', 'red'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc), col = 'blue', add = TRUE, print.auc = FALSE)

# Using the model for target classification in test dataset

hr_test %<>% mutate(gender = forcats::fct_explicit_na(gender, na_level = "None"),
                     enrolled_university = forcats::fct_explicit_na(enrolled_university, na_level = "None"),
                     education_level = forcats::fct_explicit_na(education_level, na_level = "None"),
                     experience = forcats::fct_explicit_na(experience, na_level = "None"),
                     company_size = forcats::fct_explicit_na(company_size, na_level = "None"),
                     company_type = forcats::fct_explicit_na(company_type, na_level = "None"),
                     last_new_job = forcats::fct_explicit_na(last_new_job, na_level = "None"))
pP_test <- preProcess(hr_test[,c(3,13)], method = c('center', 'scale'))
hr_test[,c(3,13)] <- predict(pP_test, hr_test[,c(3,13)])

hr_test$target_pred <- predict(model_glm, hr_test, type = 'raw')
table(hr_test$target_pred)
