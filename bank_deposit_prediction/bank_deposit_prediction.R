library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(AUC)
library(erer)

deposit_train <- tibble(read.csv("bank-additional-full.csv", header = T,
                                 sep = ";", dec = ".", stringsAsFactors = T)) %>% 
  slice(1:35000) %>% 
  mutate(y_num = factor(ifelse(as.character(y) == "no",0,1)))

anyNA(deposit_train)

summary(deposit_train)
str(deposit_train)

hist(deposit_train$age)
boxplot(deposit_train$age) #client's age

hist(deposit_train$duration) 
boxplot(deposit_train$duration) #last contact duration

hist(deposit_train$campaign) 
boxplot(deposit_train$campaign) #number of contacts with the client during campaign

hist(deposit_train$emp.var.rate) 
boxplot(deposit_train$emp.var.rate) #employment variation rate - quarterly indicator

hist(deposit_train$cons.price.idx)
boxplot(deposit_train$cons.price.idx) #consumer price index - monthly indicator

hist(deposit_train$cons.conf.idx)
boxplot(deposit_train$cons.conf.idx) #consumer confidence index - monthly indicator

hist(deposit_train$euribor3m)
boxplot(deposit_train$euribor3m) #euribor 3 month rate - daily indicator

hist(deposit_train$nr.employed)
boxplot(deposit_train$nr.employed) #number of employees - quarterly indicator

#Mann-Whitney test for continuous variables

wilcox.test(age~y, data = deposit_train) 
wilcox.test(duration~y, data = deposit_train) 
wilcox.test(campaign~y, data = deposit_train) 
wilcox.test(emp.var.rate~y, data = deposit_train) 
wilcox.test(cons.price.idx~y, data = deposit_train) 
wilcox.test(cons.conf.idx~y, data = deposit_train) 
wilcox.test(euribor3m~y, data = deposit_train) 
wilcox.test(nr.employed~y, data = deposit_train) 

#Chi-squared test for factor variables

chisq.test(table(deposit_train[deposit_train$y == "yes",]$job, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$marital, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$education, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$default, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$housing, deposit_train[deposit_train$y == "yes",]$y)[,2]) 
chisq.test(table(deposit_train[deposit_train$y == "yes",]$loan, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$contact, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$month, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$day_of_week, deposit_train[deposit_train$y == "yes",]$y)[,2])
chisq.test(table(deposit_train[deposit_train$y == "yes",]$poutcome, deposit_train[deposit_train$y == "yes",]$y)[,2])

#Some factor variables have more than 2 factors so we need to make dummies

deposit_train %>% 
  select(job, y_num, y) %>% 
  group_by(job, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = job, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(marital, y_num, y) %>% 
  group_by(marital, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = marital, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(agree_share = yes/no*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(education, y_num, y) %>% 
  group_by(education, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = education, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(default, y_num, y) %>% 
  group_by(default, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = default, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(housing, y_num, y) %>% 
  group_by(housing, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = housing, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(loan, y_num, y) %>% 
  group_by(loan, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = loan, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(month, y_num, y) %>% 
  group_by(month, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = month, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(day_of_week, y_num, y) %>% 
  group_by(day_of_week, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = day_of_week, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(poutcome, y_num, y) %>% 
  group_by(poutcome, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = poutcome, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

deposit_train %>% 
  select(contact, y_num, y) %>% 
  group_by(contact, y_num, y) %>% 
  summarise(y = n()) %>% 
  pivot_wider(id_cols = contact, names_from = y_num, values_from = y) %>% 
  rename(no = `0`, yes = `1`) %>% 
  mutate(tot = no+yes) %>% 
  mutate(agree_share = yes/tot*100) %>% 
  arrange(desc(agree_share))

#Visualizations - factor variables distribution

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~education)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~default)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~month)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~marital)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~job)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~housing)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~loan)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~contact)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~month)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~day_of_week)

ggplot(data = deposit_train, aes(y))+
  geom_bar()+
  facet_wrap(~poutcome)

#Continuous variables distribution

ggplot(data = deposit_train, aes(x = y, y = age))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = duration))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = campaign))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = emp.var.rate))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = cons.price.idx))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = cons.conf.idx))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = euribor3m))+
  geom_boxplot()

ggplot(data = deposit_train, aes(x = y, y = nr.employed))+
  geom_boxplot()

#Making dummies
deposit_train$job_nonhousemaid <- factor(ifelse(deposit_train$job == "housemaid", 0, 1))
deposit_train$marital_nonmarried <- factor(ifelse(deposit_train$marital == "married", 0, 1))
deposit_train$education_nonbasic4y <- factor(ifelse(deposit_train$education == "basic.4y", 0, 1))
deposit_train$default_nonyes <- factor(ifelse(deposit_train$default == "yes", 0, 1))
deposit_train$housing_nonunknown <- factor(ifelse(deposit_train$housing == "unknown", 0, 1))
deposit_train$loan_nonunknown <- factor(ifelse(deposit_train$loan == "unknown", 0, 1))
deposit_train$month_nonjun <- factor(ifelse(deposit_train$month == "jun", 0, 1))
deposit_train$dayofweek_nonfri <- factor(ifelse(deposit_train$day_of_week == "fri", 0, 1))
deposit_train$poutcome_nonfailure <- factor(ifelse(deposit_train$poutcome == "failure", 0, 1))
deposit_train$contact_fct <- factor(ifelse(deposit_train$contact == "cellular", 1, 0))

#First (basic) model

cor(apply(deposit_train[,22:32], 2, as.numeric)) #we exclude loan_nonunknown cause its correlation with housing_nonunknown = 1

logit_0 <- glm(data = deposit_train, y_num ~ job_nonhousemaid+marital_nonmarried+
                 education_nonbasic4y+default_nonyes+housing_nonunknown+
                 month_nonjun+dayofweek_nonfri+poutcome_nonfailure+contact_fct+age+
                 duration+campaign+pdays+previous+emp.var.rate+cons.price.idx+
                 cons.conf.idx+euribor3m+nr.employed, 
               family = "binomial"(link = "logit"), x = T) 

summary(logit_0)
maBina(logit_0, x = T)

pr_logit_0 <- predict(logit_0, deposit_train)
deposit_train <- cbind(deposit_train, pr_logit_0)
deposit_train <- mutate(deposit_train, prob_0 = stats::plogis(pr_logit_0))

roc.data_0 <- AUC::roc(deposit_train$prob_0, deposit_train$y_num)

qplot(x = roc.data_0$cutoffs, y = roc.data_0$tpr, geom = "line")
qplot(x = roc.data_0$cutoffs, y = roc.data_0$fpr, geom = "line")
qplot(x = roc.data_0$fpr, y = roc.data_0$tpr, geom = "line")

deposit_train$prob_0_fct <- ifelse(deposit_train$prob_0 >= 0.1, 1, 0)
mean(deposit_train$prob_0_fct == deposit_train$y_num) #90.14% correct predictions

deposit_train_x <- deposit_train[deposit_train$y_num == 1, ]
mean(deposit_train_x$prob_0_fct == deposit_train_x$y_num) #76.67% correct agreements

deposit_train_y <- deposit_train[deposit_train$y_num != 1, ]
mean(deposit_train_y$prob_0_fct == deposit_train_y$y_num) #91.07% correct disagreements

#Second model with job affect
logit_1 <- glm(data = deposit_train, y_num ~ marital_nonmarried+
                 education_nonbasic4y+default_nonyes+housing_nonunknown+
                 month_nonjun+dayofweek_nonfri+poutcome_nonfailure+contact_fct+job_nonhousemaid*(age+
                 duration+campaign+pdays+previous+emp.var.rate+cons.price.idx+
                 cons.conf.idx+euribor3m+nr.employed), 
               family = "binomial"(link = "logit"), x = T) 

summary(logit_1)
maBina(logit_1, x = T)

pr_logit_1 <- predict(logit_1, deposit_train)
deposit_train <- cbind(deposit_train, pr_logit_1)
deposit_train <- mutate(deposit_train, prob_1 = stats::plogis(pr_logit_1))

roc.data_1 <- AUC::roc(deposit_train$prob_1, deposit_train$y_num)

qplot(x = roc.data_1$cutoffs, y = roc.data_1$tpr, geom = "line")
qplot(x = roc.data_1$cutoffs, y = roc.data_1$fpr, geom = "line")
qplot(x = roc.data_1$fpr, y = roc.data_1$tpr, geom = "line")

deposit_train$prob_1_fct <- ifelse(deposit_train$prob_1 >= 0.1, 1, 0)
mean(deposit_train$prob_1_fct == deposit_train$y_num) #90.11% correct predictions

deposit_train_x <- deposit_train[deposit_train$y_num == 1, ]
mean(deposit_train_x$prob_1_fct == deposit_train_x$y_num) #76.80% correct agreements

deposit_train_y <- deposit_train[deposit_train$y_num != 1, ]
mean(deposit_train_y$prob_1_fct == deposit_train_y$y_num) #91.03% correct disagreements

#Third model with education affect
logit_2 <- glm(data = deposit_train, y_num ~ job_nonhousemaid+marital_nonmarried+
                 default_nonyes+housing_nonunknown+
                 month_nonjun+dayofweek_nonfri+
                 poutcome_nonfailure+contact_fct+
                 education_nonbasic4y*(age+duration+campaign+pdays+previous+
                                         emp.var.rate+cons.price.idx+
                                         cons.conf.idx+euribor3m+nr.employed), 
               family = "binomial"(link = "logit"), x = T) 

summary(logit_2)
maBina(logit_2, x = T)

pr_logit_2 <- predict(logit_2, deposit_train)
deposit_train <- cbind(deposit_train, pr_logit_2)
deposit_train <- mutate(deposit_train, prob_2 = stats::plogis(pr_logit_2))

roc.data_2 <- AUC::roc(deposit_train$prob_2, deposit_train$y_num)

qplot(x = roc.data_2$cutoffs, y = roc.data_2$tpr, geom = "line")
qplot(x = roc.data_2$cutoffs, y = roc.data_2$fpr, geom = "line")
qplot(x = roc.data_2$fpr, y = roc.data_2$tpr, geom = "line")

deposit_train$prob_2_fct <- ifelse(deposit_train$prob_2 >= 0.1, 1, 0)
mean(deposit_train$prob_2_fct == deposit_train$y_num) #90.13% correct predictions

deposit_train_x <- deposit_train[deposit_train$y_num == 1, ]
mean(deposit_train_x$prob_2_fct == deposit_train_x$y_num) #77.15% correct agreements

deposit_train_y <- deposit_train[deposit_train$y_num != 1, ]
mean(deposit_train_y$prob_2_fct == deposit_train_y$y_num) #91.02% correct disagreements

#Fourth model with month affect
logit_3 <- glm(data = deposit_train, y_num ~ job_nonhousemaid+education_nonbasic4y+marital_nonmarried+
                 default_nonyes+housing_nonunknown+dayofweek_nonfri+
                 contact_fct+month_nonjun+
               poutcome_nonfailure*(age+duration+campaign+pdays+previous+
                                         emp.var.rate+cons.price.idx+
                                         cons.conf.idx+euribor3m+nr.employed), 
               family = "binomial"(link = "logit"), x = T) 

summary(logit_3)
maBina(logit_3, x = T)

pr_logit_3 <- predict(logit_3, deposit_train)
deposit_train <- cbind(deposit_train, pr_logit_3)
deposit_train <- mutate(deposit_train, prob_3 = stats::plogis(pr_logit_3))

roc.data_3 <- AUC::roc(deposit_train$prob_3, deposit_train$y_num)

qplot(x = roc.data_3$cutoffs, y = roc.data_3$tpr, geom = "line")
qplot(x = roc.data_3$cutoffs, y = roc.data_3$fpr, geom = "line")
qplot(x = roc.data_3$fpr, y = roc.data_3$tpr, geom = "line")

deposit_train$prob_3_fct <- ifelse(deposit_train$prob_3 >= 0.1, 1, 0)
mean(deposit_train$prob_3_fct == deposit_train$y_num) #90.16% correct predictions

deposit_train_x <- deposit_train[deposit_train$y_num == 1, ]
mean(deposit_train_x$prob_3_fct == deposit_train_x$y_num) #77.07% correct agreements

deposit_train_y <- deposit_train[deposit_train$y_num != 1, ]
mean(deposit_train_y$prob_3_fct == deposit_train_y$y_num) #91.07% correct disagreements

#choosing best model
best_model <- data.frame(model = c("logit_0","logit_1","logit_2","logit_3"),
  correct_predictions = c(
  mean(deposit_train$prob_0_fct == deposit_train$y_num),
  mean(deposit_train$prob_1_fct == deposit_train$y_num),
  mean(deposit_train$prob_2_fct == deposit_train$y_num),
  mean(deposit_train$prob_3_fct == deposit_train$y_num)
),
correct_agreements = c(
  mean(deposit_train_x$prob_0_fct == deposit_train_x$y_num),
  mean(deposit_train_x$prob_1_fct == deposit_train_x$y_num),
  mean(deposit_train_x$prob_2_fct == deposit_train_x$y_num),
  mean(deposit_train_x$prob_3_fct == deposit_train_x$y_num)
),
correct_disagreements = c(
  mean(deposit_train_y$prob_0_fct == deposit_train_y$y_num),
  mean(deposit_train_y$prob_1_fct == deposit_train_y$y_num),
  mean(deposit_train_y$prob_2_fct == deposit_train_y$y_num),
  mean(deposit_train_y$prob_3_fct == deposit_train_y$y_num)
),
aic = c(
  AIC(logit_0),
  AIC(logit_1),
  AIC(logit_2),
  AIC(logit_3)
))

best_model #third model - most correct agreements and lowest AIC

#time to make predictions!!!
deposit_test <- tibble(read.csv("bank-additional-full.csv", header = T,
                                 sep = ";", dec = ".", stringsAsFactors = T)) %>% 
  slice(35001:nrow(.)) %>% 
  mutate(y_num = factor(ifelse(as.character(y) == "no",0,1)))

deposit_test$job_nonhousemaid <- factor(ifelse(deposit_test$job == "housemaid", 0, 1))
deposit_test$marital_nonmarried <- factor(ifelse(deposit_test$marital == "married", 0, 1))
deposit_test$education_nonbasic4y <- factor(ifelse(deposit_test$education == "basic.4y", 0, 1))
deposit_test$default_nonyes <- factor(ifelse(deposit_test$default == "yes", 0, 1))
deposit_test$housing_nonunknown <- factor(ifelse(deposit_test$housing == "unknown", 0, 1))
deposit_test$loan_nonunknown <- factor(ifelse(deposit_test$loan == "unknown", 0, 1))
deposit_test$month_nonjun <- factor(ifelse(deposit_test$month == "jun", 0, 1))
deposit_test$dayofweek_nonfri <- factor(ifelse(deposit_test$day_of_week == "fri", 0, 1))
deposit_test$poutcome_nonfailure <- factor(ifelse(deposit_test$poutcome == "failure", 0, 1))
deposit_test$contact_fct <- factor(ifelse(deposit_test$contact == "cellular", 1, 0))

pr_logit_test <- predict(logit_2, deposit_test, se = T)
deposit_test <- cbind(deposit_test, pr_logit_test)
deposit_test <- mutate(deposit_test, prob = stats::plogis(fit),
                         left_ci = stats::plogis(fit-1.96*se.fit),
                         right_ci = stats::plogis(fit+1.96*se.fit))

deposit_test$prediction <- ifelse(deposit_test$prob >= 0.1, 1, 0)
table(deposit_test$prediction)

qplot(data = deposit_test, x = fit, y = prob) + geom_point()