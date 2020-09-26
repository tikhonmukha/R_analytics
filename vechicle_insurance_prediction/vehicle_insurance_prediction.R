library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(AUC)
library(erer)

insurance_train <- tibble(read.csv(file = "insurance_train.csv", header = T, sep = ",", dec = ".", stringsAsFactors = T))
str(insurance_train)
glimpse(insurance_train)
head(insurance_train)
tail(insurance_train)
anyNA(insurance_train)

insurance_train <- insurance_train %>% 
  mutate(across(c("id", "Region_Code", "Driving_License", "Previously_Insured", "Policy_Sales_Channel", "Response"), factor)) 

ggplot(insurance_train, aes(x = Gender))+
  geom_bar()+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Gender distribution", x = "", y = "")

chisq.test(table(insurance_train$Gender, insurance_train$Response))

ggplot(insurance_train, aes(x = factor(Age)))+
  geom_bar()+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Age distribution", x = "", y = "")

t.test(insurance_train$Age~insurance_train$Response)

ggplot(insurance_train, aes(x = Previously_Insured))+
  geom_bar()+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Does previous insurance affect on response?", x = "", y = "")  

chisq.test(table(insurance_train$Previously_Insured, insurance_train$Response))

ggplot(insurance_train, aes(x = (Vehicle_Damage)))+
  geom_bar()+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Do car accidents affect on response?", x = "", y = "")  

chisq.test(table(insurance_train$Vehicle_Damage, insurance_train$Response))

ggplot(insurance_train, aes(x = Region_Code))+
  geom_bar()+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Responses distribution by region", x = "", y = "")

chisq.test(table(insurance_train$Region_Code, insurance_train$Response))

ggplot(insurance_train, aes(x = Vehicle_Age))+
  geom_bar()+
  scale_x_discrete(limits = c("< 1 Year", "1-2 Year", "> 2 Years"))+
  facet_wrap(~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Does vehicle age affects on response?", x = "", y = "") 

chisq.test(table(insurance_train$Vehicle_Age, insurance_train$Response))

ggplot(insurance_train, aes(x = factor(Age)))+
  geom_bar()+
  facet_wrap(Gender~Response, scales = "fixed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Age and gender affect on response", x = "", y = "") #yes

t.test(insurance_train$Annual_Premium~insurance_train$Gender)

ggplot(insurance_train, aes(x = Previously_Insured))+
  geom_bar()+
  facet_wrap(Gender~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Previous insurance and gender affect on response", x = "", y = "") #no

chisq.test(table(insurance_train$Previously_Insured, insurance_train$Gender))

ggplot(insurance_train, aes(x = (Vehicle_Damage)))+
  geom_bar()+
  facet_wrap(Gender~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Vechicle damage and gender affect on response", x = "", y = "") #no

chisq.test(table(insurance_train$Vehicle_Damage, insurance_train$Gender))

ggplot(insurance_train, aes(x = Region_Code))+
  geom_bar()+
  facet_wrap(Gender~Response, scales = "fixed")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Region and gender affect on response", x = "", y = "") #no

chisq.test(table(insurance_train$Region_Code, insurance_train$Gender))

ggplot(insurance_train, aes(x = Vehicle_Age))+
  geom_bar()+
  scale_x_discrete(limits = c("< 1 Year", "1-2 Year", "> 2 Years"))+
  facet_wrap(Gender~Response, scales = "fixed")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Vechicle age and gender affect on response", x = "", y = "") #no

chisq.test(table(insurance_train$Vehicle_Age, insurance_train$Gender))

insurance_train$gender_fct <- factor(ifelse(insurance_train$Gender == "Male", 1, 0))

insurance_train$oneyearage <- factor(ifelse(insurance_train$Vehicle_Age == "< 1 Year", 1, 0))
chisq.test(table(insurance_train$oneyearage, insurance_train$Gender))

insurance_train$onetwoyearage <- factor(ifelse(insurance_train$Vehicle_Age == "1-2 Year", 1, 0))
chisq.test(table(insurance_train$onetwoyearage, insurance_train$Gender))

insurance_train$Vehicle_Damage_fct <- factor(ifelse(insurance_train$Vehicle_Damage == "Yes", 1, 0))

insurance_train$twentyeight_region <- factor(ifelse(insurance_train$Region_Code == 28, 1, 0))
chisq.test(table(insurance_train$twentyeight_region, insurance_train$Gender))

#First (basic) model
logit_0 <- glm(data = insurance_train, Response ~ Age + Driving_License + 
               Previously_Insured + Vehicle_Damage_fct + Annual_Premium +
               Vintage + gender_fct + oneyearage + onetwoyearage + twentyeight_region, 
             family = "binomial"(link = "logit"), x = T)

summary(logit_0)
maBina(logit_0, x = T)

pr_logit_0 <- predict(logit_0, insurance_train)
insurance_train <- cbind(insurance_train, pr_logit_0)
insurance_train <- mutate(insurance_train, prob_0 = stats::plogis(pr_logit_0))

roc.data_0 <- AUC::roc(insurance_train$prob_0, insurance_train$Response)

qplot(x = roc.data_0$cutoffs, y = roc.data_0$tpr, geom = "line")
qplot(x = roc.data_0$cutoffs, y = roc.data_0$fpr, geom = "line")
qplot(x = roc.data_0$fpr, y = roc.data_0$tpr, geom = "line")

insurance_train$prob_0_fct <- ifelse(insurance_train$prob_0 >= 0.2, 1, 0)
mean(insurance_train$prob_0_fct == insurance_train$Response) #74.1% correct predictions

insurance_train_x <- insurance_train[insurance_train$Response == 1, ]
mean(insurance_train_x$prob_0_fct == insurance_train_x$Response) #79.75% correct agreements

insurance_train_y <- insurance_train[insurance_train$Response != 1, ]
mean(insurance_train_y$prob_0_fct == insurance_train_y$Response) #73.31% correct disagreements

#Second model with gender affect on customer age and without vintage
logit_1 <- glm(data = insurance_train, Response ~ gender_fct*Age + Driving_License + 
                 Previously_Insured + gender_fct + Vehicle_Damage_fct + Annual_Premium + 
                 oneyearage + onetwoyearage + twentyeight_region, 
               family = "binomial"(link = "logit"), x = T)

summary(logit_1)
maBina(logit_1, x = T)

pr_logit_1 <- predict(logit_1, insurance_train)
insurance_train <- cbind(insurance_train, pr_logit_1)
insurance_train <- mutate(insurance_train, prob_1 = stats::plogis(pr_logit_1))

roc.data_1 <- AUC::roc(insurance_train$prob_1, insurance_train$Response)

qplot(x = roc.data_1$cutoffs, y = roc.data_1$tpr, geom = "line")
qplot(x = roc.data_1$cutoffs, y = roc.data_1$fpr, geom = "line")
qplot(x = roc.data_1$fpr, y = roc.data_1$tpr, geom = "line")

insurance_train$prob_1_fct <- ifelse(insurance_train$prob_1 >= 0.2, 1, 0)
mean(insurance_train$prob_1_fct == insurance_train$Response) #74.00% correct predictions

insurance_train_x <- insurance_train[insurance_train$Response == 1, ]
mean(insurance_train_x$prob_1_fct == insurance_train_x$Response) #79.9% correct agreements

insurance_train_y <- insurance_train[insurance_train$Response != 1, ]
mean(insurance_train_y$prob_1_fct == insurance_train_y$Response) #73.17% correct disagreements

#Third model with more gender affect
logit_2 <- glm(data = insurance_train, Response ~ gender_fct*Age + Driving_License + gender_fct +
                 gender_fct*Previously_Insured + gender_fct*Vehicle_Damage_fct + gender_fct*Annual_Premium + 
                 gender_fct*oneyearage + gender_fct*onetwoyearage + gender_fct*twentyeight_region, 
               family = "binomial"(link = "logit"), x = T)

summary(logit_2)
maBina(logit_2, x = T)

pr_logit_2 <- predict(logit_2, insurance_train)
insurance_train <- cbind(insurance_train, pr_logit_2)
insurance_train <- mutate(insurance_train, prob_2 = stats::plogis(pr_logit_2))

roc.data_2 <- AUC::roc(insurance_train$prob_2, insurance_train$Response)

qplot(x = roc.data_2$cutoffs, y = roc.data_2$tpr, geom = "line")
qplot(x = roc.data_2$cutoffs, y = roc.data_2$fpr, geom = "line")
qplot(x = roc.data_2$fpr, y = roc.data_2$tpr, geom = "line")

insurance_train$prob_2_fct <- ifelse(insurance_train$prob_2 >= 0.2, 1, 0)
mean(insurance_train$prob_2_fct == insurance_train$Response) #73.95% correct predictions

insurance_train_x <- insurance_train[insurance_train$Response == 1, ]
mean(insurance_train_x$prob_2_fct == insurance_train_x$Response) #80.14% correct agreements

insurance_train_y <- insurance_train[insurance_train$Response != 1, ]
mean(insurance_train_y$prob_2_fct == insurance_train_y$Response) #73.08% correct disagreements

#Fourth model without vintage and premium and gender affect
logit_3 <- glm(data = insurance_train, Response ~ Age + Driving_License + gender_fct +
                 Previously_Insured + Vehicle_Damage_fct + 
                 oneyearage + onetwoyearage + twentyeight_region, 
               family = "binomial"(link = "logit"), x = T)

summary(logit_3)
maBina(logit_3, x = T)

pr_logit_3 <- predict(logit_3, insurance_train)
insurance_train <- cbind(insurance_train, pr_logit_3)
insurance_train <- mutate(insurance_train, prob_3 = stats::plogis(pr_logit_3))

roc.data_3 <- AUC::roc(insurance_train$prob_3, insurance_train$Response)

qplot(x = roc.data_3$cutoffs, y = roc.data_3$tpr, geom = "line")
qplot(x = roc.data_3$cutoffs, y = roc.data_3$fpr, geom = "line")
qplot(x = roc.data_3$fpr, y = roc.data_3$tpr, geom = "line")

insurance_train$prob_3_fct <- ifelse(insurance_train$prob_3 >= 0.2, 1, 0)
mean(insurance_train$prob_3_fct == insurance_train$Response) #74.09% correct predictions

insurance_train_x <- insurance_train[insurance_train$Response == 1, ]
mean(insurance_train_x$prob_3_fct == insurance_train_x$Response) #79.73% correct agreements

insurance_train_y <- insurance_train[insurance_train$Response != 1, ]
mean(insurance_train_y$prob_3_fct == insurance_train_y$Response) #73.3% correct disagreements

best_model <- data.frame(correct_predictions = c(
  mean(insurance_train$prob_0_fct == insurance_train$Response),
  mean(insurance_train$prob_1_fct == insurance_train$Response),
  mean(insurance_train$prob_2_fct == insurance_train$Response),
  mean(insurance_train$prob_3_fct == insurance_train$Response)
),
correct_agreements = c(
  mean(insurance_train_x$prob_0_fct == insurance_train_x$Response),
  mean(insurance_train_x$prob_1_fct == insurance_train_x$Response),
  mean(insurance_train_x$prob_2_fct == insurance_train_x$Response),
  mean(insurance_train_x$prob_3_fct == insurance_train_x$Response)
),
correct_disagreements = c(
  mean(insurance_train_y$prob_0_fct == insurance_train_y$Response),
  mean(insurance_train_y$prob_1_fct == insurance_train_y$Response),
  mean(insurance_train_y$prob_2_fct == insurance_train_y$Response),
  mean(insurance_train_y$prob_3_fct == insurance_train_y$Response)
),
aic = c(
  AIC(logit_0),
  AIC(logit_1),
  AIC(logit_2),
  AIC(logit_3)
))

best_model

#Third model (logit_2) has best correct agreements share and lowest AIC so it fits best for predictions
insurance_test <- tibble(read.csv(file = "insurance_test.csv", header = T, sep = ",", dec = ".", stringsAsFactors = T))
str(insurance_test)
glimpse(insurance_test)
head(insurance_test)
tail(insurance_test)
anyNA(insurance_test)

insurance_test <- insurance_test %>% 
  mutate(across(c("id", "Region_Code", "Driving_License", "Previously_Insured", "Policy_Sales_Channel"), factor)) 

insurance_test$gender_fct <- factor(ifelse(insurance_test$Gender == "Male", 1, 0))
insurance_test$oneyearage <- factor(ifelse(insurance_test$Vehicle_Age == "< 1 Year", 1, 0))
insurance_test$onetwoyearage <- factor(ifelse(insurance_test$Vehicle_Age == "1-2 Year", 1, 0))
insurance_test$Vehicle_Damage_fct <- factor(ifelse(insurance_test$Vehicle_Damage == "Yes", 1, 0))
insurance_test$twentyeight_region <- factor(ifelse(insurance_test$Region_Code == 28, 1, 0))

pr_logit_test <- predict(logit_2, insurance_test, se = T)
insurance_test <- cbind(insurance_test, pr_logit_test)
insurance_test <- mutate(insurance_test, prob = stats::plogis(fit),
                         left_ci = stats::plogis(fit-1.96*se.fit),
                         right_ci = stats::plogis(fit+1.96*se.fit))

insurance_test$response <- ifelse(insurance_test$prob >= 0.2, 1, 0)
table(insurance_test$response)

qplot(data = insurance_test, x = fit, y = prob) + geom_point()