library(dplyr)
library(magrittr)
library(ggplot2)
library(caret)
library(erer)
library(lmtest)
library(nortest)
library(pROC)

# Importing data

customers_data <- read.csv('BankChurners.csv', header = T, sep = ',', dec = '.', stringsAsFactors = T) %>% 
  tibble %>% 
  select(-c(22,23)) %>% 
  mutate(CLIENTNUM = factor(CLIENTNUM))

head(customers_data)
tail(customers_data)
glimpse(customers_data)
summary(customers_data)

# Exploratory data analysis

## Correlation between continuous variables

mcor <- cor(customers_data %>% select(-c(1,2,4,6,7:9)), method = 'spearman') %>% round(., digits = 3)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot::corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
                   tl.cex = 0.75, number.cex = 0.75, col = col(200), addCoef.col = "black",
                   order = "AOE", title = "Spearman correlation matrix")

## Distribution of continuous variables analysis

ggplot(data = customers_data, aes(x = Customer_Age, fill = Gender))+
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom')+
  labs(title = 'Customers age histogram', x = 'Age', y = 'Frequency')

ggplot(data = customers_data, aes(x = Customer_Age, fill = Attrition_Flag))+
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Customers age histogram', x = 'Age', y = 'Frequency')

ggplot(data = customers_data, aes(x = Total_Trans_Ct, fill = Attrition_Flag))+
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 150, by = 25))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Transactions count in last 12 months histogram', x = 'Total transactions', y = 'Frequency')

ggplot(data = customers_data, aes(x = Total_Ct_Chng_Q4_Q1, fill = Attrition_Flag))+
  geom_histogram(binwidth = 0.05, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Changing in transactions count Q4 vs Q1', x = 'Changing in transactions count', y = 'Frequency')

ggplot(data = customers_data, aes(x = Total_Trans_Amt, fill = Attrition_Flag))+
  geom_histogram(binwidth = 250, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 20000, by = 2500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Transactions amount in last 12 months histogram', x = 'Total transactions', y = 'Frequency')

ggplot(data = customers_data, aes(x = Total_Amt_Chng_Q4_Q1, fill = Attrition_Flag))+
  geom_histogram(binwidth = 0.05, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 4, by = 0.25))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Changing in transactions amount Q4 vs Q1', x = 'Changing in transactions amount', y = 'Frequency')

ggplot(data = customers_data, aes(x = Total_Revolving_Bal, fill = Attrition_Flag))+
  geom_histogram(position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 3000, by = 500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Total revolving balance on the credit card histogram', x = 'Revolving balance', y = 'Frequency')

ggplot(data = customers_data, aes(x = Credit_Limit, fill = Attrition_Flag))+
  geom_histogram(binwidth = 500, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 35000, by = 2500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Credit limit histogram', x = 'Credit limit', y = 'Frequency')

ggplot(data = customers_data, aes(x = Months_on_book, fill = Attrition_Flag))+
  geom_histogram(binwidth = 1, position = 'identity', alpha = 0.5)+
  scale_x_continuous(breaks = seq(0, 60, by = 5))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'bottom', legend.title = element_blank())+
  labs(title = 'Period of relationship with bank histogram', x = 'Period in months', y = 'Frequency')

## Testing continuous variables distribution normality

x <- apply(select_if(customers_data, is.numeric),2,ad.test)

for (i in 1:length(x)){
  print(x[[i]]$p.value)
} #no variables with normal distribution

##Testing continuous variables significance

y <- apply(select_if(customers_data, is.numeric),2, function(col) wilcox.test(col ~ customers_data$Attrition_Flag))

for (i in 1:length(y)){
  print(y[[i]]$p.value)
} #Customer_Age and Months_on_book are insignificant

## Distribution of factor variables analysis

ggplot(data = customers_data %>% 
         select(Education_Level, Attrition_Flag) %>% 
         group_by(Education_Level, Attrition_Flag) %>% 
         count() %>% arrange(Attrition_Flag, desc(n)), aes(x = Attrition_Flag, y = n, fill = reorder(Education_Level,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Clients education level', x = '', y = 'Share')

ggplot(data = customers_data %>% 
         select(Gender, Attrition_Flag) %>% 
         group_by(Gender, Attrition_Flag) %>% 
         count() %>% arrange(Attrition_Flag, desc(n)), aes(x = Attrition_Flag, y = n, fill = reorder(Gender,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Clients gender', x = '', y = 'Share')

ggplot(data = customers_data %>% 
         select(Marital_Status, Attrition_Flag) %>% 
         group_by(Marital_Status, Attrition_Flag) %>% 
         count() %>% arrange(Attrition_Flag, desc(n)), aes(x = Attrition_Flag, y = n, fill = reorder(Marital_Status,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Clients marital status', x = '', y = 'Share')

ggplot(data = customers_data %>% 
         select(Income_Category, Attrition_Flag) %>% 
         group_by(Income_Category, Attrition_Flag) %>% 
         count() %>% arrange(Attrition_Flag, desc(n)), aes(x = Attrition_Flag, y = n, fill = reorder(Income_Category,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Clients income category', x = '', y = 'Share')

ggplot(data = customers_data %>% 
         select(Card_Category, Attrition_Flag) %>% 
         group_by(Card_Category, Attrition_Flag) %>% 
         count() %>% arrange(Attrition_Flag, desc(n)), aes(x = Attrition_Flag, y = n, fill = reorder(Card_Category,n)))+
  geom_col(position = 'fill', colour = 'black')+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = 'Pastel1')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = 'bottom')+
  labs(title = 'Clients card category', x = '', y = 'Share')

##Testing factor variables significance

chisq.test(table(customers_data$Attrition_Flag, customers_data$Gender))$p.value

z <- apply(customers_data %>% 
             select_if(., is.factor) %>% 
             select(-c('CLIENTNUM','Attrition_Flag')),2, function(col) chisq.test(table(customers_data$Attrition_Flag, col)))

for (i in 1:length(z)){
  print(z[[i]]$p.value)
} #Education_Level and Card_Category are insignificant

## Customers transactions analysis

ggplot(data = customers_data, aes(x = Attrition_Flag, y = Total_Trans_Ct, fill = Attrition_Flag))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(0, 150, by = 25))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Transactions count by attrited and existing clients', x = '', y = 'Number of transaction in last 12 months')

ggplot(data = customers_data, aes(x = Attrition_Flag, y = Total_Trans_Amt, fill = Attrition_Flag))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(0, 20000, by = 2500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')+
  labs(title = 'Transactions amount by attrited and existing clients', x = '', y = 'Transactions amount in last 12 months')

coplot(Total_Trans_Amt ~ Total_Trans_Ct|Attrition_Flag, data = customers_data,
      xlab = c('Transactions count','Attrition flag'), ylab = 'Transactions amount',
      col = 'blue', bar.bg = c(fac = 'coral'), panel = function(x, y, ...) {
        panel.smooth(x, y, lty = 2, pch = 19, col = "blue",
                     span = 0.5)})

paste(c('EDF =', summary(mgcv::gam(Total_Trans_Amt ~ s(Total_Trans_Ct), 
                                   data = customers_data))$edf), collapse = ' ') #according to corrplot and GAM we have strong non-linear dependence between transactions count and transactions amount

#Splitting dataset into train and test

in_train <- createDataPartition(y = customers_data$Attrition_Flag, p = 0.8, list = F)
customers_data_train <- customers_data[in_train, ] 

customers_data_train %<>% 
  select(-c('Customer_Age', 'Months_on_book', 'Education_Level', 'Card_Category')) %>% 
  mutate('Attrition_Flag_fact' = ifelse(Attrition_Flag == 'Attrited Customer', 1, 0) %>% factor)
  
customers_data_test <- customers_data[-in_train, ] 

customers_data_test %<>% 
  select(-c('Customer_Age', 'Months_on_book', 'Education_Level', 'Card_Category')) %>% 
  mutate('Attrition_Flag_fact' = ifelse(Attrition_Flag == 'Attrited Customer', 1, 0) %>% factor)

#Making a logit model
##Basic model

model <- glm(Attrition_Flag_fact ~ Gender+Dependent_count+Marital_Status+Income_Category+Total_Relationship_Count+
               Months_Inactive_12_mon+Contacts_Count_12_mon+Credit_Limit+Total_Amt_Chng_Q4_Q1+
               Total_Trans_Amt+Total_Ct_Chng_Q4_Q1+Avg_Utilization_Ratio, family = binomial(link = 'logit'), x = TRUE, data = customers_data_train)

summary(model)
maBina(model)

customers_data_train$pred <- predict(model, newdata = customers_data_train, type = 'response')
pred <- ifelse(customers_data_train$pred > 0.5, 1, 0) %>% factor
confusionMatrix(data = pred, reference = customers_data_train$Attrition_Flag_fact)

m_ROC.roc <- roc(customers_data_train$Attrition_Flag_fact, customers_data_train$pred)
plot(m_ROC.roc, grid.col = c('green', 'blue'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc), col = 'blue', add = TRUE, print.auc = FALSE)

##First alternative model
model_1 <- glm(Attrition_Flag_fact ~ Gender+Dependent_count+Marital_Status+Income_Category+Total_Relationship_Count+
               Months_Inactive_12_mon+Contacts_Count_12_mon+Avg_Open_To_Buy+Total_Amt_Chng_Q4_Q1+
               Total_Trans_Ct+Total_Ct_Chng_Q4_Q1+Total_Revolving_Bal, family = binomial(link = 'logit'), x = TRUE, data = customers_data_train)

summary(model_1)
maBina(model_1)

anova(model, model_1)

customers_data_train$pred_1 <- predict(model_1, newdata = customers_data_train, type = 'response')
pred_1 <- ifelse(customers_data_train$pred_1 > 0.2, 1, 0) %>% factor #as a 'border' value i'm using the value what was gotten from ROC curve of the basic model
confusionMatrix(data = pred_1, reference = customers_data_train$Attrition_Flag_fact)

m_ROC.roc_1 <- roc(customers_data_train$Attrition_Flag_fact, customers_data_train$pred_1)
plot(m_ROC.roc_1, grid.col = c('green', 'blue'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc_1), col = 'blue', add = TRUE, print.auc = FALSE)

##Second alternative model

model_2 <- glm(Attrition_Flag_fact ~ Gender+Dependent_count+Marital_Status+Income_Category+Total_Relationship_Count+
                 Months_Inactive_12_mon+Contacts_Count_12_mon+Total_Amt_Chng_Q4_Q1+
                 Total_Trans_Ct+Total_Ct_Chng_Q4_Q1+Total_Revolving_Bal+Total_Trans_Amt, family = binomial(link = 'logit'), x = TRUE, data = customers_data_train)

summary(model_2)
maBina(model_2)

anova(model, model_1, model_2)

customers_data_train$pred_2 <- predict(model_2, newdata = customers_data_train, type = 'response')
pred_2 <- ifelse(customers_data_train$pred_2 > 0.2, 1, 0) %>% factor #as a 'border' value i'm using the value what was gotten from ROC curve of the basic model
confusionMatrix(data = pred_2, reference = customers_data_train$Attrition_Flag_fact)

m_ROC.roc_2 <- roc(customers_data_train$Attrition_Flag_fact, customers_data_train$pred_2)
plot(m_ROC.roc_2, grid.col = c('green', 'blue'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc_2), col = 'blue', add = TRUE, print.auc = FALSE)

##Model_2 is the best (lowest deviance and AIC, highest accuracy, sensitivity and specificity) so it will be used for test dataset

customers_data_test$pred <- predict(model_2, newdata = customers_data_test, type = 'response')
pred_test <- ifelse(customers_data_test$pred > 0.2, 1, 0) %>% factor 
confusionMatrix(data = pred_test, reference = customers_data_test$Attrition_Flag_fact)

m_ROC.roc_test <- roc(customers_data_test$Attrition_Flag_fact, customers_data_test$pred)
plot(m_ROC.roc_test, grid.col = c('green', 'blue'), grid = c(0.1,0.2), print.auc = TRUE,
     print.thres = TRUE)
plot(smooth(m_ROC.roc_test), col = 'blue', add = TRUE, print.auc = FALSE)

ggplot(data = customers_data_test, aes(x = pred))+
  geom_density(aes(color = Attrition_Flag, linetype = Attrition_Flag))+
  theme_minimal()
