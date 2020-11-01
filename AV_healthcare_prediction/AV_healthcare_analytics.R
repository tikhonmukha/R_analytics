library(dplyr)
library(ggplot2)
library(nnet)

#import file
healthcare_train <- tibble(read.csv("AV_healthcare_train_data.csv", stringsAsFactors = T))
anyNA(healthcare_train)
healthcare_train <- na.omit(healthcare_train)
glimpse(healthcare_train)
head(healthcare_train)
tail(healthcare_train)
summary(healthcare_train)

healthcare_train <- healthcare_train %>% 
  mutate(across(c("Hospital_code", "City_Code_Hospital"), factor))

#exploring the variables
ggplot(data = healthcare_train, aes(x = factor(Available.Extra.Rooms.in.Hospital)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Available extra rooms in hospital", x = "")

ggplot(data = healthcare_train, aes(x = Department))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Department", x = "")

ggplot(data = healthcare_train, aes(x = factor(Bed.Grade)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Bed grade", x = "")

ggplot(data = healthcare_train, aes(x = factor(Type.of.Admission)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Type of admission", x = "")

ggplot(data = healthcare_train, aes(x = factor(Severity.of.Illness)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Severity of illness", x = "")

ggplot(data = healthcare_train, aes(x = factor(Visitors.with.Patient)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Visitors with patient", x = "")

ggplot(data = healthcare_train, aes(x = factor(Age)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Age", x = "")

ggplot(data = healthcare_train, aes(y = Admission_Deposit))+
  geom_boxplot()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Admission deposit", x = "", y = "")

ggplot(data = healthcare_train, aes(x = factor(Stay)))+
  geom_bar()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 30, size = rel(0.9), hjust = 1, vjust = 1))+
  labs(title = "Stay", x = "")

#How other variables depend on stay
cor(healthcare_train[,c(6,10,15,17)])

healthcare_chisq <- function(x) {
  chisq.test(
    table(healthcare_train$Stay, x), correct = T)$p.value
}

fact_var_table <- data.frame(var = c("Hospital_code","Hospital_type_code","City_Code_Hospital",
                                     "Hospital_region_code","Department","Ward_Type",
                                 "Type.of.Admission","Severity.of.Illness","Age"),
                         p_value = c(
                           healthcare_chisq(healthcare_train$Hospital_code),
                           healthcare_chisq(healthcare_train$Hospital_type_code),
                           healthcare_chisq(healthcare_train$City_Code_Hospital),
                           healthcare_chisq(healthcare_train$Hospital_region_code),
                           healthcare_chisq(healthcare_train$Department),
                           healthcare_chisq(healthcare_train$Ward_Type),
                           healthcare_chisq(healthcare_train$Type.of.Admission),
                           healthcare_chisq(healthcare_train$Severity.of.Illness),
                           healthcare_chisq(healthcare_train$Age)
                         ))

fact_var_table

healthcare_kruskal <- function(x) {
  kruskal.test(data = healthcare_train, x ~ Stay)$p.value
}

cont_var_table <- data.frame(var = c("Available.Extra.Rooms.in.Hospital","Visitors.with.Patient",
                                     "Admission_Deposit"),
                             p_value = c(
                               healthcare_kruskal(healthcare_train$Available.Extra.Rooms.in.Hospital),
                               healthcare_kruskal(healthcare_train$Visitors.with.Patient),
                               healthcare_kruskal(healthcare_train$Admission_Deposit)
                             ))

cont_var_table

#making multinomial logistic regression
model <- multinom(data = healthcare_train, Stay ~ Hospital_type_code+Department+
                    Type.of.Admission+Severity.of.Illness+Age+Available.Extra.Rooms.in.Hospital+
                    Visitors.with.Patient+Admission_Deposit)
summary(model)
probs <- fitted(model)
pred <- apply(probs, 1, function(x) colnames(probs)[which(x==max(x))])
table(Fact=healthcare_train$Stay, Forecast=pred)
accuracy <- mean(pred==healthcare_train$Stay)
paste0("Model accuracy is ",round(accuracy*100,2),"%")

z <- summary(model)$coefficients/summary(model)$standard.errors
pt(1-pnorm(abs(z),0,1))*2 #wald test for checking coefficients significance