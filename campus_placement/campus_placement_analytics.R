library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

campus <- read.csv("Placement_Data_Full_Class.csv", header = T, stringsAsFactors = T, na.strings = "")
head(campus)
str(campus)

campus_dp <- as_tibble(campus)
campus_dp$salary <- as.double(campus_dp$salary)

camp_selection <- function(col){
  campus_dp %>% 
    select(col, status) %>% 
    mutate("index" = as.factor(col)) %>% 
    rename("value" = col)
}

campus_dp_ssc_p <- camp_selection("ssc_p")
campus_dp_hsc_p <- camp_selection("hsc_p")
campus_dp_degree_p <- camp_selection("degree_p")
campus_dp_etest_p <- camp_selection("etest_p")
campus_dp_mba_p <- camp_selection("mba_p")
campus_dp_salary <- camp_selection("salary")

t1 <- t.test(value ~ status, campus_dp_ssc_p)$p.value
t2 <- t.test(value ~ status, campus_dp_hsc_p)$p.value
t3 <- t.test(value ~ status, campus_dp_degree_p)$p.value
t4 <- t.test(value ~ status, campus_dp_etest_p)$p.value
t5 <- t.test(value ~ status, campus_dp_mba_p)$p.value

campus_t_test <- as_tibble(data.frame(index = c("ssc_p", "hsc_p", "degree_p", "etest_p", "mba_p"),
                     value = c(t1, t2, t3, t4, t5)))

campus_t_test$important <- ifelse(campus_t_test$value >= 0.05, "No", "Yes")

campus_t_test

campus_dp_boxplot <- rbind(campus_dp_ssc_p, campus_dp_hsc_p, campus_dp_degree_p,
                           campus_dp_etest_p, campus_dp_mba_p, campus_dp_salary)

ggplot(campus_dp_boxplot, aes(x = status, y = value, fill = index))+
  geom_boxplot(na.rm = T)+
  facet_wrap(~ index, scales = "free")+
  scale_fill_discrete(labels = c("Secondary Education\n percentage - 10th Grade",
                                 "Higher Secondary Education\n percentage- 12th Grade",
                                 "Degree percentage",
                                 "Employability test percentage\n( conducted by college)",
                                 "MBA percentage", "Salary"))+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(1, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Placement dependance from different indexes")
  
campus_dp_cor <- campus_dp %>%
  select(ssc_p, hsc_p, degree_p, etest_p, mba_p, salary) 

ggpairs(campus_dp_cor)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Correlation matrix")

ggplot(campus_dp_boxplot, aes(x = value, fill = index))+
  geom_histogram(colour = "black", na.rm = T)+
  facet_wrap(~ index, scales = "free")+
  scale_fill_discrete(labels = c("Secondary Education\n percentage - 10th Grade",
                                 "Higher Secondary Education\n percentage- 12th Grade",
                                 "Degree percentage",
                                 "Employability test percentage\n( conducted by college)",
                                 "MBA percentage", "Salary"))+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Indexes distribution")

shapiro.test(campus_dp$ssc_p^1.28)
shapiro.test(campus_dp$hsc_p^0.77)
shapiro.test(campus_dp$etest_p^(0.9))
shapiro.test(campus_dp$salary^(-2))

campus_dp_ssc_p2 <- camp_selection("ssc_p")
campus_dp_ssc_p2$value <- campus_dp_ssc_p2$value^1.28
campus_dp_hsc_p2 <- camp_selection("hsc_p")
campus_dp_hsc_p2$value <- campus_dp_hsc_p2$value^0.77
campus_dp_salary2 <- camp_selection("salary")
campus_dp_salary2$value <- campus_dp_salary2$value^(-2)

campus_dp_boxplot2 <- rbind(campus_dp_ssc_p2, campus_dp_hsc_p2, campus_dp_degree_p,
                           campus_dp_etest_p, campus_dp_mba_p, campus_dp_salary2)

ggplot(campus_dp_boxplot2, aes(x = value, fill = index))+
  geom_histogram(colour = "black", na.rm = T)+
  facet_wrap(~ index, scales = "free")+
  scale_fill_discrete(labels = c("Secondary Education\n percentage - 10th Grade",
                                 "Higher Secondary Education\n percentage- 12th Grade",
                                 "Degree percentage",
                                 "Employability test percentage\n( conducted by college)",
                                 "MBA percentage", "Salary"))+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Indexes normal distribution")

campus_chisq <- function(col){
  campus_dp_chisq <- campus_dp %>% 
    select(col, status)
  campus_dp_chisq <- table(campus_dp_chisq)
  fisher.test(campus_dp_chisq)
}

c1 <- campus_chisq("gender")$p.value
c2 <- campus_chisq("ssc_b")$p.value
c3 <- campus_chisq("hsc_b")$p.value
c4 <- campus_chisq("hsc_s")$p.value
c5 <- campus_chisq("degree_t")$p.value
c6 <- campus_chisq("workex")$p.value
c7 <- campus_chisq("specialisation")$p.value

campus_chisq_dp <- as_tibble(data_frame("index" = c("gender", "ssc_b", "hsc_b", "hsc_s", "degree_t",
            "workex", "specialisation"), "value" = c(c1, c2, c3, c4, c5, c6, c7)))

campus_chisq_dp$important <- ifelse(campus_chisq_dp$value >= 0.05, "No", "Yes")

campus_chisq_dp

campus_dp$status_log <- ifelse(campus_dp$status == "Not Placed", 0, 1)

levels(campus_dp$workex)
levels(campus_dp$specialisation)

campus_dp$workex_log <- ifelse(campus_dp$workex == "No", 0, 1)

campus_dp$specialisation_log <- ifelse(campus_dp$specialisation == "Mkt&Fin", 0, 1)


#logistic regression with ssc_p, hsc_p, degree_p, workex, specialisation

fit <- glm(status_log ~ ssc_p + hsc_p + degree_p + workex_log + specialisation_log, data = campus_dp, family = "binomial")
summary(fit)
fit$residuals
plot(fit$residuals)
fit$linear.predictors

predict.glm(object = fit, type = "response")
campus_dp$status

table(ifelse(predict.glm(object = fit, type = "response") >= 0.5, "Placed", "Not Placed"))
table(campus_dp$status)

campus_dp$predict <- predict.glm(object = fit, type = "response")
campus_dp$predictors <- fit$linear.predictors

ggplot(data = campus_dp ,aes(x = predictors, y = predict))+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", size = 1.5)+
  geom_line(lwd = 2, colour = "blue")+
  labs(y="probability", title="Placement probability")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-9, 9), breaks = seq(-9, 9, by = 3))
