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

campus_chisq("gender")
campus_chisq("ssc_b")
campus_chisq("hsc_b")
campus_chisq("hsc_s")
campus_chisq("degree_t")
campus_chisq("workex")
campus_chisq("specialisation")
