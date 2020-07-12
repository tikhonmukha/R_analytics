library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

#Monthly sells report
montly_sell <- read.csv("SELL_1.csv", header = T, sep = ";", na.strings = c("","-"), dec = ",")
head(montly_sell)
str(montly_sell)
anyNA(montly_sell)
montly_sell <- na.omit(montly_sell)
monthly_sell_dp <- tibble(montly_sell)

monthly_sell_dp$Pname <- trimws(monthly_sell_dp$Pname, which = "left")

monthly_sell_dp <- monthly_sell_dp %>% 
  mutate_each(factor, PKod, Pgroup, Pname) %>% 
  rename(SKU = PKod, group = Pgroup, name = Pname, quantity = Pquantity,
         pur_netprice = pce_zn, pur_netval = pwa_zn, sale_netprice = pce_sn,
         sale_netval = pwa_sn, sale_grprice = pce_sb, sale_grval = pwa_sb,
         sal_share = pudzsb, margin = pmarza, unit_margin = pmarzajedn,
         sell_margin = pkwmarza, mar_sale = pudzmarza) %>% 
  mutate(day = str_sub(Date, 1, 2), month = str_sub(Date, 4, 5),
         year = str_sub(Date, 7, 10)) %>% 
  select(-Date) %>% 
  mutate(date = str_c(year, month, day, sep = "-")) %>% 
  select(-c(day, month, year)) %>% 
  mutate(date = as.Date(date))

monthly_sell_dp <- monthly_sell_dp[c(16, 1:15)]

#Products rotation

prod_rot <- read.csv("ROTATION_of_products01.01.2018-09.01.2019.csv", header = T, sep = ";", na.strings = c(" ", "", "-"), dec = ",")
head(prod_rot)
str(prod_rot)
anyNA(prod_rot)
prod_rot_dp <- tibble(prod_rot)

prod_rot_dp$Pname <- trimws(prod_rot_dp$Pname, which = "left")

prod_rot_dp <- prod_rot_dp %>% 
  mutate_each(factor, Pgroup, PKod, Pname) %>% 
  fill(Pgroup) %>% 
  rename(group = Pgroup, obs_num = Lp, SKU = PKod, name = Pname) %>% 
  na.omit(prod_rot_dp)

levels(prod_rot_dp$name)
#Daily sell
daily_sell <- read.csv("Day_sell_24_12_18.csv", header = T, sep = ";", na.strings = "", dec = ",")
head(daily_sell)
str(daily_sell)
anyNA(daily_sell)
daily_sell <- na.omit(daily_sell)
daily_sell_dp <- tibble(daily_sell)

daily_sell_dp <- daily_sell_dp %>% 
  rename(date = Date, pur_net = zn, sale_gross = sb, sell_tax = tax,
         margin = marza) %>% 
  mutate(day = str_sub(date, 1, 2), month = str_sub(date, 4, 5),
         year = str_sub(date, 7, 10)) %>% 
  select(-date) %>% 
  mutate(date = str_c(year, month, day, sep = "-")) %>% 
  select(-c(day, month, year)) %>% 
  mutate(date = as.Date(date))

daily_sell_dp <- daily_sell_dp[c(5, 1:4)]

#Sale analytics

ggplot(daily_sell_dp, aes(x = date, y = sale_gross))+
  geom_line(colour = "red")+
  geom_smooth(method = "lm")+
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2019-01-01"), by = "1 month"), labels = date_format("%Y %b"))+
  scale_y_continuous(breaks = seq(0, 7000, 1000), limits = c(0, 6500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(title = "Gross sales value dynamics", x = "Date", y = "Sales value")

ggplot(daily_sell_dp, aes(x = date, y = pur_net))+
  geom_line(colour = "blue")+
  geom_smooth(method = "lm", colour = "red")+
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2019-01-01"), by = "1 month"), labels = date_format("%Y %b"))+
  scale_y_continuous(breaks = seq(0, 7000, 1000), limits = c(0, 6500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(title = "Net purchase dynamics", x = "Date", y = "Net purchase")

ggplot(daily_sell_dp, aes(x = date, y = margin))+
  geom_line(colour = "green")+
  geom_smooth(method = "lm")+
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2019-01-01"), by = "1 month"), labels = date_format("%Y %b"))+
  scale_y_continuous(breaks = seq(0, 7000, 1000), limits = c(0, 6500))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  labs(title = "Margin dynamics", x = "Date", y = "Margin")

nlevels(monthly_sell_dp$group)
ggplot(monthly_sell_dp, aes(x = date, y = pur_netval, fill = group))+
  geom_col()+
  scale_x_date(breaks = seq(as.Date("2018-01-01"), as.Date("2019-01-01"), by = "3 month"), labels = date_format("%b %y"))+
  facet_wrap(~ group, scales = "free")+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.85)),
        strip.text = element_text(size = rel(0.6)))+
  labs(title = "Categories sales dynamics", x = "", y = "Net purchase value")
