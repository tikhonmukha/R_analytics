library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
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

monthly_sell_dp_abc <- monthly_sell_dp %>% 
  group_by(name) %>% 
  summarise(total_net_val = sum(pur_netval, na.rm = T)) %>% 
  arrange(desc(total_net_val)) %>% 
  filter(total_net_val != 0) %>% 
  mutate(value_share = total_net_val/sum(total_net_val)*100, 
         value_cum = cumsum(total_net_val),
         share_cum = cumsum(value_share),
         number = seq(1:nrow(monthly_sell_dp_abc)),
         category_abc = as.factor(case_when(
           share_cum >= 0 & share_cum <= 80 ~ "A",
           share_cum > 80 & share_cum <= 95 ~ "B",
           share_cum > 95 ~ "C"
         )))

ggplot(monthly_sell_dp_abc, aes(x = category_abc, y = value_share))+
  geom_col(aes(fill = category_abc))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

monthly_sell_dp_abc_agg <- monthly_sell_dp_abc %>% 
  group_by(category_abc) %>% 
  summarise(count = n())

monthly_sell_dp_abc_agg$fraction <- monthly_sell_dp_abc_agg$count / sum(monthly_sell_dp_abc_agg$count)

monthly_sell_dp_abc_agg$ymax <- cumsum(monthly_sell_dp_abc_agg$fraction)

monthly_sell_dp_abc_agg$ymin <- c(0, head(monthly_sell_dp_abc_agg$ymax, n=-1))

monthly_sell_dp_abc_agg$labelPosition <- (monthly_sell_dp_abc_agg$ymax + monthly_sell_dp_abc_agg$ymin) / 2

monthly_sell_dp_abc_agg$label <- paste0(monthly_sell_dp_abc_agg$category_abc, "\n value: ", monthly_sell_dp_abc_agg$count)

ggplot(monthly_sell_dp_abc_agg, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category_abc)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

monthly_sell_dp %>% 
  subset(date >= as.Date("2018-01-01") & date <= as.Date("2018-10-01")) %>% 
  group_by(name) %>% 
  summarise(total_net_val = sum(pur_netval, na.rm = T)) %>% 
  filter(total_net_val == 0) #if there any new products what weren't sold in last 10 months

monthly_sell_dp_xyz <- monthly_sell_dp %>% 
  filter(as.character(group) != "ICE_CREAMS_FROZEN") %>% 
  group_by(name) %>% 
  summarise(coef_var = sd(pur_netval, na.rm = T)/mean(pur_netval, na.rm = T)*100) %>% 
  arrange(coef_var) %>% 
  mutate(category_xyz = as.factor(case_when(
    coef_var >= 0 & coef_var <= 10 ~ "X",
    coef_var > 10 & coef_var <= 25 ~ "Y",
    coef_var > 25 ~ "Z"
         ))) %>% 
  filter(coef_var != 0)

table(monthly_sell_dp_xyz$category_xyz)

monthly_sell_dp_xyz_agg <- monthly_sell_dp_xyz %>% 
  group_by(category_xyz) %>% 
  summarise(count = n())

monthly_sell_dp_xyz_agg$fraction <- monthly_sell_dp_xyz_agg$count / sum(monthly_sell_dp_xyz_agg$count)

monthly_sell_dp_xyz_agg$ymax <- cumsum(monthly_sell_dp_xyz_agg$fraction)

monthly_sell_dp_xyz_agg$ymin <- c(0, head(monthly_sell_dp_xyz_agg$ymax, n=-1))

monthly_sell_dp_xyz_agg$labelPosition <- (monthly_sell_dp_xyz_agg$ymax + monthly_sell_dp_xyz_agg$ymin) / 2

monthly_sell_dp_xyz_agg$label <- paste0(monthly_sell_dp_xyz_agg$category_xyz, "\n value: ", monthly_sell_dp_xyz_agg$count)

monthly_sell_dp_xyz_agg[1,6] <- 0.9

ggplot(monthly_sell_dp_xyz_agg, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category_xyz)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

monthly_sell_dp_abcxyz <- inner_join(monthly_sell_dp_abc, monthly_sell_dp_xyz) %>% 
  select(name, category_abc, category_xyz) %>% 
  mutate(category = as.factor(str_c(category_abc, category_xyz)))

table(monthly_sell_dp_abcxyz$category_abc, monthly_sell_dp_abcxyz$category_xyz)

monthly_sell_dp_abcxyz_graph <- monthly_sell_dp_abcxyz %>% 
  group_by(category_abc, category_xyz) %>% 
  summarise(matches = n())

ggplot(monthly_sell_dp_abcxyz_graph, aes(x = category_abc, y = category_xyz, fill = matches))+
  geom_tile()+
  geom_text_repel(data = monthly_sell_dp_abcxyz_graph, aes(x = category_abc, y = category_xyz, label = matches), size = 5) +
  scale_fill_distiller(palette = "Spectral")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+
  labs(title = "ABC XYZ analysis", x = "", y = "")
