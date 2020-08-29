library(data.table)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(stringr)

data <- fread("online_retail_uci.csv", header = T, sep = ",", 
              dec = ".", stringsAsFactors = T, 
              na.strings = c("","?","? sold as sets?","??","?? missing","???",
                             "?????","????damages????","????missing","?display?",
                             "?lost","?missing","?sold as sets?","?sold individually?",
                             "check","CHECK","check?","checked"))
str(data)
head(data)
tail(data)
anyNA(data$Description)
data <- na.omit(data)

data[,InvoiceDate := as.Date(InvoiceDate)][,`Customer ID` := factor(`Customer ID`)][,Description := str_replace(Description, "\\*","")][,Description := trimws(Description)][,Description := factor(Description)]
levels(data$Description)
data[,ValueSales := Quantity*Price]

data_date <- data[,(c=sum(ValueSales)),by=InvoiceDate]
setnames(data_date, "V1", "Agg_Values_Sales")

ggplot(data_date, aes(x = InvoiceDate, y = Agg_Values_Sales))+
  geom_line(colour = "blue")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1, 
                                   vjust = 1, size = rel(0.85)))+
  scale_y_continuous(breaks = seq(0, 100000, by = 20000), labels = seq(0, 100000, by = 20000))+
  scale_x_date(breaks = "2 month")+
  labs(title = "Daily sales dynamics", x = "Invoice date", y = "Aggregate sales value")

data_country <- data[,(c=sum(ValueSales)),by=Country]
setnames(data_country, "V1", "Agg_Values_Sales")
setorder(data_country, -Agg_Values_Sales)
data_country$share_per_country <- round((data_country$Agg_Values_Sales/sum(data_country$Agg_Values_Sales))*100, digits = 2)
data_country <- head(data_country, 10)

ggplot(data_country, aes(x = reorder(Country, -share_per_country), y = share_per_country, fill = share_per_country))+
  geom_col()+
  geom_text_repel(aes(label = share_per_country), size = 3.5, vjust = -1)+
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow"))+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 30, hjust = 1, 
                                   vjust = 1),
        legend.position = "bottom")+
  labs(title = "Top 10 countries by market share", x = "", y = "Market share per country",
       fill = "Market share value")

data_goods <- data[,(c=sum(ValueSales)),by=Description]
setnames(data_goods, "V1", "Agg_Values_Sales")
setorder(data_goods, -Agg_Values_Sales)
data_goods$share_per_country <- round((data_goods$Agg_Values_Sales/sum(data_goods$Agg_Values_Sales))*100, digits = 2)
data_goods <- head(data_goods, 20)

ggplot(data_goods, aes(x = reorder(Description, -share_per_country), y = share_per_country, fill = share_per_country))+
  geom_col()+
  geom_text_repel(aes(label = share_per_country), size = 3.5, vjust = -1)+
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow"))+
  scale_y_continuous(limits = c(0,2))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 60, hjust = 1, 
                                   vjust = 1, size = rel(0.85)),
        legend.position = "bottom")+
  labs(title = "Top 20 SKU by market share (including seasonality)", x = "", y = "Market share per SKU",
       fill = "Market share value")

#топ-20 скю исключая сезонные и новинки, abc и xyz анализ
data_goods_adj <- data[,c(3,5,9)]
data_goods_adj$yearmonth <- format(data_goods_adj$InvoiceDate, "%Y-%m")
data_goods_adj_agg <- dcast(data_goods_adj, Description ~ as.character(yearmonth), fun.aggregate = sum, value.var = c("ValueSales"))                                    
data_goods_adj_agg$value_sales <- apply(data_goods_adj_agg[,2:ncol(data_goods_adj_agg)],1,sum)

data_goods_adj_agg$new <- ifelse(apply(data_goods_adj_agg[,2:25],1,sum) == 0, 1, 0)
table(data_goods_adj_agg$new)

data_goods_adj_agg$spring <- ifelse(apply(data_goods_adj_agg[,c(5:7,17:19)],1,sum) > 0 &
                                      apply(data_goods_adj_agg[,-c(1,5:7,17:19)],1,sum) == 0, 1, 0)
table(data_goods_adj_agg$spring)

data_goods_adj_agg$summer <- ifelse(apply(data_goods_adj_agg[,c(8:10,20:22)],1,sum) > 0 &
                                      apply(data_goods_adj_agg[,-c(1,8:10,20:22)],1,sum) == 0, 1, 0)
table(data_goods_adj_agg$summer)

data_goods_adj_agg$autumn <- ifelse(apply(data_goods_adj_agg[,c(11:13,23:25)],1,sum) > 0 &
                                      apply(data_goods_adj_agg[,-c(1,11:13,23:25)],1,sum) == 0, 1, 0)
table(data_goods_adj_agg$autumn)

data_goods_adj_agg$winter <- ifelse(apply(data_goods_adj_agg[,c(2:4,14:16,26)],1,sum) > 0 &
                                      apply(data_goods_adj_agg[,-c(1:4,14:16,26)],1,sum) == 0, 1, 0)
table(data_goods_adj_agg$winter)

data_goods_adj_agg$empty_months <- ifelse(apply(data_goods_adj_agg[,-c(1,27:32)],1, function(x) sum(x == 0)) > 19, 1, 0)
table(data_goods_adj_agg$empty_months)

ncol(data_goods_adj_agg)

data_goods_abcxyz <- data_goods_adj_agg[new != 1,][spring != 1,][summer != 1,][autumn != 1,][winter != 1,][empty_months != 1,][value_sales >= 0,c(1:27)]
setorder(data_goods_abcxyz, -value_sales)

data_goods_abcxyz$value_share <- data_goods_abcxyz$value_sales/sum(data_goods_abcxyz$value_sales)*100
data_goods_abcxyz$cumsum_share <- cumsum(data_goods_abcxyz$value_share)      
data_goods_abcxyz$variation <- (apply(data_goods_abcxyz[,c(2:26)],1,sd)/apply(data_goods_abcxyz[,c(2:26)],1,mean))*100

data_goods_abcxyz$abc_group <- ifelse(data_goods_abcxyz$cumsum_share <= 80, "A", 
                                      ifelse(data_goods_abcxyz$cumsum_share > 80 & data_goods_abcxyz$cumsum_share <= 95, 
                                             "B", "C"))
table(data_goods_abcxyz$abc_group)
data_goods_abcxyz$xyz_group <- ifelse(data_goods_abcxyz$variation <= 10, "X", 
                                      ifelse(data_goods_abcxyz$variation > 10 & data_goods_abcxyz$variation <= 25, 
                                             "Y", "Z"))
table(data_goods_abcxyz$xyz_group)
table(data_goods_abcxyz$abc_group, data_goods_abcxyz$xyz_group)

data_goods_abcxyz$abcxyz_group <- str_c(data_goods_abcxyz$abc_group, 
                                        data_goods_abcxyz$xyz_group, sep = "")

ggplot(data_goods_abcxyz, aes(x = variation, y = value_share, colour = abcxyz_group))+
  geom_point(alpha = I(0.5))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+
  labs(title = "ABC-XYZ analysis", x = "Variation coefficient, %", 
       y = "SKU market share, %", colour = "SKU group")

data_goods_AZ <- data_goods_abcxyz[abcxyz_group == "AZ", -c(31:33)]

#drilling into AZ group: top sku with min variation, putting them into groups, then pivot table into long format & variation boxplots for top 20-30 sku, months dynamics sells for top 20-30 sku
