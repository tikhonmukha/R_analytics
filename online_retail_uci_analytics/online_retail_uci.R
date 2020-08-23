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