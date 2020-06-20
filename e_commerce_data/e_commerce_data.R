library(data.table)
library(stringr)
library(ggplot2)
library(ggrepel)

orders_list <- fread("List of Orders.csv", na.strings = "")
anyNA(orders_list)
orders_list <- na.omit(orders_list)
str(orders_list)
setnames(orders_list, c("Order ID", "Order Date"), c("OrderID", "OrderDate"))
orders_list <- orders_list[, OrderID := as.factor(OrderID)]
orders_list <- orders_list[, CustomerName := as.factor(CustomerName)]
orders_list <- orders_list[, State := as.factor(State)]
orders_list <- orders_list[, City := as.factor(City)]
orders_list$day <- substr(orders_list$OrderDate, 1, 2)
orders_list$month <- substr(orders_list$OrderDate, 4, 5)
orders_list$year <- substr(orders_list$OrderDate, 7, 10)
orders_list$OrderDate <- sprintf("%s-%s-%s", orders_list$year, orders_list$month, orders_list$day)
orders_list <- orders_list[, OrderDate := as.Date(OrderDate)]
orders_list$day <- NULL
orders_list$month <- NULL
orders_list$year <- NULL
setkey(orders_list, OrderID)

order_details <- fread("Order Details.csv")
anyNA(order_details)
str(order_details)
setnames(order_details, c("Order ID", "Sub-Category"), c("OrderID", "SubCategory"))
order_details <- order_details[, OrderID := as.factor(OrderID)]
order_details <- order_details[, Category := as.factor(Category)]
order_details <- order_details[, SubCategory := as.factor(SubCategory)]
setkey(order_details, OrderID)
setkey(order_details, Category)

sales_target <- fread("Sales target.csv")
anyNA(sales_target)
str(sales_target)
setnames(sales_target, c("Month of Order Date"), c("OrderMonthYear"))
sales_target <- sales_target[, Category := as.factor(Category)]
setkey(sales_target, Category)
sales_target$month <- substr(sales_target$OrderMonthYear, 1, 3)
sales_target$year <- substr(sales_target$OrderMonthYear, 5, 6)
sales_target$year <- sprintf("20%s", sales_target$year)

sales_target$month <- str_replace_all(sales_target$month, "Jan", "01")
sales_target$month <- str_replace_all(sales_target$month, "Feb", "02")
sales_target$month <- str_replace_all(sales_target$month, "Mar", "03")
sales_target$month <- str_replace_all(sales_target$month, "Apr", "04")
sales_target$month <- str_replace_all(sales_target$month, "May", "05")
sales_target$month <- str_replace_all(sales_target$month, "Jun", "06")
sales_target$month <- str_replace_all(sales_target$month, "Jul", "07")
sales_target$month <- str_replace_all(sales_target$month, "Aug", "08")
sales_target$month <- str_replace_all(sales_target$month, "Sep", "09")
sales_target$month <- str_replace_all(sales_target$month, "Oct", "10")
sales_target$month <- str_replace_all(sales_target$month, "Nov", "11")
sales_target$month <- str_replace_all(sales_target$month, "Dec", "12")

sales_target$OrderMonthYear <- sprintf("%s-%s", sales_target$month, sales_target$year)
sales_target$month <- NULL
sales_target$year <- NULL

###Most sales active states

order_details_states <- order_details[,1:4][,.(tot_amount = sum(Amount),
                                                tot_profit = sum(Profit),
                                                tot_quantity = sum(Quantity)),
                                             by = OrderID]
states <- orders_list[order_details_states]
states <- states[,c(1,4:8)]
states_activity <- states[,.(Amount = sum(tot_amount),
           Profit = sum(tot_profit),
           Quantity = sum(tot_quantity)),
        by = State]
setorder(states_activity, -Amount)

ggplot(states_activity, aes(x = reorder(State, -Amount), y = Amount, fill = Amount))+
  geom_col()+
  scale_fill_distiller(palette = "Spectral")+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(1, "cm"),
        legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "States purchase activity", x = "State", y = "Sales amount")

ggplot(states_activity, aes(x = Amount, y = Profit, size = Quantity))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_point(aes(colour = State))+
  geom_text_repel(aes(label = State), size = 3.25)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Revenue-Profit scatter plot", x = "Revenue", y = "Profit", size = "Transactions quantity")+
  guides(colour = FALSE)

states_dynamics <- orders_list[order_details_states]
states_dynamics <- states_dynamics[,c(2,4,6)]
states_dynamics$MonthYear <- format(as.Date(states_dynamics$OrderDate), "%Y-%m")
states_dynamics <- states_dynamics[,.(Amount = sum(tot_amount)),
                          by = c("State", "MonthYear")]
states_dynamics$MonthYear <- as.factor(states_dynamics$MonthYear)

ggplot(states_dynamics, aes(x = MonthYear, y = Amount, color = State))+
  geom_line(aes(group = 1), size = 1.2)+
  facet_wrap(~ State, scales = "free")+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.95)))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "States revenue dynamics", x = "Period", y = "Revenue")
