library(data.table)
library(stringr)
library(ggplot2)
library(ggrepel)
library(maps)
library(mapproj)

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
states_dynamics$MonthYear <- format(states_dynamics$OrderDate, "%Y-%m")
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

###Most revenuable cities

map <- data.table(map_data("world"))[region == "India"]
data <- data.table(world.cities)[country.etc == "India"]
str(data)
cities_dynamics <- orders_list[order_details_states]
cities_dynamics <- cities_dynamics[,c(5,6)]
cities_dynamics <- cities_dynamics[,.(Amount = sum(tot_amount)),
                                   by = c("City")]

cities <- data[cities_dynamics, on = .(name = City)]
cities$name <- as.factor(cities$name)
cities[name == "Ahmedabad"]$lat <- 23.03
cities[name == "Ahmedabad"]$long <- 72.59
cities[name == "Kolkata"]$lat <- 22.57
cities[name == "Kolkata"]$long <- 88.36
cities[name == "Kashmir"]$lat <- 34.08
cities[name == "Kashmir"]$long <- 74.80
cities[name == "Lucknow"]$lat <- 26.85
cities[name == "Lucknow"]$long <- 80.95
cities[name == "Gangtok"]$lat <- 27.34
cities[name == "Gangtok"]$long <- 88.61
cities[name == "Goa"]$lat <- 15.50
cities[name == "Goa"]$long <- 73.83
cities[name == "Simla"]$lat <- 31.10
cities[name == "Simla"]$long <- 77.17
cities[name == "Mumbai"]$lat <- 19.08
cities[name == "Mumbai"]$long <- 72.88

str(cities)

ggplot() +
  geom_polygon(data = map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_point(data= cities, aes(x=long, y=lat, color=name, size = Amount), na.rm = T) +
  geom_text_repel(data = cities, aes(x=long, y=lat, label = name), size = 3.25) +
  scale_fill_distiller(palette = "Spectral") +
  scale_size_continuous(range=c(1,12)) +
  theme_minimal() + 
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  coord_map() +
  guides(colour = FALSE, size = guide_legend(title = "Revenue")) +
  labs(title = "Most revenueable cities", x = "Longitude", y = "Latitude")

#Cities cluster analysis

cities_clusters <- orders_list[order_details_states]
cities_clusters <- cities_clusters[,c(5,6,8)]
cities_clusters <- cities_clusters[,.(Amount = sum(tot_amount),
                                      Quantity = sum(tot_quantity)),
                                   by = c("City")]
cities_clusters_graph <- cities_clusters[,c(2,3)]
fit_clust <- kmeans(cities_clusters_graph, 3)
cities_clusters_graph$clusters <- factor(fit_clust$cluster)
cities_clusters <- cbind(cities_clusters$City, cities_clusters_graph)
setnames(cities_clusters, "V1", "City")

ggplot(cities_clusters, aes(x = Amount, y = Quantity, col = clusters))+
  geom_point(size = 2)+
  geom_text_repel(aes(label = City), size = 3)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  scale_colour_discrete(labels = c("Most active", "Middle", "Less active"))+
  labs(title = "Cities cluster plot", x = "Revenue", y = "Orders quantity")

###Plan-fact

order_details_planfact <- order_details[,1:5][,.(tot_amount = sum(Amount),
                                               tot_profit = sum(Profit),
                                               tot_quantity = sum(Quantity)),
                                            by = c("OrderID", "Category")]
planfact <- orders_list[order_details_planfact]
planfact <- planfact[,c(1,2,6,7)]
planfact <- planfact[,.(Amount = sum(tot_amount)),
                          by = c("OrderDate", "Category")]
planfact$Month <- format(planfact$OrderDate, "%m")
planfact$Year <- format(planfact$OrderDate, "%Y")
planfact$MonthYear <- str_c(planfact$Year, planfact$Month, sep = "-")
planfact <- planfact[,c(6,2,3)]
planfact <- planfact[,.(Fact = sum(Amount)),
                     by = c("MonthYear", "Category")]
planfact$MonthYear <- as.factor(planfact$MonthYear)

sales_target$Month <- str_sub(sales_target$OrderMonthYear, 1, 2)
sales_target$Year <- str_sub(sales_target$OrderMonthYear, 4, 7)
sales_target$MonthYear <- str_c(sales_target$Year, sales_target$Month, sep = "-")
sales_target <- sales_target[,c(6,2,3)]
sales_target$MonthYear <- as.factor(sales_target$MonthYear)

planfact <- planfact[sales_target, on = .(MonthYear = MonthYear, Category = Category)]
planfact$difference <- planfact$Fact - planfact$Target
planfact$color <- as.factor(ifelse(planfact$difference >= 0, 1, 0))

str(planfact)

ggplot(planfact, aes(x = MonthYear, y = difference, fill = color))+
  geom_col()+
  facet_wrap( ~ Category)+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))+
  labs(title = "Sales plan-fact report", x = "Period", y = "Sales plan implementation")
