library(dplyr)
library(ggplot2)
library(stringr)
library(rgl)

cluster <- read.csv("cluster_analysis.csv", header = T, stringsAsFactors = T, na.strings = "NA", sep = ";", dec = ".")
head(cluster)
str(cluster)

cluster_dp <- tibble(cluster)

cluster_dp <- cluster_dp %>% 
  rename("IF_FLAV"="IF.FLAVOURED", "VOLUME_SALES"="VOLUME.SALES",
         "VALUE_SALES"="VALUE.SALES", "PRICE_PER_UNIT"="PRICE.PER.UNIT",
         "NUM_DIST"="NUMERIC.SELLING.DISTRIBIUTION..AVG.",
         "WEI_DIST"="WEIGHTED.SELLING.DISTRIBIUTION..AVG.",
         "OFFTAKE"="Average.Sales.Per.Shop.Selling", "MANUFACTURER"="BRAND.OWNER")

cluster_plot_1 <- cluster_dp %>%
  filter(as.character(PERIOD) == "MAT APR'20") %>% 
  select("FAT","TASTE","VOLUME_SALES") %>% 
  group_by(FAT, TASTE) %>% 
  summarise(VOLUME_SALES = sum(VOLUME_SALES, na.rm = T)) %>%
  arrange(desc(VOLUME_SALES)) %>% 
  filter(as.character(FAT) != "PRIVATE LABEL") %>% 
  filter(as.character(TASTE) != "NOT AVAILABLE") %>% 
  head(100)

ggplot(cluster_plot_1, aes(x = reorder(TASTE, -VOLUME_SALES), y = FAT))+
  geom_point(aes(size = VOLUME_SALES), color = "blue", alpha = 0.5)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = rel(0.85)))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Scatter plot", x = "Taste", y = "Fat level")

cluster_plot_2 <- cluster_dp %>% 
  filter(as.character(PERIOD) == "MAT APR'20") %>%
  select("WEIGHT","TASTE","VOLUME_SALES") %>% 
  group_by(WEIGHT, TASTE) %>% 
  summarise(VOLUME_SALES = sum(VOLUME_SALES, na.rm = T)) %>% 
  arrange(desc(VOLUME_SALES)) %>% 
  filter(as.character(WEIGHT) != "PRIVATE LABEL") %>% 
  filter(as.character(TASTE) != "NOT AVAILABLE") %>% 
  head(100)

ggplot(cluster_plot_2, aes(x = reorder(TASTE, -VOLUME_SALES), y = WEIGHT))+
  geom_point(aes(size = VOLUME_SALES), color = "blue", alpha = 0.5)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, size = rel(0.85)))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Scatter plot", x = "Taste", y = "Weight")

cluster_plot_3 <- cluster_dp %>% 
  filter(as.character(PERIOD) == "MAT APR'20") %>%
  select("WEIGHT","FAT","VOLUME_SALES") %>% 
  group_by(WEIGHT, FAT) %>% 
  summarise(VOLUME_SALES = sum(VOLUME_SALES, na.rm = T)) %>% 
  arrange(desc(VOLUME_SALES)) %>% 
  filter(as.character(WEIGHT) != "PRIVATE LABEL") %>% 
  head(100)

ggplot(cluster_plot_3, aes(x = FAT, y = WEIGHT))+
  geom_point(aes(size = VOLUME_SALES), color = "blue", alpha = 0.5)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))+
  labs(title = "Scatter plot", x = "Fat level", y = "Weight")


#Cluster k-means

cluster_df <- cluster_dp %>% 
  filter(as.character(PERIOD) == "MAT APR'20") %>%
  select(BRAND, VOLUME_SALES, VALUE_SALES) %>% 
  group_by(BRAND) %>% 
  summarise(VOLUME_SALES = sum(VOLUME_SALES, na.rm = T), VALUE_SALES = sum(VALUE_SALES, na.rm = T))

df <- cluster_df %>% select(VOLUME_SALES, VALUE_SALES)
fit <- kmeans(df, 3)
cluster_df$clusters <- factor(fit$cluster)

ggplot(cluster_df, aes(x = VOLUME_SALES, y = VALUE_SALES, col = clusters))+
  geom_point(size = 2, alpha = 0.5)+
  theme_minimal()+
  theme(legend.text = element_text(lineheight = .8), legend.key.height = unit(0.5, "cm"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  labs(title = "K-means clustering", x = "Volume sales", y = "Value sales")

cluster_df$clusters <- str_replace_all(cluster_df$clusters, "1", "Premium")
cluster_df$clusters <- str_replace_all(cluster_df$clusters, "2", "Middle")
cluster_df$clusters <- str_replace_all(cluster_df$clusters, "3", "Econom")

cluster_df <- cluster_df %>% arrange(desc(VOLUME_SALES))

#three-dimension plot

cluster_3d <- cluster_dp %>%
  filter(as.character(PERIOD) == "MAT APR'20") %>% 
  select(BRAND, VOLUME_SALES, VALUE_SALES, PRICE_PER_UNIT) %>% 
  group_by(BRAND) %>% 
  summarise(volume_sales = sum(VOLUME_SALES, na.rm = T), value_sales = sum(VALUE_SALES, na.rm = T),
            price_per_unit = mean(PRICE_PER_UNIT, na.rm = T)) %>% 
  arrange(desc(volume_sales)) %>% 
  head(10)

interleave <- function(v1, v2)  as.vector(rbind(v1,v2))

plot3d(cluster_3d$volume_sales, cluster_3d$value_sales, cluster_3d$price_per_unit,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size = 1, type = "s", lit = FALSE, fill = cluster_3d$BRAND)

segments3d(interleave(cluster_3d$volume_sales,   cluster_3d$volume_sales),
           interleave(cluster_3d$value_sales, cluster_3d$value_sales),
           interleave(cluster_3d$price_per_unit,  min(cluster_3d$price_per_unit)),
           alpha = 0.4, col = "blue")

# Draw the box.
rgl.bbox(color = "grey50",          # grey60 surface and black text
         emission = "grey50",       # emission color is grey50
         xlen = 0, ylen = 0, zlen = 0)  # Don't add tick marks

# Set default color of future objects to black
rgl.material(color = "black")

# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges = c("x--", "y+-", "z--"),
       ntick = 6,                       # Attempt 6 tick marks on each side
       cex = .75)                       # Smaller font

# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Volume sales",       edge = "x--", line = 2)
mtext3d("Value sales", edge = "y+-", line = 3)
mtext3d("Price per unit",          edge = "z--", line = 3)