library(tidyverse)
library(lintr)
library(styler)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)
library(plotly)

# import data from csv
market_sales <- read_delim("sales_data_2017_2018.csv")

# select important columns and drop NA values
products <- market_sales %>% 
  select(date, item_name, main_category, unit_buying_price, unit_selling_price, total_profit)

products <- drop_na(products)

# date column becomes a month column
temp <- unlist(strsplit(products$date, " "))
temp <- temp[
  (1:length(temp))[seq(1, length(temp), 3)]
]
temp2 <- unlist(strsplit(temp, "/"))
temp2 <- temp2[
  (1:length(temp2))[seq(1, length(temp2), 3)]
]

products[1] = temp2

colnames(products)[1] = "month"

# summary stats
avg_stats <- products %>% 
  group_by(month, main_category) %>% 
  summarize(avg_buying_price = mean(unit_buying_price),
            avg_selling_price = mean(unit_selling_price),
            avg_total_profit = mean(total_profit)) %>% 
  arrange(main_category, as.numeric(month))

# The highest selling products by month and category and corresponding graph
high_sell_products <- avg_stats %>% group_by(main_category) %>% 
  filter(avg_selling_price == max(avg_selling_price)) %>% 
  arrange(main_category, as.numeric(month))

max_price_graph <- high_sell_products %>%
                   ggplot(aes(x = main_category, y = avg_selling_price)) + 
                   geom_col(fill="gray19", col="gray19") +
                   xlab("category") + ylab("max_sell_price")
ggplotly(max_price_graph)

# The least selling products by month and category and corresponding graph
least_sell_products <- avg_stats %>% group_by(main_category) %>% 
  filter(avg_selling_price == min(avg_selling_price)) %>% 
  arrange(main_category, as.numeric(month))

min_price_graph <- least_sell_products %>% 
                   ggplot(aes(x = main_category, y = avg_selling_price)) + 
                   geom_col(fill="gray19", col="gray19") +
                   xlab("category") + ylab("min_sell_price")
ggplotly(min_price_graph)


# graph high and low points for each category on the same graph for easy comparison
combined_plot <- ggplot(mapping=aes(x=main_category, y=avg_selling_price)) +               
  geom_point(data = high_sell_products, 
             color = "blue", 
             ) +
  geom_point(data = least_sell_products, 
             color = "firebrick3",
             ) +
  labs(x = "category", y = "avg selling price")

ggplotly(combined_plot)