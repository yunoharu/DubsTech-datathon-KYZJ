library(tidyverse)
library(readr)
library(ggplot2)

market_sales <- read_delim("sales_data_2017_2018.csv")
unique(market_sales$main_category)
unique(market_sales$sub_category)

profit_sales <- market_sales %>% 
  group_by(main_category) %>% 
  summarize(total.sales=sum(total_selling_price),
            total.profit = sum(total_profit),
            ave.sales=mean(total_selling_price),
            ave.profit=mean(total_profit)) %>% 
  arrange(desc(total.sales)) 

profit_sales %>% 
  select(main_category,total.sales,total.profit) %>% 
  pivot_longer(-main_category, names_to = "key", values_to = "value") %>% 
  ggplot(aes(x=main_category, y=value, fill=factor(key)))+
  geom_bar(stat='identity')+
  labs(x="Category",y="Average Sales/Profit", fill="Sales/Profit")+
  scale_fill_discrete(labels = c("Average Sales", "Average Profit"))
  
profit_sales %>% 
  select(main_category,ave.sales,ave.profit) %>% 
  pivot_longer(-main_category, names_to = "key", values_to = "value") %>% 
  ggplot(aes(x=main_category, y=value, fill=factor(key)))+
  geom_bar(stat='identity')+
  labs(x="Category",y="Average Sales/Profit", fill="Sales/Profit")+
  scale_fill_discrete(labels = c("Average Sales", "Average Profit"))

pie(profit_sales$total.profit, profit_sales$main_category)

profit_sales %>% 
  select(main_category,ave.sales,ave.profit) %>% 
  pivot_longer(-main_category, names_to = "key", values_to = "value") %>% 
  ggplot(aes(x=main_category, y=value, fill=factor(key)))+
  geom_bar(stat='identity')+
  labs(x="Category",y="Average Sales/Profit", fill="Sales/Profit")+
  scale_fill_discrete(labels = c("Average Sales", "Average Profit"))

sales <- profit_sales %>% 
  select(main_category, ave.sales) %>% 
  ggplot(aes(x="", y=ave.sales, fill=main_category))+
  geom_bar(stat="identity", width=1)+
  labs(y="Average Sales", fill="Category")
sales_pie<-sales + coord_polar("y", start=0) + theme_void()
sales_pie
profit <- profit_sales %>% 
  select(main_category, ave.profit) %>% 
  ggplot(aes(x="", y=ave.profit, fill=main_category))+
  geom_bar(stat="identity", width=1)+
  labs(y="Average Sales", fill="Category")
profit_pie<-profit + coord_polar("y", start=0) + theme_void()
profit_pie

squirrel.data$month <-str_sub(squirrel.data$Date, 1, 2)
squirrel.data$day <-str_sub(squirrel.data$Date, 3, 4)
squirrel.data$year <-str_sub(squirrel.data$Date, 5, 8)

market_sales$month <- str_sub(market_sales$date, 1, 1)
market_sales$time <- str_sub(market_sales$date, -11)
market_sales$ampm <- str_sub(market_sales$time, -2)
market_sales$hour <- str_sub(market_sales$time, 1,2)
market_sales$hour.ampm <- paste0(market_sales$hour,market_sales$ampm)

time_profit <- market_sales %>% 
  group_by(hour.ampm) %>% 
  summarize(profit=sum(total_profit)) %>% 
  arrange(desc(profit))

market_sales %>% 
  group_by(main_category) %>% 
  summarize(gross_margin = sum(total_profit)/sum(total_selling_price))

market_sales %>% 
  group_by(main_category) %>% 
  summarize(total_sales=sum(total_selling_price))

market_sales %>% 
  group_by(main_category) %>% 
  summarize(total_loss=(1-sum(total_profit)/sum(total_selling_price))*100)
#The most profitable month by sales
market_sales %>% 
  group_by(month) %>% 
  summarize(profitability=sum(total_profit)/sum(total_selling_price)*100) %>% 
  ggplot(aes(x=month, y=profitability))+
  geom_col(fill="gray", col="black")+
  labs(x="Month", y="Profitability")
  
#What are the most efficient ways that the store can reduce losses
market_sales %>% 
  group_by(main_category) %>% 
  summarize(loss=(1-sum(total_profit)/sum(total_selling_price))*100) %>% 
  ggplot(aes(x=main_category, y=loss, fill=main_category))+
  geom_col()+
  labs(x="Category", y="Loss", fill="Category")

