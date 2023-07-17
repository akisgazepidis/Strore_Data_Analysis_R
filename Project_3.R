library(ggplot2)
library(dplyr)
library("data.table")  

categories_df = read.csv("C:\\Users\\Intel PC G5900\\Desktop\\Data\\Categories.csv",row.names=NULL, encoding="UTF-8",sep=";", stringsAsFactors=FALSE,header = TRUE)
sales_464_df = read.csv("C:\\Users\\Intel PC G5900\\Desktop\\Data\\Sales_464.csv",sep=";")
sales_493_df = read.csv("C:\\Users\\Intel PC G5900\\Desktop\\Data\\Sales_493.csv",sep=";")

# Print the dimensions of the three dataframes

print(dim(categories_df))
print(dim(sales_464_df))
print(dim(sales_493_df))

# Print top 10 rows of each dataframe

print(head(categories_df , 5))
print(head(sales_464_df , 5))
print(head(sales_493_df , 5))

# Print the column names of each dataframe

print(colnames(categories_df))
print(colnames(sales_464_df))
print(colnames(sales_493_df))

#Print the types of each dataframe

print(sapply(categories_df, typeof))
print(sapply(sales_464_df, typeof))
print(sapply(sales_493_df, typeof))

# Group by ean and sum quantity and price

agg_tbl <- sales_464_df %>% group_by(ean) %>% 
  summarise(total_price=sum(price),
            total_quantity = sum(quantity),.groups = 'drop')

sales_464_grouped_df <- agg_tbl %>% as.data.frame()

# Sort by quantity in descending order and keep the top 20% of quantity sales

sales_464_sortedbyQyantity_df = sales_464_grouped_df[order(-sales_464_grouped_df$total_quantity),]
sales_464_20_perc = head(sales_464_sortedbyQyantity_df, round(dim(sales_464_sortedbyQyantity_df)[1] *0.2))

# Keep from the initial dataframe only the codes of the ean column after keeping only the top 20%

list= sales_464_20_perc$ean
filtered_sales_464_df = sales_464_df[sales_464_df$ean %in% list ,]

# Group by date and sum prices in the filtered dataframe and sort by date in ascending order

agg_tbl <- filtered_sales_464_df %>% group_by(date_trns) %>% 
  summarise(total_price=sum(price),.groups = 'drop')

sales_464_grouped_df <- agg_tbl %>% as.data.frame()
sales_464_sorted_df = sales_464_grouped_df[order(sales_464_grouped_df$date_trns),]

# Convert date_trns from character to date

sales_464_sorted_df$date_trns <- as.Date(sales_464_sorted_df$date_trns, "%Y-%m-%d")

# Create and print scatterplot

p <- ggplot(data = head(sales_464_sorted_df, 50), aes(date_trns, total_price)) +
      geom_point() +
    theme(axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1))+
      ylim(0, 20000)


show(p)

# Convert date_trns from character to date to inital dataframe

sales_464_df$date_trns = as.Date(sales_464_df$date_trns, "%Y-%m-%d")

#Create a column for month and week number of the date 

sales_464_df$month =format(sales_464_df$date_trns, "%b")
sales_464_df$weekNum =format(sales_464_df$date_trns, "%V")

# Print the minimum and maximum date. The data concern 1 year

#print(min(sales_464_grouped_month_df$date_trns))
#print(min(sales_464_grouped_month_df$date_trns))

# Group by month in the dataframe and sum prices and quantity

agg_tbl <- sales_464_df %>% group_by(month) %>% 
  summarise(total_price=sum(price),
            total_quantity = sum(quantity),.groups = 'drop')

sales_464_grouped_month_df <- agg_tbl %>% as.data.frame()

# Create and print barplot

p<-ggplot(sales_464_grouped_month_df, aes(x=month, y=total_price)) + 
  geom_bar(stat = "summary", fun = "median")+
  scale_y_continuous(name="Monthly revenue of store", labels = scales::comma)

show(p)

