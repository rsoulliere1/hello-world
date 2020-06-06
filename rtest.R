library(tidyverse)

as_tibble(iris)

tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)

tibble(
  x = 12, 
  y = 3, 
  z = x ^ 2 + y
)

tibble(
  x = 12,
  y = 3, 
  z = x ^ 2 + y
)

tb <- tibble(
  `test` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

#Create a tribble
testTR <- tribble(
  ~sh, ~g,
  43, 4, 
  54, 4
)


#Calculate average and add to tribble
testTRavg <- testTR %>% 
  mutate(avg = sh / g)

#print tribble
testTRavg

#Load data from file savw to tribble sales
sales <- read_csv("../data/sales_data.csv")

sales

#Select columns into tribble
salesinfo <- select(sales,Year,Country,Order_Quantity)

#print
print(salesinfo)

salesCanada <- filter(sales, Country=="Canada") 
print(salesCanada)

# filter by country then select
salesCanada <- sales %>%
  filter(Country == "Canada") %>%
  select(Year,Order_Quantity)
print(salesCanada)


# Filter by age and sort by date 
age <- sales %>% 
  filter(Customer_Age %in% c(19, 21)) %>% 
  arrange(Date)

age

# Filter by age and sort by date 
age <- sales %>% 
  filter(Customer_Age %in% c(19, 21)) %>% 
  arrange(desc(Date))

age

avgProfitperItem <- sales %>% 
  select(Date,Year,Unit_Cost,Order_Quantity, Unit_Price,Cost,Profit)  %>% 
  mutate(avgProfit = Order_Quantity / Profit)

avgProfitperItem

sales %>% 
  filter(Year == 2016) %>% 
  group_by(Country) %>% 
  summarise(avgQuantity = mean(Order_Quantity))

sales %>% 
  filter(Year == 2016) %>% 
  group_by(Country) %>% 
  summarise(medianQuantity = sum(Order_Quantity))

sales %>%
  group_by(Year) %>%
  count(Country, sort = TRUE) %>%
  arrange(Country)

sales %>%
  group_by(Age_Group)

## keeping all data but "filtering" after a certain condition
# calculate GDP only for people with a life expectation above 50
sales %>%
  mutate(avgProfit = ifelse(Order_Quantity > 10, Order_Quantity / Profit, NA)) 
