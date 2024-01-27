library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)
library(readxl)
library(DatawRappr)
library(purrr)
library(stringr)
library(ggplot2)

# Setup -------------------------------------------------------------------

#load all sales
sales2023 <- read_excel("AllSalesDuval2023.xlsx") %>% clean_names()

#clean up
sales2023$zip_code <- str_sub(sales2023$zip_code, end=5)
sales2023[is.na(sales2023)] <- ""

#break out use column to code
sales2023 <- sales2023 %>% separate_wider_delim(property_use, delim=" - ", names=c("code","code_meaning"))

#load property uses
property_uses <- read_excel("duval-county-property-uses.xlsx")
property_industrial <- property_uses %>% filter(Category == "Industrial")
property_residential <- property_uses %>% filter(Category == "Residential")


# Industrial --------------------------------------------------------------

#break out industrial sales
industrial_sales <- sales2023 %>% filter(code %in% property_industrial$Code)

#Group sales
sum_industrial_sales <- industrial_sales %>% select(real_estate_number, owner, street_number, street, street_type, street_direction, city, zip_code, sale_date, sale_price)

sum_industrial_sales <- sum_industrial_sales %>%
  distinct()
sum_industrial_sales <- sum_industrial_sales %>% 
  arrange(desc(sale_price))

#Creates list
list_industrial_sales <- sum_industrial_sales %>%
  mutate(address = paste(street_number, street, street_type, street_direction)) %>%
  relocate(address, .after="owner") %>%
  subset(select = -c(street_number, street, street_type, street_direction)
         )
write_excel_csv(list_industrial_sales,"2023_industrial_sales.csv")


# Single Family -----------------------------------------------------------

#break out single family sales
single_family_sales <- sales2023 %>% filter (code == "0100")

#figure out zip code breakdown
home_sales_zips <- single_family_sales %>% 
  group_by(zip_code) %>% 
  summarise(
    homes_sold = n(),
    average_price = mean(sale_price)) %>%
  arrange(desc(average_price)
  )

write_excel_csv(home_sales_zips,"2023_homes_zip.csv")
