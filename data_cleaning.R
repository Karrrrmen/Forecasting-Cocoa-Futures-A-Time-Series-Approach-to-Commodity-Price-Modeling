#### Workspace setup ####
library(dplyr)
library(lubridate)

#### Clean data ####
price <- read_csv(here::here("data/raw_data/Daily_Prices_ICCO.csv"))
climate <- read_csv(here::here("data/raw_data/Ghana_data.csv"))
production <- read_csv(here::here("data/raw_data/cocoa_bean_production.csv"))

# Replace blank values in PRCP with 0
climate$PRCP[is.na(climate$PRCP)] <- 0

# Convert the Date column in price_data to actual date format
price$Date <- as.Date(price$Date, format="%d/%m/%Y")

names(price)[names(price) == "ICCO daily price (US$/tonne)"] <- "USD"

# Remove the 'Code' column
production$Code <- NULL
# Rename the last column properly
colnames(production) <- c("Entity", "Year", "production_in_ton")
# Convert 'Year' to numeric
production$Year <- as.numeric(production$Year)
# Convert 'production_in_ton' to numeric
production$production_in_ton <- as.numeric(production$production_in_ton)

# Generate monthly average temperature and price
climate_data <- climate %>%
  mutate(YearMonth = format(as.Date(DATE, format="%Y-%m-%d"), "%Y-%m")) %>%
  group_by(YearMonth) %>%
  mutate(monthly_avg_temp = mean(TAVG, na.rm = TRUE)) %>%
  ungroup()

price_data <- price %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  mutate(monthly_avg_price = mean(`USD`, na.rm = TRUE)) %>%
  ungroup()

price_monthly <- price_data %>% 
  group_by(YearMonth) %>% 
  summarise(monthly_avg_price = mean(USD, na.rm = TRUE))

climate_monthly <- climate_data %>%
  group_by(YearMonth) %>%
  summarise(monthly_avg_temp = mean(TAVG, na.rm = TRUE),
            monthly_avg_precip = mean(PRCP, na.rm = TRUE))

cocoa_data <- right_join(price_monthly, climate_monthly, by="YearMonth")
cocoa_data <- na.omit(cocoa_data)
names(cocoa_data)[names(cocoa_data) == "monthly_avg_price"] <- "price"


# Extract 'Year' from 'YearMonth' in cocoa_data
cocoa_data$Year <- as.numeric(substr(cocoa_data$YearMonth, 1, 4))
# Merge datasets on 'Year'
cocoa_data <- cocoa_data %>%
  left_join(production, by = "Year") %>%
  select(-Entity) %>% # Remove 'Entity' since it's always "Ghana"
  select(YearMonth, Year, price, monthly_avg_temp, monthly_avg_precip, production_in_ton)


#### Save data ####
#write_csv(climate_data, "data/cleaned_data/climate_data.csv")
#write_csv(price_data, "data/cleaned_data/price_data.csv")
write_csv(cocoa_data, "data/cleaned_data/cocoa_data.csv")
#write_csv(production, "data/cleaned_data/production_data.csv")
