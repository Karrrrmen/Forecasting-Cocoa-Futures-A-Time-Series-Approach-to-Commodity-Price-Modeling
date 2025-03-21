#### Workspace setup ####
library(dplyr)
library(lubridate)

#### Clean data ####
price <- read_csv(here::here("data/raw_data/Daily_Prices_ICCO.csv"))
climate <- read_csv(here::here("data/raw_data/Ghana_data.csv"))

# Replace blank values in PRCP with 0
climate$PRCP[is.na(climate$PRCP)] <- 0

# Convert the Date column in price_data to actual date format
price$Date <- as.Date(price$Date, format="%d/%m/%Y")

names(price)[names(price) == "ICCO daily price (US$/tonne)"] <- "USD"


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

#### Save data ####
write_csv(climate_data, "data/cleaned_data/climate_data.csv")
write_csv(price_data, "data/cleaned_data/price_data.csv")
write_csv(cocoa_data, "data/cleaned_data/cocoa_data.csv")

