library(tidyverse)
library(tidycensus)
library(tmap)
library(viridisLite)
library(sf)
library(tigris)


### Step 1: Gather Turnstile Data
# First, we gather the new york subway turnstile data for 2019 and 2020. Each turnstile block in each station logs how
# many people have entered and exited the station in 4 hour intervals. In its raw form, it is difficult to link the 
# turnstile data with coordiantes of each station, since unique keys linking the datasets áre not available. Therefore,
# we used the processed datasets provided by Chris Whong, who completed the arduous process of manually linking datasets.
# See the following medium article, which explains his data wrangling process:
# https://medium.com/qri-io/taming-the-mtas-unruly-turnstile-data-c945f5f96ba0

turnstile_2019 <- read_csv("https://api.qri.cloud/get/nyc-transit-data/turnstile_daily_counts_2019/body.csv?all=true")
turnstile_2020 <- read_csv("https://api.qri.cloud/get/nyc-transit-data/turnstile_daily_counts_2020/body.csv?all=true")

# if qri api does not work, these local versions can also be used
#turnstile_2019 <- read_csv("input/mta_data_2019/body.csv")
#turnstile_2020 <- read_csv("input/mta_data_2020/body.csv") # This version only goes to December 26, 2020


# The combination of stop_name & daytime_routes produces a unique identifier for a subway stop  
ts2019_month <- turnstile_2019 %>% 
  mutate(entries_exits = entries + exits) %>% 
  select(stop_name, daytime_routes, gtfs_longitude, gtfs_latitude, date, entries, entries_exits) %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  group_by(stop_name, daytime_routes, month) %>% 
  summarise(avg_entries_exits = mean(entries_exits), 
            sum_entries = sum(entries, na.rm = TRUE),
            cnt_days = n_distinct(day),
            long = first(gtfs_longitude),
            lat = first(gtfs_latitude),
            ver_long_cnt = n_distinct(gtfs_longitude),
            ver_lat_cnt = n_distinct(gtfs_latitude))

# Verify and clean 2019 data
ver_long19 <- (sum(ts2019_month$ver_long_cnt) == length(ts2019_month$ver_long_cnt))
ver_lat19 <- (sum(ts2019_month$ver_lat_cnt) == length(ts2019_month$ver_lat_cnt))
print(paste0('VERIFY: each station has a single longitude: ', ver_long19))
print(paste0('VERIFY: each station has a single latitude: ', ver_lat19))

ts2019_month_clean <- ts2019_month %>%
  select(!starts_with('ver'))

# Aggregate 2020 Data
ts2020_month <- turnstile_2020 %>%  
  mutate(entries_exits = entries + exits) %>% 
  select(stop_name, daytime_routes, gtfs_longitude, gtfs_latitude, date, entries, entries_exits) %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  group_by(stop_name, daytime_routes, month) %>% 
  summarise(avg_entries_exits = mean(entries_exits),
            sum_entries = sum(entries, na.rm = TRUE), 
            cnt_days = n_distinct(day),
            long = first(gtfs_longitude),
            lat = first(gtfs_latitude),
            ver_long_cnt = n_distinct(gtfs_longitude),
            ver_lat_cnt = n_distinct(gtfs_latitude))


# Verify and clean 
ver_long20 <- (sum(ts2020_month$ver_long_cnt) == length(ts2020_month$ver_long_cnt))
ver_lat20 <- (sum(ts2020_month$ver_lat_cnt) == length(ts2020_month$ver_lat_cnt))
print(paste0('VERIFY: each station has a single longitude: ', ver_long20))
print(paste0('VERIFY: each station has a single latitude: ', ver_lat20))

ts2020_month_clean <- ts2020_month %>%
  select(!starts_with('ver'))

### 1c: Merge 2019 and 2020 data
ts_combi <- ts2019_month_clean %>% 
  inner_join(ts2020_month_clean,
             by = c('stop_name', 'daytime_routes', 'month'),
             suffix = c("_2019", "_2020")) %>% 
  mutate(ver_long_match = (long_2019 == long_2020),
         ver_lat_match = (lat_2019 == lat_2020))


# Verify and cleanup
ver_long_combi <- (sum(ts_combi$ver_long_match) == length(ts_combi$ver_long_match))
ver_lat_combi <- (sum(ts_combi$ver_lat_match) == length(ts_combi$ver_lat_match))
print(paste0('VERIFY: station longitude 2019 matches 2020: ', ver_long_combi))
print(paste0('VERIFY: station latitude 2019 matches 2020: ', ver_lat_combi))

ts_combi_clean <- ts_combi %>%
  select(!starts_with('ver')) %>% 
  rename(long = long_2019, lat = lat_2019) %>% 
  select(!c("lat_2020", "long_2020"))

# Plot monthly totals over whole system for 2019 vs. 2020
month_labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun','Jul','Aug','Sep','Oct','Nov','Dec')
ts_combi_clean  %>% 
  group_by(month) %>%
  summarise(sum_entries_2019_system = sum(sum_entries_2019, na.rm = TRUE),
            sum_entries_2020_system = sum(sum_entries_2020, na.rm = TRUE),
            cnt_days_2019 = max(cnt_days_2019),
            cnt_days_2020 = max(cnt_days_2020)) %>% 
  mutate('2019' = (sum_entries_2019_system / cnt_days_2019) / 1000000,
         '2020' = (sum_entries_2020_system / cnt_days_2020)  / 1000000) %>% 
  pivot_longer(c('2019','2020'), names_to = 'year', values_to = 'amt') %>%
  ggplot() + 
    geom_bar(stat = "identity", aes(x = month, y = amt, fill = year)) +
    facet_grid(rows = "year") + 
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()
  ) +
  scale_x_discrete(labels = month_labels) +
  labs(
    title="Average Daily Subway Entries by Month",
    subtitle="2019 vs. 2020",
    x = "Month", y = "Average Daily Subway Entries, in Millions")

# Turn long and latitute into a geometry  
ts_combi_sf <- ts_combi_clean %>% 
  mutate(remaining_ridership = avg_entries_exits_2020 / avg_entries_exits_2019 * 100) %>% 
  filter(remaining_ridership <= 100 ) %>% 
  st_as_sf(coords = c('long', 'lat')) %>%
  st_set_crs(4326) 

### Step 2: gather census data: 
# Using the census api, we downloaded the geometries for each census tract, as well as
# corresponding demographic information (e.g. income, race) 
# To use the census api, a api key must first be requested and inserted in the following code
# https://api.census.gov/data/key_signup.html
# census_api_key("YOUR KEY HERE", install = TRUE, overwrite = TRUE)
# Some interesting variables to examine:
# B08301 - MEANS OF TRANSPORTATION TO WORK
# B25006 - RACE OF HOUSEHOLDER
# C25055 - AGE OF HOUSEHOLDER
# B28003 / B28008 - PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# K200201 - RACE
# S1701 - POVERTY STATUS IN THE PAST 12 MONTHS

options(tigris_use_cache = TRUE)
ny_geometry <- get_acs(geography = "tract",
              state = "NY",
              county = c(5, 47, 61, 81, 85),
              geometry = TRUE,
              variables = c("B19013_001"),
              cb = FALSE,
              output = "tidy")

ny_data <- get_acs(geography = "tract",
              state = "NY",
              county = c(5, 47, 61, 81, 85),
              geometry = FALSE,
              variables = c("B19013_001", # median income
                            "B08301_001","B08301_002","B08301_010","B08301_021", #transport: total, car, pt
                            "B02001_001","B02001_002","B02001_003", "B02001_005","B02001_007", "B02001_008" # race: total, white, black, other, 2 or more asian,
                            ),
              cb = FALSE,
              output = "tidy") 

ny_data_clean <- ny_data %>%
  select(!moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  transmute(GEOID = GEOID,
         NAME = NAME,
         transport_pt = B08301_010 / B08301_001, 
         transport_car = B08301_002 / B08301_001,
         race_white = B02001_002 / B02001_001,
         race_black = B02001_003 / B02001_001,
         race_asian = B02001_005 / B02001_001,
         race_other = B02001_007 / B02001_001,
         race_2ormore = B02001_008 / B02001_001,
         income_median = B19013_001)

ny <- ny_geometry %>% 
  select(GEOID,geometry) %>% 
  inner_join(ny_data_clean)

# Erase water areas from shapefiles
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

water_061 <- area_water("NY", "061")
water_005 <- area_water("NY", "005")
water_047 <- area_water("NY", "047")
water_081 <- area_water("NY", "081")
water_085 <- area_water("NY", "085")

ny_erase <- st_erase(ny, water_061)
ny_erase <- st_erase(ny_erase, water_005)
ny_erase <- st_erase(ny_erase, water_047)
ny_erase <- st_erase(ny_erase, water_081)
ny_erase <- st_erase(ny_erase, water_085)

ny_demographics <- ny_erase


### STEP 3: Combine mta turnstile data with census demographic data


## STEP 3a: Plot Remaining Percentage of Subway Usage in April 2020 compared to April 2019
ts_combi_sf_04 <- filter(ts_combi_sf, month == "04") # Only look at April

lines <- st_read("input/subway_lines/lines.shp")
vir <- viridis(9)

tmap_mode("view")
tm_shape(ny_demographics) +
  tm_polygons(col = "income_median", 
              breaks = c(0, 50000, 100000, 150000, 250000),
              palette = vir, 
              alpha = 0.6, 
              border.alpha = 0.5, 
              title = "Median Household Income in 2019, USD") +
  tm_shape(lines)+
  tm_lines(col = "red", lwd = 1) +
  tm_shape(ts_combi_sf_04) +
  tm_symbols(col = "remaining_ridership", 
             breaks = c(0, 10, 20, 30, 100),
             palette = inferno(4, direction = -1),
             size = 0.05, 
             alpha = 1, 
             title.col = "Remaining Ridership")


## Step 3b: Map census information to subway stops
# This allows us to do regression analysis between the variables
pnts_trans <- st_transform(ts_combi_sf_04$geometry, 2163) # apply transformation to pnts sf
tt1_trans <- st_transform(ny_demographics, 2163)      # apply transformation to polygons sf

# intersect and extract tract name
ts_combi_sf_04$tract <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                            function(col) { 
                              tt1_trans[which(col), ]$NAME
                            })
# extract income
ts_combi_sf_04$income_median <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                     function(col) { 
                       tt1_trans[which(col), ]$income_median
                     })
ts_combi_sf_04$income_median = as.numeric(ts_combi_sf_04$income_median)

# extract pct white population
ts_combi_sf_04$race_white <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                                 function(col) { 
                                   tt1_trans[which(col), ]$race_white
                                 })

ts_combi_sf_04$race_white = as.numeric(ts_combi_sf_04$race_white)



# regression analysis
# Income
lm_income_lin <- lm(remaining_ridership ~ income_median, data = ts_combi_sf_04)
lm_income_log <- lm(remaining_ridership ~ log(income_median), data = ts_combi_sf_04)

predicted_income_lin <- data.frame(pred = predict(lm_income_lin, ts_combi_sf_04), estimate = ts_combi_sf_04$income_median)
predicted_income_log <- data.frame(pred = predict(lm_income_log, ts_combi_sf_04), estimate = ts_combi_sf_04$income_median)

ggplot() + 
  geom_point(ts_combi_sf_04,mapping = aes(income_median,remaining_ridership)) +
  labs(title = "Remaining Ridership (April) vs. Median Income",
       subtitle = "Regression Analysis",
       x = 'Median Household Income, 2019 (USD)',
       y = "Remaining Ridership, 2020 vs. 2019 (%)") +
  geom_line(color = 'red',data = predicted_income_lin, aes(x = estimate, y = pred)) +
  geom_line(color = 'blue',data = predicted_income_log, aes(x = estimate, y = pred)) +
  geom_text(mapping = aes(x = 150000, y = -3), color = 'red', size = 2.5, hjust = "left", vjust="bottom",
            label = paste("ridership ~ income",
                          "\nAdj R2 = ", signif(summary(lm_income_lin)$adj.r.squared, 5),
                          "\nIntercept =", signif(lm_income_lin$coef[[1]],5 ),
                          "\nP =", signif(summary(lm_income_lin)$coef[2,4], 5))) +
  geom_text(mapping=aes(x = 210000, y = 5), color = 'blue', size = 2.5, hjust = "left", vjust="bottom",
            label = paste("ridership ~ log(income)",
                          "\nAdj R2 = ", signif(summary(lm_income_log)$adj.r.squared, 5),
                          "\nIntercept =", signif(lm_income_log$coef[[1]],5 ),
                          "\nP =", signif(summary(lm_income_log)$coef[2,4], 5)))
  

# Race (in progress)
lm_white <- lm(remaining_ridership ~ race_white, data = ts_combi_sf_04)
summary(lm_white)
predicted_white <- data.frame(pred = predict(lm_white, ts_combi_sf_04), estimate = ts_combi_sf_04$race_white)


