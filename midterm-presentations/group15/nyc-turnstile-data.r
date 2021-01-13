library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(lubridate)

# load all necessary data
stationData = read_tsv("stations-modified-export")
stationBooths = read_xls("Remote-Booth-Station.xls")
lineColors = read_tsv("colors_separated.tsv")
turnstileData2020 <- read_csv("turnstile_200418.txt")
turnstileData2019 <- read_csv("turnstile_190420.txt")
boroughs <-
  st_read("borough_boundaries/geo_export_91e21a9f-5fb9-4146-83ba-0ae54aa6466a.shp")
subway_lines <-
  st_read("subway_lines/geo_export_f28282a8-f37a-4682-8135-368f54cf2e6f.shp")
subwy_colors <- read.csv("colors.csv")


# function to calculate mean entries for all stations
calculateWeeklyEntries <- function(data) {
  entryData <- data %>%
    select(`C/A`, UNIT, SCP, STATION, DATE, TIME, ENTRIES, EXITS) %>%
    mutate(DATE = as_date(DATE, format = "%m/%d/%Y")) %>%
    mutate(TIME = as.character(TIME)) %>%
    # move entries until 3 am to previous day due to irregular 4 hour reporting interval
    mutate(
      TIME = case_when(
        TIME == "00:00:00" ~ "24:00:00",
        TIME == "01:00:00" ~ "25:00:00",
        TIME == "02:00:00" ~ "26:00:00",
        TIME == "03:00:00" ~ "27:00:00",
        TRUE ~ TIME
      )
    ) %>%
    mutate(DATE = case_when(TIME > "23:59:59" ~ DATE - 1, TRUE ~ DATE)) %>%
    filter(DATE > min(DATE)) %>%
    group_by(`C/A`, UNIT, SCP, STATION, DATE) %>%
    summarise(entriesStart = min(ENTRIES),
              entriesEnd = max(ENTRIES)) %>%
    mutate(entryDiff = as.numeric(entriesEnd) - as.numeric(entriesStart)) %>%
    # filter abnormal values due to counter overflow or resetting
    filter(entryDiff < 10000) %>%
    group_by(STATION, DATE) %>%
    summarise(entryDiff = sum(entryDiff)) %>%
    group_by(STATION) %>%
    summarise(entryDiff = mean(entryDiff))
  return(entryData)
}

# compare 2019 with 2020 data, new column with percentage (2020 relative to 2019)
analysedData2020 <- calculateWeeklyEntries(turnstileData2020)
analysedData2019 <- calculateWeeklyEntries(turnstileData2019)

compareYears <- function(data1, data2) {
  compareResult <- data1 %>%
    inner_join(data2,
              by = c("STATION"),
              suffix = c(".year1", ".year2")) %>%
    mutate(percentage = round(entryDiff.year2 / entryDiff.year1 * 100, digits = 1))
  return(compareResult)
}

# prepare results table with matching station lat/lon

stationDataClean <- stationData %>%
  mutate(stationData, `Stop Name` = toupper(`Stop Name`))

comparison1920 <-
  compareYears(analysedData2019, analysedData2020) %>%
  left_join(stationDataClean, by = c("STATION" = "Stop Name")) %>%
  select(
    -c(
      Division,
      Line,
      Borough,
      `GTFS Stop ID`,
      `Daytime Routes`,
      Structure,
      `North Direction Label`,
      `South Direction Label`,
      ADA,
      `ADA Notes`
    )
  ) %>%
  group_by(STATION, percentage) %>%
  summarise(
    `GTFS Latitude` = max(`GTFS Latitude`),
    `GTFS Longitude` = min(`GTFS Longitude`)
  )

comparison_sf <-
  filter(comparison1920,
         !is.na(`GTFS Latitude`),
         !is.na(`GTFS Longitude`)) %>%
  st_as_sf(coords = c("GTFS Longitude", "GTFS Latitude"))

tmap_mode("plot")
tm_shape(boroughs) +
  tm_fill("boro_name", title = "Boroughs", palette = "Pastel2", n = 8) +
  tm_shape(subway_lines_colored) +
  tm_lines("url",lwd = 4, legend.col.show = FALSE, palette = "viridis", n=1) +
  tm_shape(comparison_sf) +
  tm_bubbles(size = "percentage", col = "percentage",  style="cont", palette="Spectral", legend.size.show=FALSE, title.col="Remaining Pax [%]") +
  tm_legend(legend.height=2000) 
  
