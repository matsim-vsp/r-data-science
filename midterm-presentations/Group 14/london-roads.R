library(tidyverse)
library(dplyr)
library(readxl)

#### plot 1: Height restriction on bridges and underpasses ####
height_restrictions <- read_excel("height-restrictions-in-london.xlsx")
# Extract categories from text and convert to double:
height_restrictions <- mutate(height_restrictions, `height_temp` = str_replace_all(`Height restriction (m)`, c("Between " = "", "Up to 3.0" = "0")))
height_restrictions <- mutate(height_restrictions, height = as.double(str_sub(`height_temp`, 1, 3)))

ggplot(height_restrictions, mapping=aes(x = `Borough`, fill=factor(height))) + geom_bar() +
  theme(axis.text.x = element_text(angle = -90, hjust = 0.0, vjust = 0.2)) +
  ggtitle("Hillingdon has the most underpasses with less than three meters clearance") +
  scale_y_continuous("Number of underpasses") +
  guides(fill = guide_legend(title = "minimum\nclearance"))
# X axis text orientation: https://ggplot2-book.org/polishing.html#theme-axis

#### plot 2: number of street closures per London postcode area (E, EC, N, NW, SE, SW, W, WC) ####
street_closures <- read_excel("postcode-data.xlsx")
street_closures <- select(street_closures, `POSTCODE`)
# transform 6 character postal code into 1|2 character post area code:
street_closures <- mutate(street_closures, `postcode area` = str_replace(str_sub(`POSTCODE`,1,2),"\\d$", ""))
street_closures <- group_by(street_closures, `postcode area`)
street_closures <- summarise(street_closures, `closures per postcode area` = n())

# Postcode data taken from https://www.doogal.co.uk/UKPostcodes.php
postcodes <- read.csv("London_Postcodes.csv")
postcodes <- rename(postcodes, `postcode area` = `postcode_area`)
closures_codes <- inner_join(street_closures, postcodes, 
                             suffix = c(".x", ".y"))
closures_codes <- mutate(closures_codes, `affected postcode ratio` = `closures per postcode area` / 
                           (`active_postcodes` - `non.geographic_postcodes`))
ggplot(closures_codes, mapping = aes(x = `population`, y = `affected postcode ratio`,label = `postcode area` 
                                     , fill = population)) + geom_text() + 
  ggtitle("Eastern city of London has the highest road closure rate per postal code") +
  scale_x_continuous(labels = scales::label_comma()) + 
  scale_y_continuous("number of affected postal codes/number of postal codes in the area")
