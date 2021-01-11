library("tidyverse")
library("readxl")
library("sf")
library("tmap")
library("RColorBrewer")


#read and prepare amount of inhabitants
excel_sheets("Einwohner.xlsx")
einwohner <- read_excel("Einwohner.xlsx", sheet="Inhalt")

#select last year
einwohner <- einwohner %>%
  select("Bezirke", "2019")

#fix columns names
einwohner <- rename(einwohner,"inhabitants" = '2019')
einwohner <- rename(einwohner,"district" = 'Bezirke')

#read and prepare corona cases
corona <- read_csv2("meldedatum_bezirk.csv")
corona <- corona %>%
  pivot_longer(c("Marzahn-Hellersdorf","Mitte","Neukoelln","Tempelhof-Schoeneberg","Friedrichshain-Kreuzberg","Pankow","Reinickendorf","Charlottenburg-Wilmersdorf","Spandau","Lichtenberg","Steglitz-Zehlendorf","Treptow-Koepenick"),
               names_to="district", values_to="cases")

corona_cases <- corona %>%
  group_by(district) %>%
  summarize(total_cases=sum(cases))

#replace oe with ö
corona_cases <- corona_cases %>%
  mutate(district = str_replace(district, "oe", "ö"))

#read and prepare crimes
excel_sheets("Fallzahlen&HZ 2012-2019.xlsx")
crimes <- read_excel("Fallzahlen&HZ 2012-2019.xlsx", sheet="Fallzahlen_2019", skip=3)
crimes <- rename(crimes,"district" = "Bezeichnung (Bezirksregion)"  )
crimes <- rename(crimes,"crimes_total" = "Straftaten \r\n-insgesamt-")
crimes <- select(crimes, "district", "crimes_total")


#join tables
data <- crimes %>%
  inner_join(einwohner, by="district") %>%
  inner_join(corona_cases, by="district")

#sum and normalize corona/crimes
data <- data %>%
  mutate(crimes_total_100k= crimes_total/inhabitants*100000,
         cases_total_100k= total_cases/inhabitants*100000) %>%
  select(district, crimes_total_100k, cases_total_100k, inhabitants)


#color palette from https://colorbrewer2.org/
myColors <- tibble(Color= c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))
myColors <- t(myColors)

#visualize corona cases vs crimes 
data %>% ggplot(aes(x=cases_total_100k, y=crimes_total_100k, color=district)) +
  geom_point(size=3)+
  geom_smooth(method="lm", color="blue", se=FALSE, size=1)+
  labs(title = "Linear model applied to corona cases and crimes rates", x="Corona cases per 100000 inhabitants", y="Crimes per 100000 inhabitants")+
  scale_colour_manual(values = myColors)

#load shap file for map
berlin <- st_read('shp-bezirke/Berlin_Bezirke.shp')

#join data to districts
berlin <- data %>%
  inner_join(berlin, by=c("district"=  "Gemeinde_n")) %>%
  rename("Crimes per 100000 inhabitants"=crimes_total_100k ) %>%
  rename("Corona cases per 100000 inhabitants"=cases_total_100k) %>%
  st_as_sf()

#visualize crimes
tm_shape(berlin) + 
  tm_borders() +
  tm_fill(col="Crimes per 100000 inhabitants")

#visualize corona cases
tm_shape(berlin) + 
  tm_borders() +
  tm_fill(col="Corona cases per 100000 inhabitants")

#calculate pearson correlation
cor(data$crimes_total_100k, data$cases_total_100k, method = c("pearson"))
