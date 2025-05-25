# "Workshop Climate Change and Fertility_ Firenze" -----

# author: "Itza A. Olguin Zuñiga , University of Bologna (POPCLIMA)"
# email: itza.olguinzuniga@unibo.it

# Let's start!!!

# Clear environment
rm(list=ls(all=TRUE))

# Install packages -------

# install.packages("sf")        #To work with spatial vector data in R
# install.packages("dplyr")     #To handle and visualize data
# install.packages("terra")     #To manipulate raster and vector data
# install.packages("ggplot")    #To graph data
# install.packages("mapview")   #To crate interactive maps
# install.packages("viridis")   #For color scales
# install.packages("exactextractr") #Extract values faster for raster data
# install.packages("stats")     #Package with different function for statistical anlysis
# install.packages("scales")    #Graphical scales map data to aesthetics

# Libraries
library(sf)
library(dplyr)
library(terra)
library(ggplot2)
library(mapview)
library(viridis)
library(exactextractr)
library(stats)
library(scales)

# Definition of the working directory (the folder where is your data) -----

setwd("~/POPCLIMA/workshop_fertility_CC/data")

# The geographical unit of analysis -------

# Open the shp file the NUTS0 countries named 'europe_cleaned_FR_ES_PT.shp'
# The shp file was created specially for this workshop.
# The original shp file can be downloaded from: 
# https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics

nuts0 <- sf::st_read("europe_cleaned_FR_ES_PT.shp") %>%
  st_make_valid()  # fix invalid geometries

# Graph
ggplot(data = nuts0) +
  geom_sf(fill = "lightblue", color = "gray30") +
  theme_minimal() 


# Working with fertility data ----

# Open the csv file with the monthly births 
euro_monthly_births<- read_csv("estat_demo_fmonth_en.csv")
# The original data set can be downloaded from: 
# https://ec.europa.eu/eurostat/databrowser/view/demo_fmonth__custom_16765251/default/table?lang=en

# Inspection
head(euro_monthly_births)

# Let's select the variables that are useful
monthly_births <- euro_monthly_births[,c(5:8)]
head(monthly_births)

# Create a table: Country name -> NUTS 0 code
name_to_nuts <- data.frame(
  geo = c("Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
          "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland",
          "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
          "Kosovo*", "Latvia", "Lithuania", "Luxembourg", "Malta", "Moldova",
          "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland",
          "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia", "Spain",
          "Sweden", "Switzerland", "Türkiye", "Ukraine"),
  NUTS_ID = c("AL", "AT", "BE", "BA", "BG", "HR", "CY", "CZ", "DK", "EE",
              "FI", "FR", "DE", "EL", "HU", "IS", "IE", "IT", "XK", "LV",
              "LT", "LU", "MT", "MD", "ME", "NL", "MK", "NO", "PL", "PT",
              "RO", "RS", "SK", "SI", "ES", "SE", "CH", "TR", "UA"),
  stringsAsFactors = FALSE
)

# Join the birth data with the NUTS_ID table
monthly_births_id <- monthly_births %>%
  left_join(name_to_nuts, by = "geo")

# Let's check what is with NA
na_nuts0 <- monthly_births_id %>%
  filter(is.na(NUTS_ID)) 

# A list of countries and regions that are not considered in our analysis
na_nuts_name <- as.data.frame(unique(na_nuts0$geo))

# We're gonna consider just the countries that are in the NUTS0 classification
births_in_nuts0 <- monthly_births_id %>%
  filter(!is.na(NUTS_ID))

# Inspection
head(births_in_nuts0)

# Revision of how many countries are inside our data
unique_nuts_name<-as.data.frame(unique(births_in_nuts0$geo))
# There are 39 countries! the same number of countries that are in our NUTS0 shp

# Let's create some maps !!
# We're going to use a lot of filters with dplyr. 
# The following steps are just to prepare our data to put it in the map

# First map: Average births in 2002 and 2022 in all the countries ----

map01 <- births_in_nuts0 %>%
  filter(TIME_PERIOD %in% c(2002,2022)) %>%   # selection of the years
  filter(month %in% month.name) %>%   # filter to consider just 'real' months
  group_by(NUTS_ID, TIME_PERIOD) %>%
  dplyr::summarise(MEAN_BIRTHS = mean(OBS_VALUE), 
                   .groups = 'drop')

# Merge with our shp file
map01_geo<- map01 %>%
  left_join(nuts0[,c("NUTS_ID","ID_NUM","geometry")], by = "NUTS_ID")

# Very important step: ensure that our data set stays with the spatial information
map01_geo <- sf::st_as_sf(map01_geo) 
head(map01_geo)

# First map
ggplot(map01_geo, 
       aes(fill = MEAN_BIRTHS)) + 
  geom_sf(color = "white", size = 0.1) +  
  coord_sf(datum = NA) +
  scale_fill_viridis_c(option = "plasma", 
                       name = "Mean Births", 
                       na.value = "grey90") +
  facet_wrap(~ TIME_PERIOD)+
  labs(
    title = "Average monthly births in 2002 and 2022",
    caption = "Source: Eurostat"
  ) +
  theme_minimal(base_size = 12)

# Let's upload the population
# The original data set can be downloaded from: 
# https://ec.europa.eu/eurostat/databrowser/view/demo_pjan__custom_16765577/default/table?lang=en

euro_yearly_pop<- read_csv("estat_demo_pjan_filtered_en_age_0024.csv")

# Select just the useful variables
head(euro_yearly_pop)
pop_yearly <- euro_yearly_pop[,c(8,10,12,13,14,16)]

# Select the population to get the Women Birth Rate
# The Women Birth Rate for this exercise is defined as the number of births
# per 100,000 women in a given country-month.
women_birth_rate <- pop_yearly %>%
  filter(sex == "F") %>%
  filter(age == "TOTAL") %>%
  filter(geo %in% nuts0$NUTS_ID) %>%
  group_by(TIME_PERIOD,geo) %>%
  dplyr::summarise(WOMEN_POP= sum(OBS_VALUE), 
                   .groups = 'drop') %>%
  rename(NUTS_ID = geo)

# Merge the population with the monthly births
WBR_nuts0 <- births_in_nuts0 %>%
  filter(TIME_PERIOD %in% c(2000:2023)) %>%    
  filter(month %in% month.name) %>%
  merge(women_birth_rate, by = c("NUTS_ID", "TIME_PERIOD")) %>%
  mutate(WBR = (OBS_VALUE/WOMEN_POP)*100000) # Women Birth Rate  (WBR) 

# Second map: Average birth rate in 2002 and 2022 in all the countries ----

# Filter the information that we need
map02 <- WBR_nuts0 %>%
  filter(TIME_PERIOD %in% c(2002,2022)) %>%
  filter(month %in% month.name) %>%
  group_by(NUTS_ID, TIME_PERIOD) %>%
  dplyr::summarise(MEAN_TBR = mean(WBR), 
                   .groups = 'drop')

# Merge with our shp file information
map02_geo<- map02 %>%
  left_join(nuts0[,c("NUTS_ID","ID_NUM","geometry")], by = "NUTS_ID")

# Very important step: ensure that our data set stays with the spatial information
map02_geo <- st_as_sf(map02_geo) 

# Second map
ggplot(map02_geo, 
       aes(fill = MEAN_TBR)) + 
  geom_sf(color = "white", size = 0.1) +  
  coord_sf(datum = NA) +
  scale_fill_viridis_c(option = "plasma", 
                       name = "Mean Birth Rate", 
                       na.value = "grey90") +
  facet_wrap(~ TIME_PERIOD)+
  labs(
    title = "Average birth rate in 2002 and 2022",
    caption = "Source: Eurostat"
  ) +
  theme_minimal(base_size = 12)

# Working with climate data ----
# Let's go to the funny part... work with the climate information
# We want to get monthly temperature, that will be useful to analyze together 
# with the birth data.

# First we save a palette of nice colors that we will use later
my.palette <- brewer.pal(n = 9, name = "OrRd")

# Open the raster data with the daily temperature information
tmp_2015 <- terra::rast("temp_jan_to_dic_2015_europe.nc")
# The original data set can be downloaded from: 
# https://cds.climate.copernicus.eu/datasets/derived-era5-land-daily-statistics?tab=overview

# Check the structure
print(tmp_2015)

# Check the Cordinate Reference System
st_crs(tmp_2015)

# The names of the layers (each layer contain information)
names(tmp_2015) # We have 365 days

# Create sequence of names  from 01/January/2015 to 31/December/2015
dates_2015<- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "day")

# Put the names in the raster layers
names(tmp_2015) <- dates_2015
names(tmp_2015)

# How it looks our data now
print(tmp_2015)

# The climate information is usually in degree Kelvins. Let's transform it to Celsius 
# ... be patient. Takes a lit bit of time

# We're gonna use a progressive approach to see how much time require the transformation 

# Let's start!
# Get number of layers
n_layers <- nlyr(tmp_2015)

# Create an empty list to store converted layers
converted_layers <- vector("list", n_layers)

# Start the timer
start_time <- Sys.time()

# Loop through each layer
for (i in 1:n_layers) {
  # Convert the layer from Kelvin to Celsius
  converted_layers[[i]] <- tmp_2015[[i]] - 273.15
  
  # Estimate remaining time
  elapsed <- Sys.time() - start_time
  avg_time <- elapsed / i
  remaining_time <- avg_time * (n_layers - i)
  
  message(sprintf("Converted layer %d of %d — Estimated time remaining: %.2f seconds",
                  i, n_layers, as.numeric(remaining_time, units = "secs")))
}

# Combine all converted layers back into a SpatRaster
tmp_2015_celsius <- rast(converted_layers)
print(tmp_2015_celsius)

# You can do the same directly without watching the remaining time
# tmp_2015 <- tmp_2015 - 273.15  #it takes around 5 minutes

# Create a factor for months (e.g., "2015-01", "2015-02", etc.)
month_ids <- format(dates_2015, "%Y-%m")

# Apply the mean by time group (month)
monthly_means <- terra::tapp(tmp_2015_celsius, index = month_ids, fun = mean)
# tapp() used to apply a function across grouped layers

# Check, named and plot result
names(monthly_means) <- unique(month_ids)
print(monthly_means)
plot(monthly_means)

# Until this point  we have monthly climate data for all the surface, 
# let's classify it by country 

# Raster layer extraction based on the values within the NUTS0 polygons
temp_exctr <- 
  exact_extract(
    monthly_means, 
    nuts0,
    progress = T,
  )

# We combine the list of data sets (each grid within each country) into a single data frame 
# and have a look at the data:
temp_exctr_comb <- bind_rows(temp_exctr, .id = "id")  %>% 
  as_tibble()
head(temp_exctr_comb)

# The data contains the raster values (temperature data) extracted from 
# the grid cells that fall within that region

# Based on the fraction of area covered  we can create
# a monthly average weighted value for each province.

#To do so, we first pivot the data to a long format.
temp_exctr_long <- pivot_longer(
  temp_exctr_comb, 
  -c(id, coverage_fraction), 
  names_to = "date",
  values_to = "t2m"
)  

# Now we can create the weighted average based on 
# location using the fraction as weights:
# first we eliminate NAs.
temp_exctr_long <- temp_exctr_long[complete.cases(temp_exctr_long), ]

# Creation of weighted averages
temp_exctr_weight <-  temp_exctr_long %>% 
  group_by(id, date) %>% 
  dplyr::summarise(t2m = sum(t2m * coverage_fraction) / sum(coverage_fraction),
                   .groups = 'drop')

# The next step is the merge with the shp with nuts0 division

# Select just the necessary variables
europe_var_select <- nuts0[, c("ID_NUM", "NUTS_ID", "NUTS_NAME")]

# Eliminate the geometry of the spatial object for creating a line graph
europe_no_geo <- st_set_geometry(europe_var_select, NULL)

# Merge temperature data with cluster ID
temp_coord <- merge(europe_no_geo,temp_exctr_weight, by.x = "ID_NUM", by.y = "id", all.x = TRUE, all.y = FALSE)
head(temp_coord)

# Let us plot the temperature in Italy over time
df_italy <- temp_coord %>% 
  filter(NUTS_ID == "IT")

# Generate the plot
ggplot(df_italy, aes(x=date, y=t2m, group=1)) +
  geom_line(color="blue") +
  labs(title = "Monthly mean temperature in Italy, 2015", x = "Date", y = "Temperature (Celsius)")

# Let's map the temperature just for one month
may_t2m <- temp_coord %>%
  filter(date == "2015-05") # for example May

# Merge with the polygon information
may_t2m <- merge(europe_var_select[,c("ID_NUM","geometry")],may_t2m,  by.x = "ID_NUM", by.y = "ID_NUM",  all.x = TRUE, all.y = FALSE)
names(may_t2m)

# Generate interact map that allows to visualize the values
mapview(may_t2m[,"t2m"], legend.title ="Temp")

# Let's map the temperature for all the year
allyear_t2m <- merge(europe_var_select[,c("ID_NUM","geometry")],temp_coord,  by.x = "ID_NUM", by.y = "ID_NUM",  all.x = TRUE, all.y = FALSE)

# Map of monthly mean temperatures in 2015
ggplot(allyear_t2m, aes(fill = t2m)) +
  geom_sf(color = "darkgrey", size = 0.05) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    name = "°C",
    low = "yellow",
    high = "red",
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 10,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  facet_wrap(~ date, nrow = 3) +
  labs(
    title = "Mean temperature by month, 2015",
    subtitle = "Aggregated by country (NUTS0 level)"
  ) 

# Exercises!! :D ---------
# Choose any exercise that you prefer

# Exercise 1:  Can we spot a climate effect on fertility?
# -  Let’s explore this possibility by comparing birth rates in May 2016 
#    with the average temperature in July 2015 (9 months prior).
# -  Merge the information that you already know of birth rates and temperature
#    using the NUTS_ID to get them into the same table.
# -  Visualize the relationship using a scatterplot and regression line.
# -  HINT 1: For visualizations in ggplot is better use long format
#    HINT 2: Use geom_point() and geom_smooth(method = "lm") in ggplot2
#    to plot the data points and the trend line.

# Filter the birth data
may_br <- births_in_nuts0 %>%
  filter(TIME_PERIOD == 2016) %>%
  filter(month == "May") %>%
  merge(women_birth_rate, by = c("NUTS_ID", "TIME_PERIOD")) %>%
  mutate(WBR = (OBS_VALUE/WOMEN_POP)*100000) 

head(may_br)

# Filter the temperature data
july_tmp <- temp_coord %>%
  filter(date == "2015-07")
head(july_tmp)

# Merge the information based on NUTS_ID
tmp_n_br <- merge(july_tmp, may_br[,c(1,7)],by ="NUTS_ID",all.x = TRUE)
head(tmp_n_br)

# Reshape data to long format
map_long <- tmp_n_br %>%
  dplyr::select(WBR,t2m,NUTS_ID) %>%
  pivot_longer(cols = c(WBR, t2m),
               names_to = "variable",
               values_to = "value")

# Plot the results
ggplot(tmp_n_br, aes(x = t2m, y = WBR)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Do Hot Summers Lead to Less Births in May?",
    x = "Mean Temperature in July 2015 (°C)",
    y = "Birth Rate in May 2016"
  ) +
  theme_minimal()

# Exercise 2:  How hot was Italy in 2022?
# - Calculate and explore the daily temperatures across Italian regions
# in 2022 using high-resolution climate raster data and official regional boundaries.
# - Answer the next question: 
# When and where was the hottest and coldest day in 2022?

spdf_r <- st_read("Reg01012022_g_WGS84.shp")

# Inspection of spatial object:
head(spdf_r)
st_geometry(spdf_r)
st_crs(spdf_r)

#Visualize Italian provinces on a map
mapviewOptions(fgb = FALSE)
mapview(spdf_r)

# Temperature of 2022

# Open the raster data with the daily temperature information
tmp_2022 <- terra::rast("temp_jan_to_dic_2022_europe.nc")

# Check names of layers
names(tmp_2022)

# Create a sequence of dates from January 2022 to December 2022 to feed to raster data
dates <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "day")

# Add the dates 
names(tmp_2022) <- dates
names(tmp_2022)

# Transform the projections of the shapefile to be the same of our raster:
italy_r <- st_transform(spdf_r, st_crs(tmp_2022))

# Crop data (limit dimensions based on max and min coordinates in the shapefile)
tmp_crop_r <- terra::crop(tmp_2022, italy_r)
print(tmp_crop_r)

# Transform to Celsius 
tmp_2022 <- tmp_2022 - 273.15  #it takes around 5 minutes

###Raster layer extraction based on the values within the polygons ###
temp_exctr <- 
  exact_extract(
    tmp_2022, 
    italy_r,
    progress = T,
  )

#We combine the list of datasets(each grid within each province) into a single dataframe 
#and have a look at the data:
temp_exctr_comb <- bind_rows(temp_exctr, .id = "id")  %>% 
  as_tibble()
head(temp_exctr_comb)

# To long format
temp_exctr_long <- pivot_longer(
  temp_exctr_comb, 
  -c(id, coverage_fraction), 
  names_to = "date",
  values_to = "t2m"
)

# Now we can create the weighted average based on 
# location using the fraction as weights:
# first we eliminate NAs.
temp_exctr_long <- temp_exctr_long[complete.cases(temp_exctr_long), ]

temp_exctr_weight <-  temp_exctr_long %>% 
    group_by(id, date) %>% 
    dplyr::summarise(t2m = sum(t2m * coverage_fraction) / sum(coverage_fraction),
                     .groups = 'drop')

# Merge the data
daily_tmp_reg_italy <- merge(temp_exctr_weight,italy_r[,c(1:3)], by.x = "id", 
                             by.y = "COD_REG", all.x = TRUE, all.y = FALSE)

hottest <- daily_tmp_reg_italy %>%
  filter(t2m == max(t2m, na.rm = TRUE)) %>%
  dplyr::select(date, t2m, DEN_REG)

print(hottest)

coldest <- daily_tmp_reg_italy %>%
  filter(t2m == min(t2m, na.rm = TRUE)) %>%
  dplyr::select(date, t2m, DEN_REG)

print(coldest)
