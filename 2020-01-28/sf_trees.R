library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(mapdeck)


# Set Themes --------------------------------------------------------------
theme_set(theme_minimal())
colors <-  colorRampPalette(c("#749776", "#205823"))(6)

# Load Data Sets ----------------------------------------------------------
tt_data <- tidytuesdayR::tt_load('2020-01-28') 
sf_trees <- tt_data$sf_trees



# A little bit of clean up ------------------------------------------------
# Calculate appro AGE in years
# Remove unusual height (dbh) values

sf_trees <- sf_trees %>%
  mutate(
    age = interval(date, today()) / years(1),
    dbh = ifelse(dbh > 200 | dbh == 0, NA, dbh)
  )


# Visualize distrubtions of AGE and HEIGHT --------------------------------

ggplot(sf_trees, aes(dbh)) +
  geom_histogram(binwidth = 5, fill = colors[1], alpha = .8) +
  theme(
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
    ) +
  labs(title = 'SF Trees Height Distribution')

ggplot(sf_trees, aes(age))+
  geom_histogram(binwidth = 5, fill = colors[1], alpha = .8) +
  theme(
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(title = 'SF Trees Age Distribution')

ggplot(sf_trees, aes(date,dbh)) +
  geom_hex() +
  scale_fill_gradient(low=colors[1], high = colors[6]) +
  theme(
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(title = 'SF Trees Height by Date')



# Create Prep and Map Functions -------------------------------------------

f_prepare_data <- function(df, measure) {
  
  ## Remove lat and long not close to SF
  df <- df %>%
    filter(
      !is.na(latitude), 
      !is.na(longitude),
      latitude >= 37.7,
      latitude < 38,
      longitude >= -123,
      longitude <= -122
    ) 
  
  ### Round Lat and Long to 3rd decimal and summarize 
  df <- df %>%
    select(latitude, longitude, measure = !!measure) %>%
    filter(!is.na(measure)) %>%
    mutate(
      latitude = round(latitude,3),
      longitude = round(longitude,3)
    ) %>%
    group_by(latitude, longitude) %>%
    summarize(mean = mean(measure, rm.na = T)) %>%
    mutate(mean = replace_na(mean, 0)) 
  
  ## Uncount mean. 
  ## Necessary transform in order to map appropriately
  df %>%
    uncount(mean)
  
}

plot_map <- function(df) {

  df %>%
    mapdeck(
      token = Sys.getenv('MAPBOX_TOKEN'),
      style = mapdeck_style("light"),
      pitch = 55,
      zoom = 12,
      bearing = 10,
      location = c(-122.445,37.75)
    ) %>%
      add_hexagon(
        lat = "latitude",
        lon = "longitude",
        layer_id = "hex_layer",
        elevation_scale = 2,
        radius = 11.132^2,
        colour_range = colors,
        highlight_colour = '#C7D5C8FF',
        auto_highlight = T,
        update_view = F
      ) 
  
}


# Generate Plots ----------------------------------------------------------

plot_mean_height <- sf_trees %>%
  f_prepare_data(quo(dbh)) %>%
  plot_map()

plot_mean_age <- sf_trees %>%
  f_prepare_data(quo(age)) %>%
  plot_map()

