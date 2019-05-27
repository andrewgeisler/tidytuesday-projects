library(tidyverse)
library(lubridate)
library(ggplot2)
library(gganimate)
library(countrycode)
library(png)
library(ggridges)


### READ DATA SETS ---
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

####
# For Year 2010 Extract
# - Total population
# - Tonnes of Mismanaged Waste
# - Per Capita Mismanged Waste
# - Per Capita GPD
# - Per Capita Waste

df_total_pop_2010 <- coast_vs_waste %>%
  filter(Year == 2010) %>%
  select(
    Entity,
    Code,
    Year,
    `Total population (Gapminder)`
  )

df_plaste_waste_tonnes_2010 <- coast_vs_waste %>%
  filter(Year == 2010) %>%
  select(
    Entity,
    Code,
    Year,
    `Mismanaged plastic waste (tonnes)`
  )

df_mismanged_waste_2010 <- mismanaged_vs_gdp %>%
  filter(Year == 2010) %>%
  select(
    Entity,
    Code,
    Year,
    `Per capita mismanaged plastic waste (kilograms per person per day)`
  )

df_gpd_2010 <- mismanaged_vs_gdp %>%
  filter(Year == 2010) %>%
  select(
    Entity,
    Code,
    Year,
    `GDP per capita, PPP (constant 2011 international $) (Rate)`
  )

df_waste_per_cap_2010 <- waste_vs_gdp %>%
  filter(Year == 2010) %>%
  select(
    Entity,
    Code,
    Year,
    `Per capita plastic waste (kilograms per person per day)`
  )

### CREATE DF FOR JOINED 2010 DATA
df_2010 <- reduce(
  list(df_gpd_2010, df_mismanged_waste_2010, df_plaste_waste_tonnes_2010, df_total_pop_2010, df_waste_per_cap_2010),
  left_join
)

### LOOK UP COUNTRY CODE
df_2010 <- df_2010 %>%
  mutate(
    Continent = countrycode(Code, origin = "iso3c", destination = "continent"),
    Continent = factor(Continent),
    Year = factor(Year)
  ) %>%
  filter(!is.na(Continent))

### FUNCTION TO REPLACE NA'S WITH MEIAN VALUES
f_replace_na_with_median <- function(x) {
  median <- median(x, na.rm = T)
  case_when(is.na(x) ~ median, TRUE ~ x)
}

### INPUT MISING VALUES STRATIFIED BY CONTINENT

df_2010 <- df_2010 %>%
  group_by(Continent) %>%
  mutate_if(is.numeric, f_replace_na_with_median)

## CODE TTO HAS AN EXTREMELY UNUSUAL PER CAPITA WASTE.
## REPLACING WITH CONTINENT MEDIA
df_2010 <- df_2010 %>%
  group_by(Continent) %>%
  mutate(`Per capita plastic waste (kilograms per person per day)` = case_when(
    Code == "TTO" ~ median(`Per capita plastic waste (kilograms per person per day)`),
    TRUE ~ `Per capita plastic waste (kilograms per person per day)`
  ))

## CALCULATE PERCENT OF WASTE THAT IS MISMANAGED
df_2010 <- df_2010 %>%
  mutate(
    `Per Capita Mismanged Waste Percent` =
      `Per capita mismanaged plastic waste (kilograms per person per day)` / `Per capita plastic waste (kilograms per person per day)`
  )

### CUT GDP INTO QUANTILES
df_2010 <- df_2010 %>%
  ungroup() %>%
  mutate(
    `GDP Quantile` = cut(`GDP per capita, PPP (constant 2011 international $) (Rate)`,
      breaks = quantile(`GDP per capita, PPP (constant 2011 international $) (Rate)`, probs = seq(0, 1, 0.2)),
      labels = c(
        "0-20",
        "20-40",
        "40-60",
        "60-80",
        "80-100"
      ),
      right = FALSE,
      include.lowest = TRUE
    ),
    `GDP Quantile` = `GDP Quantile`
  )


p_percent_waste <- df_2010 %>%
  ggplot() +
  theme_minimal() +
  geom_density_ridges(
    aes(
      y = Continent, x = `Per Capita Mismanged Waste Percent`,
      fill = Continent, color = Continent
    ),
    alpha = 0.25, show.legend = F, jittered_points = TRUE
  ) +
  scale_y_discrete(limits = rev(levels(df_2010$Continent))) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme(axis.title = element_text(size = 8)) +
  labs(x = 'Percent of Total Plastic Waste', y = '')

p_animated <- p_percent_waste +
  transition_states(`GDP Quantile`,
    transition_length = 2,
    state_length = 0,
    wrap = F
  ) +
  ggtitle("Mismanaged Plastic Waste - 2010\nPer Capita GDP Percentile: {closest_state}")

## SAVE ANIMATION ---- 
animate(p_animated, height = 450, width =800)
anim_save(filename = '2019-05-21/percent_plastic_waste_animated.gif')




