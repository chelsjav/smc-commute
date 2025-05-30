library(librarian)
# calling shelf function without loading the packages
librarian::shelf(tidyverse, tidycensus, sf, ggplot2, viridis, tmap)

# loading acs (census) data
acs5 <- load_variables(2022,'acs5')
view(acs5)

# defining variables i will be using
vars <- c(
  'B19013_001' = 'medinc', # median household income
  'B25064_001' = 'medrent', # median gross rent
  'B25070_007' = 'rentburden30', # renters spending 30-34.9% of income on rent
  'B25070_008' = 'rentburden35', # renters spending 35-39.9% of income on rent
  'B25070_009' = 'rentburden40', # renters spending 40-49.9% of income on rent
  'B25070_010' = 'rentburden50', # renters speaning 50$ or more of income on rent
  'B25077_001' = 'medhomeval', # median home value
  'B08303_001' = 'avgcommute', # average commute time ot work
  'B08006_008' = 'pubtransit' # public transportation users
)

years <- 2012:2022

# creating dataset
sanmateo <- bind_rows(lapply(years, function(y) {
  df <- get_acs(
    geography = 'county',
    variables = names(vars),
    state = 'CA',
    county = 'San Mateo',
    year = y,
    survey = 'acs5',
    geometry = TRUE # getting spatial data for mapping
  ) %>%
    mutate(year = y)  # adding a year column
  return(df)
}))
head(sanmateo)

# converting 'sf' object to regular dataframe to prevent errors with 'agr'
sanmateo_df <- st_drop_geometry(sanmateo
                                )
# cleaning the data and pivoting it into a wide format for analysis
sanmateo_wide <- sanmateo_df %>%
  select(GEOID, NAME, variable, estimate, year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(
    medinc = 'B19013_001',
    medhomeval = 'B25077_001',
    medrent = 'B25064_001',
    rentburden30 = 'B25070_007',
    rentburden35 = 'B25070_008',
    rentburden40 = 'B25070_009',
    rentburden50 = 'B25070_010',
    avgcommute = 'B08303_001',
    pubtransit = 'B08006_008'
  ) %>%
  mutate(
    totalrentburden = rentburden30 + rentburden35 + rentburden40 + rentburden50,
    rentburdenperc = (totalrentburden / (totalrentburden + medrent)) * 100
  )
head(sanmateo_wide)

# filtering for median home value data and plotting a line plot
homeval <- sanmateo %>% filter(variable == "B25077_001")
ggplot(homeval, aes(x = year, y = estimate)) +
  geom_line(color = "lightgreen", linewidth = 1.2) +
  geom_point(color = "red", linewidth = 2) +
  labs(
    title = "Median Home Value in San Mateo County (2012-2022)",
    x = "Year",
    y = "Median Home Value ($)"
  ) +
  theme_minimal()

# filtering for commute time data 
commute <- sanmateo %>% filter(variable == "B08303_001")
ggplot(commute, aes(x = year, y = estimate)) +
  geom_line(color = "lightblue", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Average Commute Time in San Mateo County (2012-2022)",
    x = "Year",
    y = "Commute Time (Minutes)"
  ) +
  theme_minimal()

# filtering for public transit usage 
transit <- sanmateo %>% filter(variable == "B08006_008")
ggplot(transit, aes(x = year, y = estimate)) +
  geom_line(color = "pink", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Public Transit Use in San Mateo County (2012-2022)",
    x = "Year",
    y = "Public Transit Commuters"
  ) +
  theme_minimal()

# research question #1
# plotting a scatter plot to compare housing costs and commute time
ggplot(sanmateo_wide, aes(x = medhomeval, y = avgcommute)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Housing Costs vs. Commute Time in San Mateo County",
    x = "Median Home Value ($)",
    y = "Average Commute Time (Minutes)"
  ) +
  theme_minimal()

# plotting a scatter plot to compare public transit use and home values
ggplot(sanmateo_wide, aes(x = medhomeval, y = pubtransit)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Public Transit Use vs. Home Values",
    x = "Median Home Value ($)",
    y = "Public Transit Commuters"
  ) +
  theme_minimal()

# research question #2
# plotting income and commute time in a scatterplot
ggplot(sanmateo_wide, aes(x = medinc, y = avgcommute)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Income vs. Commute Time in San Mateo County",
    x = "Median Household Income ($)",
    y = "Average Commute Time (Minutes)"
  ) +
  theme_minimal()

# research question #3
# plotting commute time and rent burden in a scatterplot
ggplot(sanmateo_wide, aes(x = avgcommute, y = rentburdenperc)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Commute Time vs. Rent Burden in San Mateo County",
    x = "Average Commute Time (Minutes)",
    y = "Percentage of Cost-Burdened Renters (%)"
  ) +
  theme_minimal()

# correlation analysis
cor_test <- cor.test(sanmateo_wide$avgcommute, sanmateo_wide$rentburdenperc, method = "pearson")
print(cor_test)

# ensuring unique GEOID-geometry mapping by selecting only the most recent year
sanmateo_geometry <- sanmateo %>%
  filter(year == max(year)) %>% # Keep only the latest year's geometry
  select(GEOID, geometry) %>%
  distinct(GEOID, .keep_all = TRUE) # Ensure unique GEOID values

# joining geometry back to the wide dataset
sanmateo_wide_sf <- left_join(sanmateo_wide, sanmateo_geometry, by = "GEOID") %>%
  st_as_sf() # Convert back to `sf` object
head(sanmateo_wide_sf)













