# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(ggmap)
library(dlookr)
library(scales)

# Read the data
df <- read.csv('mpox.csv')

# Convert date column to Date type and adjust the date range
df$date <- as.Date(df$date, format="%m/%d/%Y")

dlookr::diagnose(df)
# Extract month name from the date
df$month <- month.name[month(df$date)]

# 1. Top 10 countries with the highest total number of cases and deaths

# Top 10 countries by total cases
top_10_cases <- df %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  slice(1:10)

# Top 10 countries by total deaths
top_10_deaths <- df %>%
  group_by(location) %>%
  summarise(total_deaths = max(total_deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice(1:10)

# Plotting Top 10 Countries by Cases
ggplot(top_10_cases, aes(x = reorder(location, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries with the Highest Total Cases",
       x = "Country", y = "Total Cases") +
  theme_minimal(base_size = 15)

# Plotting Top 10 Countries by Deaths
ggplot(top_10_deaths, aes(x = reorder(location, -total_deaths), y = total_deaths)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Countries with the Highest Total Deaths",
       x = "Country", y = "Total Deaths") +
  theme_minimal(base_size = 15)

# 2. Visualizing the change in total cases worldwide over time

# Convert date column to Date type and filter for 'World'
df$date <- as.Date(df$date)
global_cases <- df %>%
  filter(location == "World") %>%
  select(date, total_cases)

# Plot total cases over time
ggplot(global_cases, aes(x = date, y = total_cases)) +
  geom_line(color = "darkblue") +
  labs(title = "Change in Total Cases Worldwide Over Time",
       x = "Date", y = "Total Cases") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Total case numbers by continent

# Filtering data by continents
continents <- c('Africa', 'Asia', 'Europe', 'North America', 'South America', 'Oceania')
continent_cases <- df %>%
  filter(location %in% continents) %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases, na.rm = TRUE))

# Plot total cases by continent
ggplot(continent_cases, aes(x = reorder(location, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total Cases by Continent",
       x = "Continent", y = "Total Cases") +
  theme_minimal(base_size = 15)

# 4. Top 10 countries by case rate per million people

# Top 10 countries by case rate per million
cases_per_million <- df %>%
  group_by(location) %>%
  summarise(total_cases_per_million = max(total_cases_per_million, na.rm = TRUE)) %>%
  arrange(desc(total_cases_per_million)) %>%
  slice(1:10)

# Plot cases per million by country
ggplot(cases_per_million, aes(x = reorder(location, -total_cases_per_million), y = total_cases_per_million)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Countries by Case Rate per Million People",
       x = "Country", y = "Cases per Million") +
  theme_minimal(base_size = 15)

# 5. Total cases and deaths in Africa over time

# Filter for Africa data
africa_data <- df %>%
  filter(location == "Africa") %>%
  select(date, total_cases, total_deaths) %>%
  group_by(date) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE),
            total_deaths = sum(total_deaths, na.rm = TRUE))

# Plot total cases and deaths over time for Africa
ggplot(africa_data, aes(x = date)) +
  geom_line(aes(y = total_cases, color = "Total Cases"), size = 1) +
  geom_line(aes(y = total_deaths, color = "Total Deaths"), size = 1) +
  scale_color_manual(values = c("Total Cases" = "blue", "Total Deaths" = "red")) +
  labs(title = "Total Cases and Deaths in Africa Over Time",
       x = "Date", y = "Count") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# 6. New cases and deaths per million population over time

# Aggregating new cases and deaths per million
new_cases_deaths <- df %>%
  group_by(date) %>%
  summarise(new_cases_per_million = sum(new_cases_per_million, na.rm = TRUE),
            new_deaths_per_million = sum(new_deaths_per_million, na.rm = TRUE))

# Plot new cases and deaths per million over time
ggplot(new_cases_deaths, aes(x = date)) +
  geom_line(aes(y = new_cases_per_million, color = "New Cases per Million"), size = 1) +
  geom_line(aes(y = new_deaths_per_million, color = "New Deaths per Million"), size = 1) +
  scale_color_manual(values = c("New Cases per Million" = "orange", "New Deaths per Million" = "purple")) +
  labs(title = "New Cases and Deaths per Million Population Over Time",
       x = "Date", y = "Count per Million") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Smoothed averages of new cases and deaths per million population over time

# Aggregating smoothed averages of new cases and deaths per million
smoothed_cases_deaths <- df %>%
  group_by(date) %>%
  summarise(new_cases_smoothed_per_million = sum(new_cases_smoothed_per_million, na.rm = TRUE),
            new_deaths_smoothed_per_million = sum(new_deaths_smoothed_per_million, na.rm = TRUE))

# Plot smoothed averages of new cases and deaths per million over time
ggplot(smoothed_cases_deaths, aes(x = date)) +
  geom_line(aes(y = new_cases_smoothed_per_million, color = "Smoothed New Cases per Million"), size = 1) +
  geom_line(aes(y = new_deaths_smoothed_per_million, color = "Smoothed New Deaths per Million"), size = 1) +
  scale_color_manual(values = c("Smoothed New Cases per Million" = "green", "Smoothed New Deaths per Million" = "brown")) +
  labs(title = "Smoothed New Cases and Deaths per Million Population Over Time",
       x = "Date", y = "Smoothed Count per Million") +
  scale_x_date(labels = date_format("%b %Y")) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))




# World map with Monkeypox cases
world <- st_read("TM_WORLD_BORDERS-0.3.shp")

# Join world map with data
world_data <- world %>%
  left_join(df %>%
              select(iso_code, total_cases_per_million) %>%
              group_by(iso_code) %>%
              summarise(total_cases_per_million = max(total_cases_per_million, na.rm = TRUE)), 
            by = c("iso3" = "iso_code"))

ggplot(data = world_data) +
  geom_sf(aes(fill = total_cases_per_million), color = "gray80", size = 0.3) +
  scale_fill_viridis_c(option = "C", name = "Total Cases per Million", 
                       breaks = scales::pretty_breaks(n = 5), 
                       labels = scales::label_comma()) +
  labs(title = "Distribution of Monkeypox Cases by Country",
       subtitle = "Visualizing total monkeypox cases per million population across different countries.",
       caption = "Data source: OWID Monkeypox Dataset\nDate range: 5 Jan 2022 to 25 Aug 2024\nAuthor: Islam Asal") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  coord_sf(expand = FALSE)

# Scatter plot: New Cases vs. Total Cases
ggplot(df, aes(x = total_cases, y = new_cases)) +
  geom_point(alpha = 0.5) +
  labs(title = "New Monkeypox Cases vs. Total Cases", x = "Total Cases", y = "New Cases") +
  theme_minimal()

# Scatter plot: New Cases Smoothed vs. New Deaths Smoothed
ggplot(df, aes(x = new_cases_smoothed_per_million, y = new_deaths_smoothed_per_million, color = new_cases_smoothed_per_million)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(title = "New Monkeypox Cases vs New Deaths per Million Population", 
       x = "New Cases per Million Population", 
       y = "New Deaths per Million Population", 
       color = "New Cases per Million Population") +
  theme_minimal()


