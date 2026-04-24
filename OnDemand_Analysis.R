###################################################
#' 
#' UTA OnDemand Analysis
#' 
#' Updated: Mar 24, 2026
#' Author: Andy Hong, Doahn Lee
#' 
###################################################

# -------------------------------------------------
# Load packages
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")
install.packages("mapgl")
install.packages("tigris")
install.packages("usethis")

require(tidyverse)
require(sf)
require(mapview)

require(mapgl)
require(tigris)

# You need a basemap tile, so go to https://www.maptiler.com/
# Create your account and find the API key
# Replace the "paste_your_MapTiler_api_key_here" string with your key below.
Sys.setenv(MAPTILER_API_KEY = "YPApkQp6onPmRKhtmhbl")
Sys.getenv("MAPTILER_API_KEY")


# -------------------------------------------------
# Load data
# -------------------------------------------------

file1 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2021 All Available Data.csv'
file2 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2022 part-1.csv'
file3 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2022 part-2.csv'
file4 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2023 part-1.csv'
file5 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2023 part-2.csv'
file6 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2023 -part-3.csv'
file7 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2024 Jan_June part-1.csv'
file8 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2024 Jan_June part-2.csv'
file9 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2024 Jul_Dec part-1.csv'
file10 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2024 Jul_Dec part-2.csv'
file11 = 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/2025 Jan_Jun part-1.csv'

data1 = read.csv(file1)
data2 = read.csv(file2)
data3 = read.csv(file3)
data4 = read.csv(file4)
data5 = read.csv(file5)
data6 = read.csv(file6)
data7 = read.csv(file7)
data8 = read.csv(file8)
data9 = read.csv(file9)
data10 = read.csv(file10)
data11 = read.csv(file11)


# -------------------------------------------------
# Wrangle data
# -------------------------------------------------

# Combine all the data
data = dplyr::bind_rows(
  data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11
)

write.csv(data, 'C:/Users/doahn/Desktop/Professional Project/R Analysis/Doahn Lee GRAMA Request Records/02 UTA On Demand Pickup & Dropoff Coordinate GIS Shapefile and CSV Data for all service area for 2020 to 2025/data_21to25.csv', row.names=F)

# Convert text data to spatial data
df = st_as_sf(data, coords=c("Destination.Lng", "Destination.Lat"), crs=4326)

data %>% select(`Destination.Lat`, `Destination.Lng`) %>% view

# Sample 10k points and draw map
# mapview(sample_n(df, 10000)) >>> base R syntax
df %>% sample_n(10000) %>% mapview # >> Tidy syntax


# Extract date time variables and format them

df2 = df %>% 
  mutate(
    datetime = mdy_hm(`Request.Creation.Time`),
    year = year(datetime), 
    month = month(datetime),
    date = date(datetime),
    hour = hour(datetime),
    year_month = floor_date(datetime, "month") # create a new column for the start of each month
  )

df2 %>% view # View data in a table


# -------------------------------------------------
# Analyze data
# -------------------------------------------------

# Summarize ridership by year
df3 = df2 %>% 
  st_drop_geometry() %>% # drop all the geometry data
  filter(year %in% c(2022, 2023, 2024)) %>%
  group_by(year) %>%
  summarize(total = n(), .groups = 'drop') # calculate monthly sum (or mean, etc.)

# Plot data by year
ggplot(df3, aes(x = year, y = total)) +
  geom_col()  # Use geom_col for pre-aggregated data


# Summarize ridership by month
df3 = df2 %>% 
  st_drop_geometry() %>% # drop all the geometry data
  filter(year %in% c(2022, 2023, 2024)) %>%
  group_by(year_month) %>%
  summarize(total = n(), .groups = 'drop') # calculate monthly sum (or mean, etc.)

# Plot data by month
ggplot(df3, aes(x = year_month, y = total)) +
  geom_col() + # Use geom_col for pre-aggregated data
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize ridership by hour (mean hourly trips) for year 2023
df3 <- df2 %>%
  st_drop_geometry() %>%
  filter(year == 2023) %>%
  # Total trips for every specific hour of every specific month
  group_by(month, date, hour) %>%
  summarize(trips_on_this_day = n(), .groups = "drop") %>%
  # Average those daily totals by hour
  group_by(hour) %>%
  summarize(avg_trips = mean(trips_on_this_day), .groups = "drop")

# Plot data by hour
ggplot(df3, aes(x = hour, y = avg_trips)) + geom_col()


# -------------------------------------------------
# Visualize ridership data spatially
# -------------------------------------------------

# Summarize data to show spatial patterns throughout the day
df3 = df2 %>% 
  filter(year %in% c(2023)) %>% 
  filter(month %in% c(10)) %>% 
  group_by(hour) %>% 
  summarize(total = n())

# Map ridership by hour from 4 to 12
df3 %>% filter(hour == 4) %>% mapview()
df3 %>% filter(hour == 5) %>% mapview()
df3 %>% filter(hour == 6) %>% mapview()
df3 %>% filter(hour == 7) %>% mapview()
df3 %>% filter(hour == 8) %>% mapview()
df3 %>% filter(hour == 9) %>% mapview()
df3 %>% filter(hour == 10) %>% mapview()
df3 %>% filter(hour == 11) %>% mapview()
df3 %>% filter(hour == 12) %>% mapview()


# -------------------------------------------------
# Create a heatmap of ridership 
# -------------------------------------------------

# Read through the heatmap tutorial by Kyle Walker, and understand how the codes work.
# https://walker-data.com/posts/mapgl-dots/

# Get Salt Lake County boundary
slco = counties(cb = TRUE, year = 2023) %>% 
  filter(NAME == "Salt Lake")

# Create a filtered data that only contains ridership data for Oct 2023
df3 = df2 %>% 
  filter(year %in% c(2023)) %>%
  filter(month %in% c(10))

# Create a basemap layer
# You need to get the API Key to load the base map, 
# so go back to line 29 to create your maptile account and replace it with your API Key

basemap = 
  maplibre(
    style = maptiler_style("bright"),
    bounds = slco
  ) 

# Check the basemap
basemap

# Visualize 1000 points randomly
basemap %>%
  add_circle_layer(
    id = "circles",
    source = df3 %>% sample_n(1000),
    circle_color = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1
  ) 

# Create a heatmap only for 8AM in October 2023
basemap %>%
  add_heatmap_layer(
    id = "heatmap",
    source = df3 %>% filter(hour == 8),
    heatmap_radius = 10, 
    heatmap_opacity = 0.7,
    heatmap_color = interpolate(
      property = "heatmap-density",
      values = seq(0, 1, 0.2),
      stops = c("transparent", viridisLite::viridis(5))
    )
  )%>%
  add_legend(
    legend_title = "Density, 8AM",
    values = seq(0, 1, 0.2),
    colors = viridisLite::viridis(5)
  )

# Create a heatmap only for 1PM in October 2023
basemap %>%
  add_heatmap_layer(
    id = "heatmap",
    source = df3 %>% filter(hour == 13),
    heatmap_radius = 10, 
    heatmap_opacity = 0.7,
    heatmap_color = interpolate(
      property = "heatmap-density",
      values = seq(0, 1, 0.2),
      stops = c("transparent", viridisLite::viridis(5))
    )
  )%>%
  add_legend(
    legend_title = "Density, 1PM",
    values = seq(0, 1, 0.2),
    colors = viridisLite::viridis(5)
  )

# Create a heatmap only for 5PM in October 2023
library(viridisLite)

cols <- viridis(5)

basemap %>%
  add_heatmap_layer(
    id = "heatmap",
    source = df3 %>% filter(hour == 17),
    heatmap_radius = 10, 
    heatmap_opacity = 0.7,
    heatmap_color = interpolate(
      property = "heatmap-density",
      values = seq(0, 1, 0.2),
      stops = c("transparent", viridisLite::viridis(5))
    )
  )%>%
  add_legend(
    legend_title = "Density, 5PM",
    values = seq(0, 1, 0.2),
    colors = viridisLite::viridis(5)
  )

# Isolating Destination Address Data
names(data)

dest_freq = data %>%
  group_by(Destination.Address) %>%        # replace with exact column name if different
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))                      # sort from most to least frequent

dest_freq

repeated_dest = dest_freq %>%
  filter(count > 1)

repeated_dest

# Top 20 most frequent destinations
dest_freq %>% head(20) %>% view

# Bar Chart of Top 20
dest_freq %>%
  head(20) %>%
  ggplot(aes(x = reorder(Destination.Address, count), y = count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(count)),
            hjust = 1.1,         # push label inside the bar
            color = "white",     # white text for contrast
            size = 3) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))        # add extra space for labels on the right
  ) +
  labs(
    title = "Top 20 Most Frequent Destinations",
    x = "Destination Address",
    y = "Trip Count"
  ) +
  theme_minimal()

# Get top 20 most frequent destinations with their coordinates
top_dest = data %>%
  mutate(Destination.Address = str_squish(str_to_upper(Destination.Address))) %>%
  group_by(Destination.Address, Destination.Lat, Destination.Lng) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  head(20)

top_dest

top_dest_sf = top_dest %>%
  filter(!is.na(Destination.Lat) & !is.na(Destination.Lng)) %>%  # remove missing coords
  st_as_sf(coords = c("Destination.Lng", "Destination.Lat"), crs = 4326)

# Load all Utah counties
utah_counties = counties(state = "UT", cb = TRUE, year = 2023)

# Set a light basemap globally
mapviewOptions(basemaps = c("OpenStreetMap", "CartoDB.Positron", "Esri.WorldStreetMap"))

mapview(utah_counties,
        color = "black",
        col.regions = "transparent",
        alpha.regions = 0,
        lwd = 1.5,                    # border line width
        legend = FALSE,               # no legend for county layer
        layer.name = "Utah Counties") +
  mapview(top_dest_sf,
          cex = "count",
          zcol = "count",
          layer.name = "Trip Count")
