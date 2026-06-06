# ==============================================================================
# PROJECT: US Kidney Disease Mortality Rate Map
# PURPOSE: Presentation-Ready Albers Equal Area Conic Choropleth Map
# ==============================================================================

# === BATCH 1: LOAD LIBRARIES & READ SOURCE FILES ===
library(tidyverse)
library(sf)
library(readxl)
library(viridis)

# Load data using your exact local computer file paths
g_map <- st_read("C:/Users/ASUS/Downloads/usa/usa/cb_2018_us_state_500k.shp")
mydata <- read_excel("C:/Users/ASUS/Downloads/usa/usa/mortality.xlsx")

# Transform the map layout to Albers Equal Area Projection (EPSG 5070)
# This gives the US its classic, professional, curved shape
g_map_projected <- st_transform(g_map, crs = 5070)


# === BATCH 2: DATA JOIN & TYPE CLEANING ===
# Join the datasets and uniquely name the object to bypass memory/function conflicts
final_presentation_data <- g_map_projected %>%
  left_join(mydata, by = c("STUSPS" = "statescode")) %>%
  mutate(DeathRate = as.numeric(DeathRate))


# === BATCH 3: GENERATE THE VISUALIZATION ===
ggplot(data = final_presentation_data) +
  # Draw the states with crisp white borders
  geom_sf(data = final_presentation_data, aes(fill = DeathRate), color = "white", linewidth = 0.3) +
  
  # Configure the high-contrast Plasma color palette and legend design
  scale_fill_viridis_c(
    option = "plasma", 
    direction = -1,
    name = "Deaths per 100,000 population",
    na.value = "#E0E0E0",
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = unit(8, "cm"), 
      barheight = unit(0.3, "cm")
    )
  ) +
  
  # Add professional titles, subtitles, and data sourcing annotations
  labs(
    title = 'Mortality Rate from Kidney Disease in the USA, by State',
    subtitle = 'Age-adjusted death rate',
    caption = 'Source: mortality.xlsx'
  ) +
  
  # Set strict coordinate bounding boxes optimized for the curved Albers projection
  coord_sf(
    xlim = c(-2500000, 2500000), 
    ylim = c(200000, 3200000), 
    expand = FALSE
  ) +
  
  # Apply a minimalist theme and fine-tune typography for slideshow presentations
  theme_void() + 
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey40", size = 11, hjust = 0, margin = margin(b = 15)),
    plot.caption = element_text(color = "grey60", size = 8, hjust = 1, margin = margin(t = 10)),
    legend.position = "bottom", 
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20) 
  )
