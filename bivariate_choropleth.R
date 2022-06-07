# Bivariate Map in R
# Author: Lance R. Owen 
# Date: 7 June 2022

library(biscale)
library(tidycensus)
library(geojsonR)
library(leaflet)
library(rgdal)
library(geojsonio)
library(sf)

# Import TopoJSON and set CRS
SVI <- topojson_read("https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/SVI2018_US_county.json") 
SVI_reproj <-  st_set_crs(SVI, "EPSG:4326")

# Look at extent of variables in sf dataframe
colnames(SVI_reproj)
plot(SVI_reproj$geometry)
length(unique(SVI_reproj$STATE))

# Subset data to Lower 48
lower48 <- SVI_reproj %>% 
  filter(STATE != "HAWAII" & STATE != "ALASKA")

us_data <- bi_class(lower48, 
                    x = E_DISABL, 
                    y = RPL_THEMES, 
                    style = "quantile", 
                    dim = 4)

map <- ggplot() +
  geom_sf(data = us_data, 
          mapping = aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink2", 
                dim = 4) +
  labs(
    title = "UNITED STATES | SVI and Disability",
    subtitle = "CDC 2018 Social Vulnerability Index"
  ) +
  bi_theme()
  
legend <- bi_legend(pal = "GrPink2",
                    dim = 4,
                    xlab = "Higher SVI",
                    ylab = "Higher Disability",
                    size = 10)

finalPlot <- cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.2, 0.2)

finalPlot