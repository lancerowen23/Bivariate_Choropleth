# Bivariate Map in R
# Author: Lance R. Owen 
# Date: 7 June 2022


pacman::p_load(biscale, tidycensus, geojsonR, leaflet, rgdal, geojsonio, sf, ggplot2, cowplot)


# Import TopoJSON and set CRS
SVI_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/SVI2018_US_county.json") 
SVI_2018 <-  st_set_crs(SVI_2018, "EPSG:4326")
SVI_2018_Albers <-SVI_2018 %>% st_transform(5070)

 

# Look at extent of variables in sf dataframe
colnames(SVI_2018)
length(unique(SVI_2018$STATE))

# Subset data to Lower 48
lower48 <- SVI_2018_Albers %>% 
  filter(STATE != "HAWAII" & STATE != "ALASKA")

us_data <- bi_class(lower48, 
                    x = E_DISABL, 
                    y = RPL_THEMES, 
                    style = "quantile", 
                    dim = 3)

map <- ggplot() +
  geom_sf(data = us_data, 
          mapping = aes(fill = bi_class), 
          color = "grey70", 
          size = 0.1,
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", 
                dim = 3) +
  labs(
    title = "UNITED STATES | SVI and Disability",
    subtitle = "CDC 2018 Social Vulnerability Index"
  ) +
  bi_theme()
  
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Higher SVI",
                    ylab = "Higher Disability",
                    size = 10)

finalPlot <- cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.2, 0.2)

finalPlot

#get correlations 
library(corrplot)

#subset selected variables
list <- as.list(colnames(SVI_2018))
SVI_2018_vars <- SVI_2018 %>% 
  select(E_DISABL, E_SNGPNT, E_MINRTY, E_LIMENG, E_MUNIT) %>% 
  st_drop_geometry()
  
str(SVI_2018_vars)
pairs(SVI_2018_vars)

m <- cor(SVI_2018_vars)
corrplot(m, method = 'number', order = 'AOE')
