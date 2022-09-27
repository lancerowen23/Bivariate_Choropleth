# Multilevel Regression between overall COVID-19 case fatality rates and SVI Rankings
# Author: Lance R. Owen
# Date: June 16, 2022

pacman::p_load(geojsonio, sf, tidyverse, broom, formattable, lme4)

cases <- read.csv('https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/covid_confirmed_usafacts.csv') %>% 
  select(countyFIPS, County.Name, State, X2022.06.14) %>% 
  filter(!County.Name == "Statewide Unallocated")
deaths <- read.csv('https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/covid_deaths_usafacts.csv') %>% 
  select(countyFIPS, X2022.06.14)%>% 
  filter(!countyFIPS == 0)

cfr <- left_join(cases, deaths, by='countyFIPS') %>% 
  filter(!X2022.06.14.x == 0) %>% # drop counties with 0 cases to avoid dividing by zero
  mutate(cfr = 100*X2022.06.14.y/X2022.06.14.x)

#add census regions and divisions
reg_div <- read.csv('\\\\cdc.gov\\project\\ATS_GIS_Store4\\Projects\\prj06286_SVI_Intentional_Fatal_Injury_Associations\\Data\\census_region_divisions.csv',
                    stringsAsFactors = TRUE, fileEncoding="UTF-8-BOM")

cfr_reg <- merge(cfr, reg_div, by.x = "State", by.y = 'st_abbr')

# Import TopoJSON and set CRS
SVI_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/SVI2018_US_county.json") 
SVI_2018 <-  st_set_crs(SVI_2018, "EPSG:4326")
SVI_2018_Albers <-SVI_2018 %>% st_transform(5070)

# Subset themes of interest
svi_themes <- SVI_2018 %>% 
  select(FIPS, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>% 
  filter(!RPL_THEMES == -999)

# Fix leading zero on FIPS 
cfr$countyFIPS <- str_pad(as.character(cfr$countyFIPS), 5, pad = "0")

# Join data
svi_themes_cfr <- merge(svi_themes, cfr_reg, by.x = "FIPS", by.y = "countyFIPS")

# Using standard lm function combined with group_by
# Linear Regression by State
lm_cfr_state <- svi_themes_cfr %>% 
  group_by(State) %>% 
  do(tidy(lm(cfr ~ RPL_THEMES, data = .))) %>% 
  filter(term == "RPL_THEMES") %>% 
  arrange(desc(estimate)) %>% 
  formattable(list(
    State = formatter("span", style = ~ style(color = "grey35",font.weight = "bold")),
    area(col = 3) ~ color_tile("#DeF7E9", "#71CA97"),
    area(col = 4) ~ color_tile("#F7E0C9", "#F47C0A"),
    area(col = 5) ~ color_tile("#F6F6B8","#F6EA4C"),
    area(col = 6) ~ color_tile("#F7BAC5", "#F35D7B")))

formattable(mex.cases.deaths, list(
  Municipality = formatter("span", style = ~ style(color = "grey35",font.weight = "bold")), 
  
  area(col = 8) ~ color_tile("#F7BAC5", "#F35D7B"),
  area(col = 9) ~ color_tile("#F7BAC5", "#F35D7B"),
  area(col = 10) ~ color_tile("#C9B9D9", "#AA68B9")))

# Linear Regression by Division
lm_cfr_div <- svi_themes_cfr %>% 
  group_by(div) %>% 
  do(tidy(lm(cfr ~ RPL_THEMES, data = .))) %>% 
  filter(term == "RPL_THEMES") %>% 
  arrange(desc(estimate))

# Linear Regression by Region
lm_cfr_reg <- svi_themes_cfr %>% 
  group_by(reg) %>% 
  do(tidy(lm(cfr ~ RPL_THEMES, data = .))) %>% 
  filter(term == "RPL_THEMES") %>% 
  arrange(desc(estimate))

#Linear Regression with no Levels
lm_cfr_overall <- lm(cfr ~ RPL_THEMES, data = svi_themes_cfr)
summary(lm_cfr_overall)

#Georgia
svi_themes_cfr_ga <- svi_themes_cfr %>% 
  filter(name == "Georgia")
lm_cfr_overall_ga <- lm(cfr ~ RPL_THEMES, data = svi_themes_cfr_ga)
summary(lm_cfr_overall_ga)

#Using lme4 library
summary(lmer(cfr ~ RPL_THEMES + (1 | State), data=svi_themes_cfr))
