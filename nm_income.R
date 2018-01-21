#------------------
# Load Libraries
#------------------

library(tidyverse)
library(maps)
library(rvest)
windowsFonts(Arial=windowsFont("Arial"))
#---------------------------
# Import data into dataframe
#---------------------------

nm.income <- read_html("https://en.wikipedia.org/wiki/List_of_New_Mexico_locations_by_per_capita_income") %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table() %>%
  tbl_df()


nm.income

#-------------------------
# Select only data we want
#-------------------------

nm.household_income <- nm.income %>% select(County, `Median
                                            household
                                            income`)

nm.household_income

colnames(nm.household_income) <- c('county', 'household_income')

nm.household_income

#------------------------
# clean data
#-----------------------

nm.household_income$household_income <- nm.household_income$household_income %>%
  str_replace_all('[$,]', '') %>% as.numeric()
nm.household_income$county <- tolower(nm.household_income$county)

nm.household_income$county <- nm.household_income$county %>%
  str_replace_all('[?]', 'n')
nm.household_income

map.data <- map_data('county') %>%
  filter(region =="new mexico")
head(map.data)

#-----------------------
# compare and join data frames
#--------------------------

anti_join(map.data, nm.household_income, by=c('subregion' = 'county'))

nm.map <- inner_join(map.data, nm.household_income, by=c('subregion' = 'county'))

head(nm.map)

min(nm.map$household_income)
max(nm.map$household_income)
IQR(nm.map$household_income)

#------------------------
# map the data
#-------------------------

ggplot(nm.map) +
  geom_polygon(aes(x=long, y=lat, group = group, fill=household_income), color='white') +
  coord_fixed(ratio=1.3) +
  scale_fill_gradientn(colours = c('grey', 'orange', 'red')
                       ,values = scales::rescale(c(25000,65000,105000))
                       ,breaks = c(25000, 65000, 105000)
                       ,labels=c('$25,000','$65,000','$105,000')
                       ,limits=c(25000,105000))  +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Income'
       ,title = 'Household Income'
       ,subtitle = 'New Mexico household income by county'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family='Times', color='black')
        ,panel.background = element_rect(fill='white')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,plot.background = element_rect(fill='white')
        ,legend.position = 'right'
        ,legend.background = element_blank()
        ,legend.key = element_blank())
