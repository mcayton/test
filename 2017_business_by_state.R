## From Shapesite Labs
## http://www.sharpsightlabs.com/blog/map-best-states-business/
## 4/11/18

library(tidyverse)
library(rvest)

html.states <- read_html("https://chiefexecutive.net/2017-best-worst-states-business/")

df.states <- html.states %>%
  html_nodes('table') %>%
  .[[1]] %>%
  html_table()
  
df.states %>% colnames()

colnames(df.states) <- c('2017_rank'
                      ,'state'
                      ,'description'
                      ,'2016_rank'
                      ,'change')

df.states %>% tbl_df()

map.usa <- map_data('state')

head(map.usa)

df.states <- df.states %>% mutate(state = str_to_lower(state)) %>%
  tbl_df()

df.states

ggplot(df.states, aes(map_id=state)) +
  geom_map(aes(fill =df.states$`2017_rank`), map = map.usa) +
  expand_limits(x=map.usa$long, y=map.usa$lat)
