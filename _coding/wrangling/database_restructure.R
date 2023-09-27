
# packages
library(tidyverse)
library(rio)

# data import
list <- import("_data/input/list.csv")
cabinet <- import("_data/input/cabinet.csv")

# mandate dataframe
mandate <- list %>%
  select(id, name.wikitag, constituency, mandate, voteshare, landtag.state,
         landtag.state.abb, electoralperiod, term.start, term.end) %>%
  mutate_if(is.character, ~na_if(., ''))

# cabinet dataframe
new_cab <- cabinet %>%
  select(id, name.wikitag, ministry, ministry.clean, position, position.clean,
         cabinet, party, economy:electoralperiod)

# distinct dataframe for parliment
legislative <- list %>%
  rename(birthplace.latitude = birthplace.lat.geocode,
         birthplace.longititude = birthplace.lon.geocode) %>% 
  select(name, name.wikitag, birthplace, birthplace.wikitag,
         birthplace.latitude, birthplace.longititude, birthplace.region,
         birthplace.subregion, birthplace.country, birthplace.country.code,
         birthplace.state, birthplace.state.code, birthplace.city, birthdate, gender) %>%
  distinct(name.wikitag, .keep_all = T)

# distinct dataframe for executive
executive <- cabinet %>%
  rename(birthplace.latitude = latitude,
         birthplace.longititude = longitude) %>% 
  select(name, name.wikitag, birthplace, birthplace.wikitag,
         birthplace.latitude, birthplace.longititude, birthplace.region,
         birthplace.subregion, birthplace.country, birthplace.country.code,
         birthplace.state, birthplace.state.code, birthplace.city, birthdate, gender) %>%
  distinct(name.wikitag, .keep_all = T)

# bind_rows
politicians <- bind_rows(legislative, executive) %>%
  mutate_if(is.character, ~na_if(., '')) %>% 
mutate_if(is.character, ~na_if(., 'NA')) %>% 
  distinct(name.wikitag, .keep_all = T) %>%
  arrange(name)

# export
export(mandate, "_data/output/mandate.csv")
export(politicians, "_data/output/politicians.csv")
export(new_cab, "_data/output/cabinet.csv")
