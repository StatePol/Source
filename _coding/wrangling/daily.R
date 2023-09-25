#------------------------------ daily datasets --------------------------------#
################################################################################

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)

# download dataset -------------------------------------------------------------
cabinet <- import("_data/input/cabinet.csv") %>%
  select(-V1)

faction <- import("_data/input/faction.csv") %>%
  select(-V1)

list <- import("_data/input/list.csv") %>%
  select(-V1) %>%
  filter(!is.na(term.start))

# 1) daily dataset faction -----------------------------------------------------

faction.daily <- faction %>%
  left_join(list %>% 
              select(id, gender, birthdate, mandate),
            by = "id") %>%
  select(-name.wikitag) %>%
  drop_na(id, start.date, end.date) %>%
  filter(start.date <= end.date) %>%
  mutate(date = map2(start.date, end.date, seq, by = 1, units = "day")) %>%
  unnest(date)

# 2) daily dataset cabinet -----------------------------------------------------

cabinet.daily <- cabinet %>%
  left_join(list %>% 
              select(id, birthdate),
            by = "id") %>%
  select(-name.wikitag) %>%
  drop_na(id, start.date, end.date) %>%
  filter(start.date <= end.date) %>%
  mutate(date = map2(start.date, end.date, seq, by = 1, units = "day")) %>%
  unnest(date)

# 3) export data ---------------------------------------------------------------

export(faction.daily, "_data/output/faction.daily.rds")
export(cabinet.daily, "_data/output/cabinet.daily.rds")
