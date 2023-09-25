#--------------------------------- POWER POSITIONS ----------------------------#
################################################################################

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)

# download dataset -------------------------------------------------------------
board <- import("../statepol_dataset/00_dataset/board.csv") %>%
  select(-V1)

cabinet <- import("../statepol_dataset/00_dataset/cabinet.csv") %>%
  select(-V1)

committee <- import("../statepol_dataset/00_dataset/committee.csv") %>%
  select(-V1)

faction <- import("../statepol_dataset/00_dataset/faction.csv") %>%
  select(-V1)

list <- import("../statepol_dataset/00_dataset/list.csv") %>%
  select(-V1)

presidium <- import("../statepol_dataset/00_dataset/presidium.csv") %>%
  select(-V1)

# daily
cabinet.daily <- import("_data/output/cabinet.daily.rds")


# 1) power position datasets ---------------------------------------------------

## age und gender for join ##
age.gender <- list %>%
  select(id, gender, birthdate, term.start, term.end)

## party für join ##
party <- faction %>% 
  select(id, fraction.id)

# power position factions
pp.board <- board %>%
  filter(position.clean == "Fraktionsvorsitz"|
           position.clean == "Parlamentarische Geschäftsführung"|
           position.clean == "Stellvertretender Fraktionsvorsitz") %>%
  mutate(position = "Fraktionsvorstand") %>%
  rename("party" = "fraction.id") %>% 
  left_join(age.gender, by = "id") %>%
  select(id, gender, birthdate, position, party, start.date, end.date, 
         landtag.state.abb, electoralperiod, term.start, term.end)

# power position committee
pp.committee <- committee %>%
  filter(position.clean == "Vorsitz"|
           position.clean == "Stellvertretender Vorsitz") %>%
  mutate(position = "Ausschussvorsitz",
         party = NA) %>% 
  left_join(age.gender, by = "id") %>% 
  select(id, gender, birthdate, position, party, start.date, end.date, 
         landtag.state.abb, electoralperiod, term.start, term.end)

# power positions presidium
pp.presidium <- presidium %>% 
  mutate(position = "Präsidium",
         party = NA) %>%
  left_join(age.gender, by = "id") %>% 
  select(id, gender, birthdate, position, party, start.date, end.date, 
         landtag.state.abb, electoralperiod, term.start, term.end)

# power position parliments complete (board + committe + presidium)
pp.parl <- rbind(pp.board, pp.committee, pp.presidium) %>% 
  left_join(party, by = "id") %>%
  mutate(party = case_when(is.na(party) ~ fraction.id,
                           !is.na(party) ~ party)) %>% 
  select(-fraction.id)

# power positions cabinet
pp.cabinet <- cabinet.daily %>%
  filter(position.clean != "Andere Position") %>%
  select(id, gender, position.clean, party, start.date, end.date, 
         landtag.state.abb, electoralperiod, date)

# 2) export data ---------------------------------------------------------------

# base dataset 
export(board, "_data/output/board.rds")
export(cabinet, "_data/output/cabinet.rds")
export(committee, "_data/output/committee.rds")
export(faction, "_data/output/faction.rds")
export(list, "_data/output/list.rds")
export(presidium, "_data/output/presidium.rds")

# powerposition data
export(pp.parl, "_data/output/pp.parl.rds")
export(pp.cabinet, "_data/output/pp.cabinet.rds")