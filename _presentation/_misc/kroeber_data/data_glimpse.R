# Checking the Kroeber Data ----------------------------------------------------

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)

# data import ------------------------------------------------------------------

kroeber <- import("kroeber_data_v1-0-0.dta")
mueller <- import("/Users/danielkuhlen/Desktop/work/darmstadt/projekte/germanparliments-paper/data/list.rds")

# glimpse at the data ----------------------------------------------------------

# welche variablen gibt es in den Daten? welche überschneiden sich?
colnames(kroeber)

relevant <- kroeber %>%
  select(1:45)

# testen ob wir die gleichen cases haben ---------------------------------------

## bb_01 nach kroeber 
kroeber_bb_01 <- relevant %>%
  filter(state == "bb" &
           elec_period == 1) %>%
  select(mp_lastname, mp_firstname) %>%
  mutate(name = paste(mp_firstname, mp_lastname, sep = " "))

## bb_01 nach mueller
mueller_bb_01 <- mueller %>%
  filter(landtag.state.abb == "bb" &
           electoralperiod == 1) %>%
  select(name) %>%
  mutate(included_kroeber = name %in% kroeber_bb_01$name)

# wichtig: es scheinen systematisch nachrücker:innen zu fehlen!

# nochmal testen für nrw
## bb_01 nach kroeber 
kroeber_nw_01 <- relevant %>%
  filter(state == "nrw" &
           elec_period == 14) %>%
  select(mp_lastname, mp_firstname) %>%
  mutate(name = paste(mp_firstname, mp_lastname, sep = " "))

## nw_01 nach mueller
mueller_nw_01 <- mueller %>%
  filter(landtag.state.abb == "nw" &
           electoralperiod == 14) %>%
  select(name) %>%
  mutate(included_kroeber = name %in% kroeber_nw_01$name)

# WIEVIELE MDL'S fehlen insgesamt?

kroeber_mdls <- kroeber %>%
  select(mp, mp_lastname, mp_firstname) %>%
  mutate(name = paste(mp_firstname, mp_lastname, sep = " ")) %>%
  distinct(mp, .keep_all = TRUE)

mueller_mdls <- mueller %>%
  select(name) %>%
  mutate(included_kroeber = name %in% kroeber_mdls$name) %>%
  filter(included_kroeber == "FALSE")


