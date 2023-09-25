#--------------------------------- PLOTS --------------------------------------#
################################################################################

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)


# download dataset -------------------------------------------------------------

# daily
cabinet.daily <- import("data/cabinet.daily.rds")
faction.daily <- import("data/faction.daily.rds")

# power position
pp.parl <- import("data/pp.parl.rds")
pp.cabinet <- import("data/pp.cabinet.rds")


# gender by position and party -------------------------------------------------

# datawrangling
gender.comp <- faction.daily %>%
  group_by(date, gender) %>%
  tally() %>%
  group_by(date) %>%
  mutate(sum = sum(n),
         percentage = n/sum,
         Durchschnitt = "Insg.") %>%
  filter(date >= "1990-01-01",
         date <= "2019-12-31",
         gender == "female")

# pp.parl daily machen
pp.parl.daily <- pp.parl %>%
  drop_na(id, term.start, term.end) %>%
  filter(start.date <= end.date) %>%
  mutate(date = map2(term.start, term.end, seq, by = 1, units = "day")) %>%
  unnest(date)


pp.parl.gendershare.party <- pp.parl.daily %>% 
  select(party, position, gender, date) %>%
  filter(party %in% c("CSU","SPD", "CDU",
                      "FDP", "GRÜNE", "LINKE")) %>%
  mutate(party = recode(party,
                        "CSU" = "CDU/CSU",
                        "CDU" = "CDU/CSU"),
         position = recode(position,
                           "Fraktionsvorstand" = "Fraktionsführung")) %>% 
  group_by(date, party, position, .drop = FALSE) %>%
  add_tally() %>%
  rename(total = n) %>% 
  ungroup() %>%
  group_by(date, party, position, gender) %>% 
  add_tally() %>%
  rename(total_bygender = n) %>%
  mutate(percentage = total_bygender/total,
         party = factor(party, levels = c("CDU/CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AfD"))) %>%
  filter(gender == "female",
         date >= "1990-01-01")


pp.cabinet.gendershare.party <- pp.cabinet %>%
  mutate(party = recode(party, "PDS" = "LINKE",
                        "PDS-LL" = "LINKE",
                        "CSU" = "CDU/CSU",
                        "CDU" = "CDU/CSU"),
         position.clean = recode(position.clean,
                                 "Regierungsoberhaupt" = "Ministerpräsident:in",
                                 "Minister/in" = "Minister:in")) %>%
  filter(party %in% c("SPD", "FDP", "CDU/CSU", "GRÜNE", "LINKE"),
         position.clean %in% c("Ministerpräsident:in", "Minister:in")) %>%
  drop_na() %>%
  group_by(date, party, position.clean, gender, .drop = FALSE) %>% 
  tally() %>% 
  group_by(date, party, position.clean) %>% 
  mutate(sum = sum(n),
         percentage = n/sum,
         party = factor(party, levels = c("CDU/CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AfD"))) %>%
  filter(gender == "female",
         date >= "1990-01-01",
         date <= "2020-01-01")


# plotting
#plot_gender_position_party <-
  ggplot()+
  geom_line(data = pp.cabinet.gendershare.party, se=FALSE, size = .5,aes(x = date, y = percentage, color = position.clean))+
  geom_smooth(data = pp.parl.gendershare.party, method = "loess",se=FALSE, size = .5,aes(x = date, y = percentage, color = position))+
  geom_smooth(data = gender.comp, se=FALSE, size = .5, linetype = "dotted",
              aes(x = date, y = percentage, color = Durchschnitt))+
  facet_wrap(~party, nrow = 3, ncol = 2) + 
  scale_color_manual(values = c("Insg." = "black",
                                "Ausschussvorsitz" = "#002642", 
                                "Fraktionsführung" = "#E59500",
                                "Präsidium" = "#107E7D",
                                "Minister:in" = "#E63946",
                                "Ministerpräsident:in" = "#7EA172"),
                     breaks = c("Insg.", "Präsidium", "Ausschussvorsitz", "Fraktionsführung", "Ministerpräsident:in", "Minister:in"))+
  theme_bw()+
  theme(text=element_text(family="Times New Roman"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5)))))+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     labels = scales::percent_format(accuracy = 1))+
  coord_cartesian(ylim = c(0, 1))
  
  
  
  ggplot()+
    geom_line(data = pp.cabinet.gendershare.party, se=FALSE, size = .5,aes(x = date, y = percentage, color = position.clean))+
    geom_smooth(data = pp.parl.gendershare.party, method = "loess", se=FALSE, size = .5,aes(x = date, y = percentage, color = position))+
    geom_smooth(data = gender.comp, se=FALSE, size = .5, linetype = "dotted",
                aes(x = date, y = percentage, color = Durchschnitt))+
    facet_wrap(~party, nrow = 3, ncol = 2) + 
    scale_color_manual(values = c("Insg." = "black",
                                  "Ausschussvorsitz" = "#002642", 
                                  "Fraktionsführung" = "#E59500",
                                  "Präsidium" = "#107E7D",
                                  "Minister:in" = "#E63946",
                                  "Ministerpräsident:in" = "#7EA172"),
                       breaks = c("Insg.", "Präsidium", "Ausschussvorsitz", "Fraktionsführung", "Ministerpräsident:in", "Minister:in"))+
    theme_bw()+
    theme(text=element_text(family="Times New Roman"),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8))+
    labs(x = "",
         y = "",
         color = "",
         subtitle = "")+
    guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5)))))+
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = scales::percent_format(accuracy = 1))+
    coord_cartesian(ylim = c(0, 1))
  
