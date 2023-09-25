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


# base level datasets ----------------------------------------------------------

# dataset for gendershare total
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

# daily pp.parl
pp.parl.daily <- pp.parl %>%
  drop_na(id, term.start, term.end) %>%
  filter(start.date <= end.date) %>%
  mutate(date = map2(term.start, term.end, seq, by = 1, units = "day")) %>%
  unnest(date)

# gender by position vers 2 ----------------------------------------------------

# datawrangling
pp.parl.gendershare <- pp.parl.daily %>% 
  select(id, party, position, gender, date) %>%
  mutate(position = recode(position,
                           "Fraktionsvorstand" = "Fraktionsführung",
                           "Minister/in" = "Minister:in")) %>% 
  filter(party %in% c("AfD", "CSU","SPD", "CDU",
                      "FDP", "GRÜNE", "LINKE")) %>% 
  group_by(date, position, gender, .drop = FALSE) %>% 
  tally() %>%
  ungroup() %>%
  group_by(date, position) %>% 
  mutate(sum = sum(n),
         percentage = n/sum) %>%
  filter(gender == "female",
         date >= "1990-01-01",
         date <= "2020-01-01")


pp.cabinet.gendershare <- pp.cabinet %>%
  filter(position.clean != "Stellvertretendes Regierungsoberhaupt") %>%
  mutate(position.clean = recode(position.clean,
                                 "Regierungsoberhaupt" = "Ministerpräsident:in",
                                 "Minister/in" = "Minister:in")) %>% 
  group_by(date, gender, position.clean) %>% 
  tally() %>% 
  group_by(date, position.clean) %>% 
  mutate(sum = sum(n),
         percentage = n/sum) %>%
  filter(gender == "female",
         date >= "1990-01-01",
         date <= "2020-01-01")

# plotting
plot_genderposition_vers2 <- ggplot() +
    geom_smooth(data = pp.cabinet.gendershare, se = FALSE, linewidth = .8, aes(x = date, y = percentage, color = position.clean)) +
    geom_smooth(data = pp.parl.gendershare, se = FALSE, linewidth = .8, aes(x = date, y = percentage, color = position)) +
    geom_point(data = pp.cabinet.gendershare, aes(x = date, y = percentage, color = position.clean), size = .05, alpha = .01) + 
    geom_point(data = pp.parl.gendershare, aes(x = date, y = percentage, color = position), size = .05, alpha = .01) + 
    geom_hline(yintercept = 0.5, size = .2, linetype = "dashed") +
    geom_smooth(data = gender.comp, se = FALSE, linewidth = .5, linetype = "dotted",
                aes(x = date, y = percentage, color = Durchschnitt)) +
    scale_color_manual(values = c("Insg." = "black",
                                  "Ausschussvorsitz" = "#002642",
                                  "Fraktionsführung" = "#E59500",
                                  "Präsidium" = "#107E7D",
                                  "Minister:in" = "#E63946",
                                  "Ministerpräsident:in" = "#7EA172"),
                       breaks = c("Insg.", "Präsidium", "Ausschussvorsitz", "Fraktionsführung", "Ministerpräsident:in", "Minister:in")) +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank()) +
    labs(x = "",
         y = "",
         color = "") +
    guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5))))) +
    scale_y_continuous(breaks = seq(0, 0.6, 0.1),
                       labels = scales::percent_format(accuracy = 1)) +
    scale_x_date(expand = c(0, 0), 
                 breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
                 labels = c("1990", "2000", "2010", "2020")) +
    coord_cartesian(ylim = c(0, 0.55))
  
# save plot
ggsave("plots/gender_position_vers2.png", plot = plot_genderposition_vers2, dpi = 500, width = 180, height = 120, units = "mm")

# remove process datafiles
rm(plot_genderposition, pp.parl.gendershare)

# gender by position and party 1, 2 and 3 --------------------------------------

# datawrangling
pp.parl.gendershare.party <- pp.parl.daily %>% 
  select(id, party, position, gender, date) %>%
  filter(party %in% c("CSU","SPD", "CDU",
                      "FDP", "GRÜNE", "LINKE")) %>%
  mutate(party = recode(party,
                        "CSU" = "CDU/CSU",
                        "CDU" = "CDU/CSU"),
         position = recode(position,
                           "Fraktionsvorstand" = "Fraktionsführung")) %>% 
  group_by(date, party, position, gender, .drop = FALSE) %>%
  tally() %>%
  ungroup() %>%
  group_by(date, party, position) %>% 
  mutate(sum = sum(n),
         percentage = n/sum,
         party = factor(party, levels = c("CDU/CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AfD"))) %>%
  filter(gender == "female",
         date >= "1990-01-01",
         date <= "2020-01-01")


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
plot_gender_position_party <- ggplot()+
  geom_smooth(data = pp.cabinet.gendershare.party, se=FALSE, size = .5,aes(x = date, y = percentage, color = position.clean))+
  geom_smooth(data = pp.parl.gendershare.party, se=FALSE, size = .5,aes(x = date, y = percentage, color = position))+
  geom_point(data = pp.cabinet.gendershare.party, se=FALSE, size = .05, alpha = .008, aes(x = date, y = percentage, color = position.clean))+
  geom_point(data = pp.parl.gendershare.party, se=FALSE, size = .05, alpha = .008, aes(x = date, y = percentage, color = position))+
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
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5)))))+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_x_date(expand = c(0, 0), 
                 breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
                 labels = c("1990", "2000", "2010", "2020")) +
  coord_cartesian(ylim = c(0, 1))

# save plot
ggsave("plots/gender_position_party.png", plot = plot_gender_position_party, dpi = 500, width = 180, height = 135, units = "mm")


# plotting add version 2: facet_wrap nach parteien nur mit parlamentspositionen
plot_gender_position_party_add2 <- ggplot()+
  geom_smooth(data = pp.parl.gendershare.party, se=FALSE, size = .5, aes(x = date, y = percentage, color = position))+
  geom_smooth(data = gender.comp, se=FALSE, size = .5, linetype = "dotted",
              aes(x = date, y = percentage, color = Durchschnitt))+
  geom_point(data = pp.parl.gendershare.party, se=FALSE, size = .05, alpha = .008, aes(x = date, y = percentage, color = position))+
  facet_wrap(~party, nrow = 3, ncol = 2)+
  scale_color_manual(values = c("Insg." = "black",
                                "Ausschussvorsitz" = "#002642", 
                                "Fraktionsführung" = "#E59500",
                                "Präsidium" = "#107E7D"),
                     breaks = c("Insg.", "Präsidium", "Ausschussvorsitz", "Fraktionsführung"))+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 3)))))+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_x_date(expand = c(0, 0), 
               breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
               labels = c("1990", "2000", "2010", "2020"))

# save plot
ggsave("plots/plot_gender_position_party_add2.png", plot = plot_gender_position_party_add2, dpi = 500, width = 180, height = 120, units = "mm")

# plotting add version 3: facet_wrap nach parteien nur mit kabinettspositionen
plot_gender_position_party_add3 <- ggplot()+
  geom_line(data = pp.cabinet.gendershare.party, size = .2, aes(x = date, y = percentage, color = position.clean))+
  geom_smooth(data = gender.comp, se=FALSE, size = .5, linetype = "dotted",
              aes(x = date, y = percentage, color = Durchschnitt))+
  geom_point(data = pp.cabinet.gendershare.party, se=FALSE, size = .05, alpha = .008, aes(x = date, y = percentage, color = position.clean))+
  facet_wrap(~party, nrow = 3, ncol = 2) + 
  scale_color_manual(values = c("Insg." = "black",
                                "Minister:in" = "#E63946",
                                "Ministerpräsident:in" = "#7EA172"),
                     breaks = c("Insg.", "Ministerpräsident:in", "Minister:in"))+
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 2)))))+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1))+
  scale_x_date(expand = c(0, 0), 
               breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
               labels = c("1990", "2000", "2010", "2020"))

# save plot
ggsave("plots/plot_gender_position_party_add3.png", plot = plot_gender_position_party_add3, dpi = 500, width = 180, height = 120, units = "mm")
