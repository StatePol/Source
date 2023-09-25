#--------------------------------- PLOTS --------------------------------------#
################################################################################

# packages ---------------------------------------------------------------------
library(tidyverse)
library(rio)
library(apyramid)

# download dataset -------------------------------------------------------------

# other 
age <- import("../germanparliments/dataset/list.csv") %>% select(id, birthdate)

# daily
cabinet.daily <- import("data/cabinet.daily.rds")
faction.daily <- import("data/faction.daily.rds")

# power position
pp.parl <- import("data/pp.parl.rds")
pp.cabinet <- import("data/pp.cabinet.rds")


# age by party -----------------------------------------------------------------

# complete
age.comp <- faction.daily %>%
  mutate(age = as.numeric(date-birthdate)/365, units = "years",
         fraction.id = recode(fraction.id, "CSU" = "CDU/CSU",
                              "CDU" = "CDU/CSU")) %>%
  drop_na(age) %>%
  group_by(date) %>%
  summarise(mean_age = mean(age)) %>%
  filter(date <="2020-01-01",
         date >= "1990-01-01") %>%
  mutate(average = "Insg.")

# by party
age.party <- faction.daily %>%
  mutate(age = as.numeric(date-birthdate)/365, units = "years",
         fraction.id = recode(fraction.id, "CSU" = "CDU/CSU",
                              "CDU" = "CDU/CSU")) %>%
  drop_na(age) %>%
  group_by(date, fraction.id) %>%
  summarise(mean_age = mean(age)) %>%
  filter(fraction.id %in% c("SPD", "AfD", "CDU/CSU", "FDP", "GRÜNE", "LINKE"),
         date <="2020-01-01",
         date >= "1990-01-01")

# plotting
plot_age_faction <- ggplot()+
  geom_line(data = age.party, aes(x = date, y = mean_age, color = fraction.id))+
  geom_smooth(data = age.comp, se = FALSE, size = 0.5, linetype = "dotted",
              aes(x = date, y = mean_age, color = average))+
    scale_color_manual(values = c("Insg." = "#000000", 
                                  "Durchschnitt" = "black",
                                  "SPD" = "#E3000F", 
                                  "CDU/CSU" = "#000000",
                                  "GRÜNE" = "#1AA037", 
                                  "AfD" = "#0489DB",
                                  "FDP" = "#FFEF00", 
                                  "LINKE" = "deeppink"),
                       breaks = c("Insg.", "CDU/CSU", "SPD", "GRÜNE", "LINKE", "FDP", "AfD")) +
  theme_bw()+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 6))))) +
  scale_x_date(expand = c(0, 0),
               breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
               labels = c("1990", "2000", "2010", "2020")) +
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")

# save plot
ggsave("plots/age_faction.png", plot = plot_age_faction, dpi = 500, width = 180, height = 120, units = "mm")


# remove process datafiles
rm(age.party, plot_age_faction)

# age by position --------------------------------------------------------------

# parliment
pp.parl.age <- pp.parl %>%
  mutate(birthdate = as.Date(birthdate),
         term.start = as.Date(term.start),
         age = as.numeric(term.start-birthdate)/365, units = "years",
         position = recode(position,
                           "Fraktionsvorstand" = "Fraktionsführung")) %>%
  drop_na(age) %>% 
  group_by(term.start, position) %>%
  summarise(mean_age = mean(age))

# cabinet
pp.cabinett.age <- pp.cabinet %>%
  left_join(age, by = "id") %>%
  mutate(birthdate = as.Date(birthdate),
         date = as.Date(date),
         age = as.numeric(date-birthdate)/365,
         position.clean = recode(position.clean,
                                 "Minister/in" = "Minister:in",
                                 "Regierungsoberhaupt" = "Ministerpräsident:in")) %>%
  drop_na(age) %>% 
  filter(position.clean %in% c("Minister:in", "Ministerpräsident:in")) %>%
  group_by(date, position.clean) %>%
  summarise(mean_age = mean(age))

# plotting
#plot_age_position <- 
  ggplot()+
  geom_line(data = pp.cabinett.age, size = .5,
            aes(x = date, y = mean_age, color = position.clean))+
  geom_smooth(data = pp.parl.age, se=FALSE, size = .5,
              aes(x = term.start, y = mean_age, color = position))+
  geom_smooth(data = age.comp, se=FALSE, linetype = "dotted", size = .5,aes(x = date, y = mean_age, color = average))+
    scale_color_manual(values = c("Insg." = "black",
                                  "Ausschussvorsitz" = "#002642", 
                                  "Fraktionsführung" = "#E59500",
                                  "Präsidium" = "#107E7D",
                                  "Minister:in" = "#E63946",
                                  "Ministerpräsident:in" = "#7EA172"),
                       breaks = c("Insg.", "Präsidium", "Ausschussvorsitz", "Fraktionsführung", "Ministerpräsident:in", "Minister:in"))+
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5)))))+
  theme(text=element_text(family="Times New Roman"))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  coord_cartesian(ylim = c(35, 70))+
  xlim(as.Date("1990-01-01"), as.Date("2020-01-01"))
  
# save plot
ggsave("plots/age_position.png", plot = plot_age_position, dpi = 500, width = 180, height = 135, units = "mm")

# remove process datafiles
rm(age.comp, plot_age_position, pp.cabinett.age, pp.parl.age)


# age by position option 2 -----------------------------------------------------

# daily pp.parl
pp.parl.daily <- pp.parl %>%
  drop_na(id, term.start, term.end) %>%
  filter(start.date <= end.date) %>%
  mutate(date = map2(term.start, term.end, seq, by = 1, units = "day")) %>%
  unnest(date)

# parliment
pp.parl.age <- pp.parl.daily %>%
  mutate(birthdate = as.Date(birthdate),
         term.start = as.Date(term.start),
         age = as.numeric(term.start-birthdate)/365, units = "years",
         position = recode(position,
                           "Fraktionsvorstand" = "Fraktionsführung")) %>%
  drop_na(age) %>% 
  group_by(date, position) %>%
  summarise(mean_age = mean(age)) %>% 
filter(date >= "1990-01-01",
       date <= "2020-01-01")

# cabinet
pp.cabinett.age <- pp.cabinet %>%
  left_join(age, by = "id") %>%
  mutate(birthdate = as.Date(birthdate),
         date = as.Date(date),
         age = as.numeric(date-birthdate)/365,
         position.clean = recode(position.clean,
                                 "Minister/in" = "Minister:in",
                                 "Regierungsoberhaupt" = "Ministerpräsident:in")) %>%
  drop_na(age) %>% 
  filter(position.clean %in% c("Minister:in", "Ministerpräsident:in")) %>%
  group_by(date, position.clean) %>%
  summarise(mean_age = mean(age)) %>% 
  filter(date >= "1990-01-01",
         date <= "2020-01-01")

# plotting
plot_age_position <- ggplot()+
  geom_smooth(data = pp.cabinett.age, se=FALSE, size = .5,
            aes(x = date, y = mean_age, color = position.clean))+
  geom_smooth(data = pp.parl.age, se=FALSE, size = .5,
              aes(x = date, y = mean_age, color = position))+
  geom_point(data = pp.cabinett.age, size = .05, alpha = .01,
              aes(x = date, y = mean_age, color = position.clean))+
  geom_point(data = pp.parl.age, size = .05, alpha = .01,
              aes(x = date, y = mean_age, color = position))+
  geom_smooth(data = age.comp, se=FALSE, linetype = "dotted", size = .5,aes(x = date, y = mean_age, color = average))+
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
        plot.caption = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c("dotted", rep("solid", 5)))))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "")+
  coord_cartesian(ylim = c(35, 70))+
  scale_x_date(expand = c(0, 0), 
               breaks = as.Date(c("1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
               labels = c("1990", "2000", "2010", "2020"))

# save plot
ggsave("plots/age_position.png", plot = plot_age_position, dpi = 500, width = 180, height = 120, units = "mm")


# Alterspyramidem
  



plot_years <- seq(1991, 2020, 1)

for (i in plot_years) {
  temp_year <- paste0(i)
  temp_date <- paste0(i, "-01-01")
  plot_name <- paste0("pyramid_", i)
  
  temp_base <- faction.daily %>%
    filter(date == as.Date(temp_date),
           gender !="") %>%
    group_by(landtag.state.abb) %>%
    mutate(age = as.numeric(as.Date(date)-as.Date(birthdate))/365,
           age_group = round(age)) %>%
    group_by(gender, age_group) %>%
    tally() %>%
    select(age_group, gender, n) %>%
    ungroup() %>%
    pivot_wider(names_from = gender, values_from = n) %>%
    mutate(male_count = case_when(is.na(male) ~ 0, T ~ male),
           female_count = case_when(is.na(female) ~ 0, T ~ female)) %>%
    select(-c("male", "female")) 
  
  sum <- sum(temp_base$male_count)+sum(temp_base$female_count)
  
  
  temp_combined_data <- 
    data.frame(age_group = temp_base$age_group,
               count = c(-temp_base$female_count, temp_base$male_count),
               gender = rep(c("Female", "Male"), each = length(temp_base$age_group))) %>%
    pivot_wider(names_from = gender, values_from = count) %>%
    mutate(perc_female = Female/sum,
           perc_male = Male/sum)%>%
    rowwise() %>%
    mutate(midpoint = (perc_female+perc_male) / 2) %>% 
    pivot_longer(cols = c("perc_female", "perc_male"),
                 names_to = "gender",
                 values_to = "perc")
  
  
temp_plot <- ggplot(temp_combined_data, aes(x = age_group, y = perc, fill = gender)) +
    geom_bar(stat = "identity", position = "identity")+
    geom_point(aes(y=midpoint), show.legend = F)+
    coord_flip()+
    labs(x = "Age Group", y = "Count", title = "Age Pyramid") +
    theme_minimal()+
    scale_linetype_manual(values = "dashed", guide = guide_legend(title = NULL, order = 2))+
    scale_fill_manual(                             # specify colors AND labels
      values = c("#bc4749", "#457b9d"),              
      labels = c("perc_male" = "Männlich", "perc_female" = "Weiblich"),
      name = "Geschlecht")+
    scale_y_continuous(labels = function(x) percent(abs(x)), limits = c(-0.05, 0.05))+ 
    scale_x_continuous(limits = c(20, 80), breaks = seq(20, 80, 5))+
  labs(x = "",
       y = "",
       color = "",
       subtitle = "",
       title = i)+  #wenn gewünscht, option weiter unten auskommentieren
  theme(text = element_text(family = "Times New Roman"),
          plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"),
          plot.title = element_text(hjust = .5, face = "bold"), # Ensure no title is displayed
          plot.subtitle = element_blank(), 
          plot.caption = element_blank(),
          strip.text = element_text(size = 6),
          panel.background = element_rect(fill = "white"), 
          plot.background = element_rect(fill = "white"))


ggsave(plot = temp_plot, paste0("plots/Alterspyramiden/", i, ".png"), dpi = 500, width = 180, height = 120, units = "mm")
}

