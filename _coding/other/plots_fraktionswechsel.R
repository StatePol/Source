library(pacman)
pacman::p_load(tidyverse,
               ggplot2,
               rio,
               scales)

df_daily <- import("data/faction.daily.rds")
df_fac_raw <- import("data/faction.rds")
df_list <- import("data/list.rds")
df_toy <- df_daily[1:1000000,]



df_terms <- df_list %>%
  distinct(landtag.state.abb, electoralperiod, term.start, term.end)


df_fac <- df_fac_raw %>% 
  left_join(df_terms, by = c("landtag.state.abb", "electoralperiod")) %>%
  filter(start.date != term.start)


df_daily$type <- NA
df_daily$old <- NA
for (i in 2:nrow(df_daily)) {
  if (df_daily$id[i] == df_daily$id[i-1] && df_daily$fraction.id[i] != df_daily$fraction.id[i-1]) {
    df_daily$type[i] <- "Fraktionswechsel"
    df_daily$old[i] <-df_daily$fraction.id[i-1]
  }
}




df_switch <- df_daily %>%
  filter(type == "Fraktionswechsel")%>%
  mutate(fraction.id = case_when(fraction.id == "Saar-Linke" ~ "LINKE",
                                 fraction.id %in% c("fraktionlos", "fraktionslos") ~ "fraktionslos",
                                 fraction.id == "AfD" ~ "AfD",
                                 fraction.id == "CDU" ~ "CDU",
                                 fraction.id %in% c("FDVP", "FDP") ~ "FDP",
                                 fraction.id %in% c("Saar-Linke", "LINKE") ~"LINKE",
                                 fraction.id == "GRÜNE" ~ "GRÜNE",
                                 fraction.id == "SPD" ~ "SPD",
                                 T ~ "andere"),
         old = case_when(old == "Saar-Linke" ~ "LINKE",
                                 old %in% c("fraktionlos", "fraktionslos") ~ "fraktionslos",
                                 old == "AfD" ~ "AfD",
                                 old == "CDU" ~ "CDU",
                                 old %in% c("FDVP", "FDP") ~ "FDP",
                                 old %in% c("Saar-Linke", "LINKE") ~"LINKE",
                                 old == "GRÜNE" ~ "GRÜNE",
                                 old == "SPD" ~ "SPD",
                                 old == "fraktionslos" ~ "fraktionslos",
                                 T ~ "andere"),
         old = factor(old, levels = c("CDU", "SPD", "FDP", "GRÜNE", "LINKE", "AfD", "andere", "fraktionslos")),
         fraction.id = factor(fraction.id, levels = c("CDU", "SPD", "FDP", "GRÜNE", "LINKE", "AfD", "andere", "fraktionslos"))) %>%
  arrange(id, date)

#OPTION FÜR ohne fraktionslos: Wenn fraction.id == "fraktionslos" und drüber und drunter gleicher id: Fraktionslos-Zeile löschen; sonst behalten



  

# Create a new data frame
df_switch_no_fraktionslos <- df_switch

# Initialize the changed variable
df_switch_no_fraktionslos$changed <- FALSE

# Iterate through each row in the data frame
for (i in 1:(nrow(df_switch_no_fraktionslos) - 1)) {
  # Check if the current row has fraction.id as "fraktionslos" and id is not missing
  if (!is.na(df_switch_no_fraktionslos$fraction.id[i]) && df_switch_no_fraktionslos$fraction.id[i] == "fraktionslos") {
    # Check if the next row has a different fraction.id, a higher start.date, and id is not missing
    if (!is.na(df_switch_no_fraktionslos$id[i]) && df_switch_no_fraktionslos$id[i] == df_switch_no_fraktionslos$id[i+1] &&
        !is.na(df_switch_no_fraktionslos$fraction.id[i+1]) && df_switch_no_fraktionslos$fraction.id[i] != df_switch_no_fraktionslos$fraction.id[i+1] &&
        df_switch_no_fraktionslos$start.date[i] < df_switch_no_fraktionslos$start.date[i+1]) {
      
      # Replace "fraktionslos" with the fraction.id and start.date from the next row
      df_switch_no_fraktionslos$fraction.id[i] <- df_switch_no_fraktionslos$fraction.id[i+1]
      df_switch_no_fraktionslos$start.date[i] <- df_switch_no_fraktionslos$start.date[i+1]
      
      # Update the changed variable to TRUE
      df_switch_no_fraktionslos$changed[i] <- TRUE
      
      # Remove the next row with fraction.id as "fraktionslos"
      df_switch_no_fraktionslos <- df_switch_no_fraktionslos[-(i+1), ]
    }
  }
}



df_switch_no_fraktionslos <- df_switch_no_fraktionslos %>%
  mutate(fraction.id = recode(fraction.id, "fraktionslos" = "blieben fraktionslos"))


bar_order <- names(sort(-table(df_switch$old)))
abb_fraktionslos <- ggplot(df_switch, aes(x = factor(old, levels = bar_order), fill = fraction.id)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("CDU" = "black",
                               "SPD" = "#E3000F", 
                               "CDU/CSU" = "#000000",
                               "GRÜNE" = "#1AA037", 
                               "AfD" = "#0489DB",
                               "FDP" = "#FFEF00", 
                               "LINKE" = "deeppink",
                               "andere" = "grey",
                               "fraktionslos" = "pink"),
                    name = "...wurden die Abgeordneten\nMitglieder dieser Fraktionen") +
  labs(x = "Nach dem Austritt aus diesen Fraktionen...",
       y = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  theme(axis.title.x = element_text(margin = margin(t = 15)))

abb_fraktionslos

bar_order <- names(sort(-table(df_switch_no_fraktionslos$old)))
abb_ohne_fraktionslos <- ggplot(df_switch_no_fraktionslos, aes(x = factor(old, levels = bar_order), fill = fraction.id)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("CDU" = "black",
                               "SPD" = "#E3000F", 
                               "CDU/CSU" = "#000000",
                               "GRÜNE" = "#1AA037", 
                               "AfD" = "#0489DB",
                               "FDP" = "#FFEF00", 
                               "LINKE" = "deeppink",
                               "andere" = "grey",
                               "blieben fraktionslos" = "pink"),
                    name = "...wurden die Abgeordneten\nMitglieder dieser Fraktionen") +
  labs(x = "Nach dem Austritt aus diesen Fraktionen...",
       y = "") +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(t = 2.5, r = 5.5, b = 0, l = 5.5, unit = "pt"), # Modify plot margins
        plot.title = element_blank(), # Ensure no title is displayed
        plot.subtitle = element_blank(), # Ensure no subtitle is displayed
        plot.caption = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))+
  theme(axis.title.x = element_text(margin = margin(t = 15)))

abb_ohne_fraktionslos

df_timing <- df_switch %>%
  left_join(df_terms, by = c("landtag.state.abb", "electoralperiod")) %>%
  mutate(until_next_elec = as.character(as.Date(term.end) - as.Date(date)),
         full_term_duration = as.character(as.Date(term.end) - as.Date(term.start)),
         full_term_duration = as.numeric(gsub(" days", "", full_term_duration)),
         until_next_elec = as.numeric(gsub(" days", "", until_next_elec)),
         since_term_start = full_term_duration-until_next_elec,
         term_progress = since_term_start/full_term_duration) %>%
  group_by(term_progress, old) %>%
  tally() %>%
  filter(old != "fraktionslos")
  


abb_density_term_progress <- ggplot(df_timing, aes(x = term_progress, color = old)) +
  geom_density() +
  scale_color_manual(values = c("CDU" = "black",
                                "SPD" = "#E3000F", 
                                "CDU/CSU" = "#000000",
                                "GRÜNE" = "#1AA037", 
                                "AfD" = "#0489DB",
                                "FDP" = "#FFEF00", 
                                "LINKE" = "deeppink",
                                "andere" = "grey"),
                     name = "Fraktionen, die\nAbgeordnete verlassen") +
  scale_x_continuous(labels = percent_format())+
  labs(x = "Term Progress",
       y = "")+
  theme_minimal()

abb_density_term_progress

       