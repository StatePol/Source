setwd("/Users/eliaskoch/Desktop/germanparliments-paper")

library(pacman)
pacman::p_load(tidyverse,
               rio,
               ggplot2)


df_com <- rio::import("data/committee.rds") %>%
  mutate(budget = str_detect(committee, "aushalt")) %>% #Ermittlung von Haushaltsausschüssen
  filter(budget == T,
         budget == !str_detect(committee, c("Unteraussch*")),
         budget == !str_detect(committee, c("Haushaltskontrolle")),
         budget == !str_detect(committee, c("tädt*")),
         budget == !str_detect(committee, c("(Stadt)"))) %>%
  distinct() 
  

temp <- df_com %>%
  group_by(id, landtag.state.abb) %>% 
  tally()

df_com <- df_com %>%
  left_join(temp, by = "id") %>%
  mutate(drop = case_when(position.clean == "Mitglied" & n >= 2 ~ "yes",
                          T ~ "no")) %>%
  filter(drop == "no")



df_daily <- import("data/faction.daily.rds") %>%
  filter(id %in% df_com$id) %>%
  left_join(df_com, by = "id")


df_plot <- df_daily %>%
  mutate(fraction.id = as.factor(fraction.id),
         age = as.numeric(as.Date(date)-as.Date(birthdate))/365) %>%
  group_by(fraction.id, date) %>%
  summarise(avg_age = mean(age))

ggplot(df_plot, aes(x = date, y= avg_age, color = fraction.id))+
  geom_line()+
  scale_color_manual(values = c("SPD" = "#E3000F", 
                                "CDU/CSU" = "#000000",
                                "GRÜNE" = "#1AA037", 
                                "AfD" = "#0489DB",
                                "FDP" = "#FFEF00", 
                                "LINKE" = "deeppink"))


