library(dplyr)
library(readr)

ohl_201718 <- read_csv("2017-18 OHL Skaters.csv") %>% mutate(season = "201718")
ohl_201617 <- read_csv("2016-17 OHL Skaters.csv") %>% mutate(season = "201617")
ohl_201516 <- read_csv("2015-16 OHL Skaters.csv") %>% mutate(season = "201516")

ohl_data <- bind_rows(ohl_201718, ohl_201617, ohl_201516) %>%
  rename(HD_G = "HD G", HD_Sh = "HD Sh", MD_G = "MD G", MD_Sh = "MD Sh", LD_G = "LD G", LD_Sh = "LD Sh")

avg_pct <- ohl_data %>%
  select(HD_G, HD_Sh, MD_G, MD_Sh, LD_G, LD_Sh) %>%
  summarize_all(funs(sum)) %>%
  summarize(hd_shot_pct = (HD_G / HD_Sh), 
            md_shot_pct = (MD_G / MD_Sh),
            ld_shot_pct = (LD_G / LD_Sh))

xg_data <- ohl_data %>%
  mutate(x_goals = (avg_pct$hd_shot_pct * HD_Sh) + (avg_pct$md_shot_pct * MD_Sh) + (avg_pct$ld_shot_pct * LD_Sh),
         x_goals_60 = ((x_goals * 60) / eTOI)) %>%
  filter(x_goals_60 != "Inf") %>%
  select("#", Name, season, Pos, Team, Age, GP, x_goals, x_goals_60, everything()) %>%
  arrange(desc(x_goals_60))




