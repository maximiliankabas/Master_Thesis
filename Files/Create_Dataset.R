rm(list = ls())

library(tidyverse)
library(dplyr)
library(here)
library(tsibble)
here()

#dir.create("/Users/maximiliankabas/Desktop/Master_Thesis_Erich/Data")
#dir.create("/Users/maximiliankabas/Desktop/Master_Thesis_Erich/Files")
#dir.create("/Users/maximiliankabas/Desktop/Master_Thesis_Erich/Output")

# Loading the Data ----
load(here("Data/dvb_muni_weekly_TH_MKabas.RData"))
load(here("Data/popdat_aut.RData"))


# Merging the Data ----

dvb_muni_weekly <- dvb_muni_weekly %>% 
  mutate(
    year = year(yrweek),
    week = week(yrweek)
  )

popdat_aut <- popdat_aut %>% 
  rename(
    year = Year,
    id = Kennziffer
  )
popdat_aut <- popdat_aut %>%
  mutate(sex =
    case_when(
      sex == "male" ~ "M",
      sex == "female" ~ "W",
      TRUE ~ NA
    )
  )
popdat_aut$year <- as.numeric(popdat_aut$year)

merge <- dvb_muni_weekly %>% 
  left_join(popdat_aut, by = c("year", "id", "age","sex"))
rm(dvb_muni_weekly, popdat_aut)

save(merge, file = here("Data/merge.RData"))


#rm(merge)

load(here("Data/ZAMG_AUT_muni_weekly_TH_MKabas.RData"))

ZAMG_AUT_muni <- ZAMG_AUT_muni %>% 
  rename(id = ID) %>% 
  mutate(
    year = year(yrweek),
    week = week(yrweek)
  ) 

# from daily temperature to weekly temperature
ZAMG_AUT_muni <- ZAMG_AUT_muni %>% 
  group_by(id, year, week, regname) %>% 
  summarise(
    week_t2m_mean = mean(T2M_mean), # weekly mean of the daily mean temperature
    week_t2m_max = max(T2M_mean), # weekly max of the daily mean temperature
    week_tmax_mean = mean(T2M_max), # weekly mean of daily max temperature
    week_tmax_max = max(T2M_max) # weekly max of daily max temperature
    ) %>% ungroup() |> 
  select(-regname)

#load(here("Data/merge.RData"))


merge <- merge %>% 
  left_join(ZAMG_AUT_muni, by = c("id", "year", "week"))

# Takes up less space
object.size(merge)
merge$pop <- as.integer(merge$pop)
merge$year <- as.integer(merge$year)
merge$n <- as.integer(merge$n)
merge$age <- as.integer(merge$age)
merge$week <- as.integer(merge$week)

object.size(merge)
save(merge, file = here("Data/merge.RData"))

rm(ZAMG_AUT_muni)
gc()

# first rename ID to id, then aggregate by week, then join by year, 
# problem: POSTAL CODE vs OTHER CODE in merge

# Note: the municipality-week analysis panel with all four weekly temperature
# variables (week_t2m_mean, week_t2m_max, week_tmax_mean, week_tmax_max) is
# built separately in Files/Build_Panel.R and saved as Data/panel_weekly.RData.
# Data.Exploration.R uses that panel for all temperature analyses.
