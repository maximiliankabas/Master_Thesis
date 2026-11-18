rm(list = ls())

library(tidyverse)
library(here)
load(here("Data", "merge.RData"))          # merge: cell-level discharges))
          

infants <- merge |> filter(age %in% c(0)) |> 
  group_by(yrweek, age) |> 
  summarise(n = sum(n, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
            week_t2m_mean = mean(week_t2m_mean, na.rm = TRUE))


births <- read.csv("/Users/maximiliankabas/Desktop/STFF_countAUT.csv")
rm(merge)

library(dplyr)
library(tidyr)
library(lubridate)

infants_monthly <- infants %>%
  mutate(
    date = as.Date(yrweek),
    Year = lubridate::year(date),
    Month = lubridate::month(date, label = TRUE, abbr = FALSE)
  ) %>%
  group_by(Year, Month) %>%
  summarise(n = sum(n, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
    week_t2m_mean = mean(week_t2m_mean, na.rm = TRUE),
    .groups = "drop"
  )



births_monthly <- births %>%
  pivot_longer(
    cols = January:December,
    names_to = "Month",
    values_to = "births"
  ) %>%
  mutate(
    Month = factor(
      Month,
      levels = month.name,
      ordered = TRUE
    )
  )

merge.monthly <- births_monthly %>%
  left_join(
    infants_monthly,
    by = c("Year", "Month")
  ) |> select(-c(X, CountryCode, Area)) |> 
  filter(Year %in% seq(from = 2012, to = 2018))



plot(merge.monthly$n ~ merge.monthly$Month)
plot(merge.monthly$births ~ merge.monthly$Month)
plot(merge.monthly$week_t2m_mean ~ merge.monthly$Month)


library(dplyr)
library(ggplot2)
library(lubridate)

# Create monthly date
merge.monthly <- merge.monthly %>%
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", Year, Month))
  ) %>%
  arrange(date)

# 1. Weekly/monthly count n
p1 <- ggplot(merge.monthly, aes(x = date, y = n)) +
  geom_line() +
  labs(
    title = "Monthly observations (n)",
    x = "Year",
    y = "n"
  ) +
  theme_minimal()

# 2. Births
p2 <- ggplot(merge.monthly, aes(x = date, y = births)) +
  geom_line() +
  labs(
    title = "Monthly births",
    x = "Year",
    y = "Births"
  ) +
  theme_minimal()
# 3. Mean temperature
p3 <- ggplot(merge.monthly, aes(x = date, y = week_t2m_mean)) +
  geom_line() +
  labs(
    title = "Monthly mean temperature",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal()


# Dates for August in every year
august_dates <- as.Date(paste0(
  2012:2018, "-08-01"
))

p1 <- p1 +
  geom_vline(
    xintercept = august_dates,
    linetype = "dashed",
    color = "red",
    alpha = 0.6
  )

p2 <- p2 +
  geom_vline(
    xintercept = august_dates,
    linetype = "dashed",
    color = "red",
    alpha = 0.6
  )

p3 <- p3 +
  geom_vline(
    xintercept = august_dates,
    linetype = "dashed",
    color = "red",
    alpha = 0.6
  )

p1
p2
p3

# Display separately
library(patchwork)

p1 / p2 / p3


