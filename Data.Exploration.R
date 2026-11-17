# =====================================================================
# Master thesis: weekly temperature and hospitalisations in Austria
# Data exploration, Granger causality and fixed-effects count models
# =====================================================================
# Inputs
#   Data/merge.RData         hospital discharges
#                            (municipality x ISO week x age x sex)
#   Data/panel_weekly.RData  municipality x week panel with the four weekly
#                            temperature variables, built once by
#                            Files/Build_Panel.R from the daily ZAMG data
#
# Outputs
#   Output/Figures/Fig1_distribution_n.png
#   Output/Figures/Fig2_distribution_temperature.png
#   Output/Figures/Fig3_n_and_temperature.png
#   Output/Figures/Fig4_granger_causality.png
#   Output/Figures/Fig5_fe_regression.png
#   Output/Tables/eda_summary.csv
#   Output/Tables/granger_panel.csv
#   Output/Tables/granger_national.csv
#   Output/Tables/model_comparison.csv
#
# Note: n is stored as a fractional count in the source data (post-processing
# weights). Values are summed to municipality-week totals; Poisson/NB
# likelihoods are quasi-likelihoods for non-integer responses, which is
# unproblematic for estimation and inference.
# =====================================================================

rm(list = ls())

library(tidyverse)
library(here)
library(tsibble)
library(data.table)
library(plm)
library(fixest)
library(lmtest)
library(patchwork)
library(scales)

load(here("Data", "merge.RData"))          # merge: cell-level discharges

dir.create(here("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Tables"), recursive = TRUE, showWarnings = FALSE)

merge <- as.data.table(merge)
merge[, `:=`(
  date     = as.Date(yrweek),
  yrweek_f = factor(format(yrweek)),
  week_f   = factor(week),
  t        = as.integer(factor(format(yrweek)))
)]

temp_vars <- c("week_t2m_mean", "week_t2m_max", "week_tmax_mean", "week_tmax_max")
temp_labs <- c(
  week_t2m_mean  = "Mean of daily mean T",
  week_t2m_max   = "Max of daily mean T",
  week_tmax_mean = "Mean of daily max T",
  week_tmax_max  = "Max of daily max T"
)
temp_cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")


max(merge$t)
# =====================================================================
# 1. Original exploratory checks (kept from the working script)
# =====================================================================
# The original script took the temperature columns from `merge`, but
# Data/merge.RData only stores the hospitalisation columns. All temperature
# work therefore uses `merge_weekly`, where the four weekly variables are
# computed from the daily ZAMG data.

# Hospitalisations by age group, selected years
merge |>
  filter(year == 2009, sex == "M") |>
  select(week, n, age) |>
  group_by(week, age) |>
  summarise(n = sum(n), .groups = "drop") |>
  ggplot(aes(x = week, y = n, group = age, color = age)) +
  geom_line()

merge |>
  filter(year == 2018, sex == "M") |>
  select(week, n, age) |>
  group_by(week, age) |>
  summarise(n = sum(n), .groups = "drop") |>
  ggplot(aes(x = week, y = n, group = age, color = age)) +
  geom_line()

# National weekly series (men)
merge |>
  filter(sex == "M") |>
  select(yrweek, n) |>
  group_by(yrweek) |>
  summarise(n = sum(n), .groups = "drop") |>
  ggplot(aes(x = yrweek, y = n)) +
  geom_line()

# Weekly maximum temperature by year
merge |>
  group_by(year, week) |>
  summarise(week_tmax_max = mean(week_tmax_max), .groups = "drop") |>
  ggplot(aes(x = week, y = week_tmax_max, group = year, color = year)) +
  geom_line()

# IQR outliers per municipality
IQR_outlier <- function(data, variable) {
  data |>
    group_by(id) |>
    mutate(
      !!paste0("IQR.", variable) := case_when(
        .data[[variable]] > quantile(.data[[variable]], 0.75, na.rm = TRUE) +
          1.5 * IQR(.data[[variable]], na.rm = TRUE) ~ "High",
        .data[[variable]] < quantile(.data[[variable]], 0.25, na.rm = TRUE) -
          1.5 * IQR(.data[[variable]], na.rm = TRUE) ~ "Low",
        TRUE ~ "Normal"
      )
    ) |>
    ungroup()
}

merge_iqr <- as_tibble(merge)
for (v in c("n", temp_vars)) {
  merge_iqr <- IQR_outlier(merge_iqr, v)
}

outlier_share <- sapply(c("n", temp_vars), function(v) {
  mean(merge_iqr[[paste0("IQR.", v)]] != "Normal")
})
print(round(100 * outlier_share, 1))
rm(merge_iqr)

# =====================================================================
# 2. Exploratory data analysis: three figures
# =====================================================================

n_mean   <- mean(merge$n)
n_median <- median(merge$n)
n_var    <- var(merge$n)
n_zero   <- mean(merge$n == 0)
n_q99    <- quantile(merge$n, 0.99)

eda_summary <- data.table(
  variable = c("n", temp_vars),
  mean     = c(mean(merge$n), sapply(merge[, ..temp_vars], mean)),
  sd       = c(sd(merge$n),   sapply(merge[, ..temp_vars], sd)),
  median   = c(median(merge$n), sapply(merge[, ..temp_vars], median)),
  min      = c(min(merge$n),  sapply(merge[, ..temp_vars], min)),
  max      = c(max(merge$n),  sapply(merge[, ..temp_vars], max)),
  zero_share = c(n_zero, rep(NA_real_, length(temp_vars)))
)
fwrite(eda_summary, here("Output", "Tables", "eda_summary.csv"))

# Figure 1: distribution of n ------------------------------------------------
fig1 <- ggplot(merge, aes(x = n)) +
  geom_histogram(bins = 100, fill = "#2c7fb8", colour = "white", linewidth = 0.1) +
  scale_y_log10(labels = label_comma()) +
  labs(
    title = "Distribution of weekly hospitalisations per municipality",
    subtitle = sprintf(
      "Mean = %.1f, median = %.0f, variance = %s, zeros = %.1f%%, 99%% <= %.0f (y-axis log scale)",
      n_mean, n_median, comma(n_var), 100 * n_zero, n_q99
    ),
    x = "Hospitalisations per municipality-week (n)",
    y = "Municipality-weeks (log scale)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")

ggsave(here("Output", "Figures", "Fig1_distribution_n.png"),
       fig1, width = 8, height = 5, dpi = 300)

# Figure 2: distribution of weekly mean temperature --------------------------
t_mean <- mean(merge$week_t2m_mean)
t_sd   <- sd(merge$week_t2m_mean)
t_med  <- median(merge$week_t2m_mean)

fig2 <- ggplot(merge, aes(x = week_t2m_mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "#de2d26", colour = "white", linewidth = 0.1) +
  geom_density(colour = "black", linewidth = 0.7) +
  geom_vline(xintercept = t_mean, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = t_med,  linetype = "dotted", linewidth = 0.6) +
  labs(
    title = "Distribution of weekly mean temperature",
    subtitle = sprintf("Mean = %.1f deg C, median = %.1f deg C, SD = %.1f deg C (dashed = mean, dotted = median)",
                       t_mean, t_med, t_sd),
    x = "Weekly mean of daily mean temperature (deg C)",
    y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")

ggsave(here("Output", "Figures", "Fig2_distribution_temperature.png"),
       fig2, width = 8, height = 5, dpi = 300)

# Figure 3: how n and temperature run together -------------------------------
nat <- merge[, .(
  n    = sum(n),
  temp = mean(week_t2m_mean)
), by = .(yrweek, date, year, week)]
setorder(nat, date)

# Temperature and hospitalisations are kept on their natural scales in the
# stacked panels; no dual axis or rescaling is needed.
fig3_temp <- ggplot(nat, aes(x = date, y = temp)) +
  geom_line(colour = "#d62728", linewidth = 0.45) +
  labs(x = NULL, y = "deg C", title = "(a) National weekly mean temperature") +
  theme_minimal(base_size = 10)

fig3_n <- ggplot(nat, aes(x = date, y = n)) +
  geom_line(colour = "#1f77b4", linewidth = 0.45) +
  labs(x = NULL, y = "Hospitalisations per week",
       title = "(b) National weekly hospitalisations") +
  theme_minimal(base_size = 10)

fig3_scatter <- ggplot(nat, aes(x = temp, y = n, colour = week)) +
  geom_point(alpha = 0.65, size = 1.5) +
  geom_smooth(method = "loess", span = 0.5, colour = "black",
              se = FALSE, linewidth = 0.7) +
  scale_colour_viridis_c(name = "Week of year") +
  labs(
    x = "Weekly mean temperature (deg C)",
    y = "Hospitalisations per week",
    title = "(c) Weekly association; colour identifies the week of year"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right")

fig3 <- ((fig3_temp / fig3_n) | fig3_scatter) +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = sprintf("Hospitalisations and temperature across %d weeks", nrow(nat)),
    subtitle = "Panels (a) and (b) keep the natural scales of each series; panel (c) pools the same weeks.",
    theme = theme(plot.title = element_text(face = "bold"))
  )

ggsave(here("Output", "Figures", "Fig3_n_and_temperature.png"),
       fig3, width = 10, height = 7, dpi = 300)


library(fixest)
library(tidyverse)

summary(merge)

merge.complete <- merge %>%
  filter(complete.cases(.))


merge.complete |> filter(age %in% c(5, 65, 80)) |> 
  group_by(age, yrweek_f) |> 
  summarise(n = mean(n), .groups = "drop") |> 
  ggplot(aes(x = yrweek_f, y = n, group = factor(age), color = factor(age))) +
  geom_line() +
  scale_x_discrete(breaks = \(x) x[seq(1, length(x), by = 10)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



rates <- merge.complete |> filter(age %in% c(5, 65, 80)) |> 
  select(c(age, n, pop, yrweek_f)) |> 
  group_by(age, yrweek_f) |> 
  summarise(rate = mean(n)/mean(pop), .groups = "drop") 



  ggplot(data = rates, aes(x = yrweek_f, y = rate, group = factor(age), color = factor(age))) +
  geom_line() +
  scale_x_discrete(breaks = \(x) x[seq(1, length(x), by = 10)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  merge.complete<- merge.complete |> 
  group_by(id) |> mutate(avg_temp = mean(week_t2m_mean, na.rm = TRUE)) |> ungroup()

summary(model.1)

pearson_chisq <- sum(residuals(model.1, type = "pearson")^2)

df_resid <- nobs(model.1) - model.1$nparams

dispersion <- pearson_chisq / df_resid

dispersion

merge.complete_filtered <- merge.complete |> filter(week_f !=53)

summary(model.2 <- fepois(
  n ~ week_t2m_mean * age + I(age^2) | id + yrweek_f,
  offset = ~log(pop),
  data = merge.complete,
  vcov = ~id + yrweek_f 
))

summary(model.3 <- fepois(
  n ~ week_t2m_mean * (I(age^2) + age)| id + yrweek_f,
  offset = ~log(pop),
  data = merge.complete,
  vcov = ~id + yrweek_f 
))


lrtest(model.2, model.3)

wald(model.3, "week_t2m_mean:age")

# Extract fitted values and Pearson residuals
fitted_values <- predict(model.3, type = "response")
pearson_resid <- residuals(model.3, type = "pearson")

set.seed(123)

idx <- sample(
  seq_along(fitted_values),
  min(50000, length(fitted_values))
)

plot(
  fitted_values[idx],
  pearson_resid[idx],
  pch = 20,
  cex = 0.4,
  col = rgb(0, 0, 0, 0.15),
  xlab = "Fitted values",
  ylab = "Pearson residuals",
  main = "Residuals vs Fitted"
)

abline(h = 0, col = "red", lwd = 2)

lines(
  lowess(fitted_values[idx], pearson_resid[idx]),
  col = "blue",
  lwd = 2
)


r <- residuals(model.3, type = "pearson")
mu <- fitted(model.3)

idx <- order(r, decreasing = TRUE)[1:30]

diagnostics <- data.frame(
  observed = merge.complete$n[idx],
  predicted = mu[idx],
  pearson_residual = r[idx],
  population = merge.complete$pop[idx],
  age = merge.complete$age[idx],
  id = merge.complete$id[idx],
  week = merge.complete$yrweek_f[idx]
)

diagnostics


library(ggplot2)

b <- coef(model.3)

ages <- seq(from = 0, to = 95, by =5)


plot_data <- data.frame(
  age = ages,
  effect =
    b["week_t2m_mean"] +
    b["week_t2m_mean:age"] * ages +
    b["week_t2m_mean:I(I(age^2))"] * ages^2
)

ggplot(plot_data, aes(x = age, y = effect)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Age",
    y = "Marginal effect of temperature on log hospitalisation rate"
  ) +
  theme_minimal()

# Pearson dispersion statistic
pearson_chisq <- sum(residuals(model.2, type = "pearson")^2)

df_resid <- nobs(model.2) - model.2$nparams

dispersion <- pearson_chisq / df_resid

dispersion # underdispersin!



# Observations retained by the model
idx <- fixest::obs(model.3)

# Align explanatory variable and residuals
temp <- merge.complete$week_t2m_mean[idx]
res <- residuals(model.3, type = "pearson")

# Check lengths
length(temp)
length(res)
set.seed(123)

sample_idx <- sample(
  seq_along(res),
  min(50000, length(res))
)

plot(
  temp[sample_idx],
  res[sample_idx],
  pch = 20,
  cex = 1,
  col = rgb(0, 0, 0, 0.15),
  xlab = "Weekly mean temperature",
  ylab = "Pearson residuals",
  main = "Residuals vs Temperature"
)

abline(h = 0, col = "red", lwd = 2)

lines(
  lowess(temp[sample_idx], res[sample_idx]),
  col = "blue",
  lwd = 2
)
