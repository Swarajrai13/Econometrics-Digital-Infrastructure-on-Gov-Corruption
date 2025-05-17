setwd("~/Desktop/ECON326 FP Data")
# -----------------------------------------------
# 1.  Setup
# -----------------------------------------------
install.packages("fixest")
library(tidyverse)   # for data‐wrangling
library(fixest)      # for fast FE regressions
install.packages("stargazer")
library(stargazer)   # for nicely formatted tables
library(readxl)
library(countrycode)
library(readr)
library(dplyr)
library(tidyr)
install.packages("modelsummary")
library(modelsummary)
library(ggplot2)
library(patchwork)

# -----------------------------------------------
# 2.  Load your panel data
# -----------------------------------------------

# (Assumes you have a CSV with columns: Country, Year, CPI, DigitalPayments,
#  InformalRate, UrbanShare, InternetPenetration, etc.)
df_cpi <- read_csv("final_ECON326_merged_panel_cpi.csv")
df_wgi <- read_csv("final_ECON326_merged_panel_wgi.csv")
df_wgi <- df_wgi %>%
  mutate(
    corruption_control = as.numeric(corruption_control),
    mobile_subs = as.numeric(mobile_subs)
  )


# peek at it
glimpse(df_cpi)
glimpse(df_wgi)

summary(df_cpi)
summary(df_wgi)

# -----------------------------------------------
# 3. Summary statistics table for key vars
# -----------------------------------------------

# Keep only numeric variables for summary
summary_table <- df_wgi %>%
  select(where(is.numeric)) %>%
  summarise(across(
    everything(),
    list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ),
    .names = "{.fn}_{.col}"
  )) %>%
  pivot_longer(cols = everything(),
               names_to = c("Stat", "Variable"),
               names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  relocate(Variable) %>%
  arrange(Variable) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# View the table
print(summary_table)



# -----------------------------------------------
# 4. Baseline OLS: Corruption on Mobile Subscriptions
# -----------------------------------------------
m1 <- feols(cpi_score ~ mobile_subs, data = df_cpi)
summary(m1)

# -----------------------------------------------
# 5. Country & Year Fixed Effects
# -----------------------------------------------
m2 <- feols(cpi_score ~ mobile_subs | iso3 + year, data = df_cpi)
summary(m2)

# -----------------------------------------------
# 6. Mechanism Model
# -----------------------------------------------
m3 <- feols(
  cpi_score ~ mobile_subs + urban_pct + working_age_pct + wbl_index | iso3 + year,
  data = df_cpi
)
summary(m3)

# -----------------------------------------------
# 7. Add Additional Mechanisms (Working Age Pop, WBL Index)
# -----------------------------------------------
m4 <- feols(
  cpi_score ~ mobile_subs
  + urban_pct
  + working_age_pct
  + wbl_index
  + mobile_subs:urban_pct
  + mobile_subs:working_age_pct
  + mobile_subs:wbl_index
  | iso3 + year,
  data = df_cpi
)
summary(m4)

# -----------------------------------------------
# 8. Robustness Check: Region FE + Year
# -----------------------------------------------

# Region + Year fixed effects using CPIA as DV
m5 <- feols(
  cpi_score ~ mobile_subs | region + year,
  data = df_cpi
)

summary(m5)

#Multivariate Region FE:
m5a <- feols(cpi_score ~ mobile_subs + urban_pct + working_age_pct + wbl_index | region + year, data = df_cpi)
summary(m5a)

# -----------------------------------------------
# 9. Compare Models Side-by-Side
# -----------------------------------------------
modelsummary(
  list("OLS" = m1, "FE" = m2, "FE + Mechanism" = m3, "FE + Interactions" = m4, "Region + Year FE" = m5, "Region & Year FE + Mechanism" =m5a ),
  statistic = "std.error",
  stars = TRUE,
  title = "Effect of Mobile Subscriptions on Corruption (CPIA)"
)

# -----------------------------------------------
# 10. Robustness Check -> Alternate DV Specification (WB Control Of Corruption)
# -----------------------------------------------
# Baseline OLS
m1_wgi <- feols(corruption_control ~ mobile_subs, data = df_wgi)
summary(m1_wgi)

# Country + Year Fixed Effects
m2_wgi <- feols(corruption_control ~ mobile_subs | iso3 + year, data = df_wgi)
summary(m2_wgi)

# Mechanism Model
m3_wgi <- feols(
  corruption_control ~ mobile_subs + urban_pct + working_age_pct + wbl_index | iso3 + year,
  data = df_wgi
)
summary(m3_wgi)

# Full Mechanism Model
m4_wgi <- feols(
  corruption_control ~ mobile_subs
  + urban_pct
  + working_age_pct
  + wbl_index
  + mobile_subs:urban_pct
  + mobile_subs:working_age_pct
  + mobile_subs:wbl_index
  | iso3 + year,
  data = df_wgi
)
summary(m4_wgi)

# Region + Year FE
m5_wgi <- feols(
  corruption_control ~ mobile_subs, 
  data = df_wgi,
  fixef = c("region", "year"),
  cluster = ~ iso3  # still cluster by country
)
summary(m5_wgi)

# Region + Year FE: with Mechanism 
m6_wgi<- feols(
  corruption_control ~ mobile_subs 
  + urban_pct
  + working_age_pct
  + wbl_index,
  data = df_wgi,
  fixef = c("region", "year"),
  cluster = ~ iso3
)
summary(m6_wgi)


modelsummary(
  list("wgi_OLS" = m1_wgi, "wgi_FE" = m2_wgi, "wgi_FE + Mechanism" = m3_wgi, "wgi_FE + Interactions" = m4_wgi, "wgi_Region + Year FE" = m5_wgi, "wgi_Region + Year FE: Mechanisms" = m6_wgi
),
  statistic = "std.error",
  stars = TRUE,
  title = "Effect of Mobile Subscriptions on Control of Corruption (WGI)"
)


# -----------------------------------------------
# 11. Basic Plots
# -----------------------------------------------

# Plot 1: Histogram for each variable (including WGI)
df_wgi <- df_wgi %>%
  mutate(log_mobile_subs = log1p(mobile_subs))  # log1p handles 0s safely
df_wgi %>%
  mutate(`Mobile Subscriptions (/100 people)` = log1p(mobile_subs)) %>%
  select(
    `Corruption (CPIA)` = cpi_score,
    `Corruption (WGI)` = corruption_control,
    `Mobile Subscriptions (/100 people)`,
    `Urban Population (%)` = urban_pct,
    `Working-Age Population (%)` = working_age_pct,
    `WBL Index` = wbl_index
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~Variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Distribution of Key Variables",
    x = NULL,
    y = "Count",
    caption = "Note: Mobile Subscriptions are log-transformed (log1p) to reduce skew"
  )


# Plot 2: Mobile Subscriptions vs. Corruption (CPIA)
ggplot(df_cpi, aes(x = mobile_subs, y = cpi_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(title = "Mobile Subscriptions vs. Corruption (CPIA)",
       x = "Mobile Subscriptions (/100 people)",
       y = "CPIA Corruption Score")

# Plot 3: Mobile Subscriptions vs. Corruption (WGI)
ggplot(df_wgi, aes(x = mobile_subs, y = corruption_control)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(title = "Mobile Subscriptions vs. Corruption (WGI)",
       x = "Mobile Subscriptions (/100 people)",
       y = "WGI Corruption Score")


# Plot 4: Time Trend Plots
# Grouped time trend data
time_trends <- df_wgi %>%
  group_by(year) %>%
  summarise(
    `Mobile Subscriptions (/100 people)` = mean(mobile_subs, na.rm = TRUE),
    `Corruption (CPIA)` = mean(cpi_score, na.rm = TRUE),
    `Corruption (WGI)` = mean(corruption_control, na.rm = TRUE),
    `Urban Population (%)` = mean(urban_pct, na.rm = TRUE),
    `Working-Age Population (%)` = mean(working_age_pct, na.rm = TRUE),
    `WBL Index` = mean(wbl_index, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -year, names_to = "Variable", values_to = "Mean")

time_trends <- time_trends %>%
  filter(year >= 1995)


# Create list of plots
plot_list <- time_trends %>%
  group_split(Variable) %>%
  lapply(function(df) {
    ggplot(df, aes(x = year, y = Mean)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "darkblue", size = 2) +
      theme_minimal() +
      labs(title = df$Variable[1], x = "Year", y = "Mean")
  })

# Combine using patchwork
wrap_plots(plotlist = plot_list, ncol = 2)

