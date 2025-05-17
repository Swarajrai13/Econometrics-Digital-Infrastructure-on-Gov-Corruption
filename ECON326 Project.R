# Load essential packages
library(tidyverse)
library(readxl)
library(haven)
install.packages("WDI")
library(WDI)
install.packages("countrycode")
library(countrycode)
install.packages("skimr")
library(skimr)
install.packages("fixest")
library(fixest)




# Read Global Findex (replace with actual file path)
findex <- read_csv("/Users/swaraj/Desktop/Findex_Cleaned copy 2.csv")

# Read CPI Data (Transparency International)
cpi <- read_csv("/Users/swaraj/Downloads/cpi.csv")

# Read WGI data
wgi_raw <- read_excel("/Users/swaraj/Downloads/wgidataset.xlsx", sheet = "Sheet1")

#World Development Indicators (WDI) (can be pulled from API)
wdi_vars <- c("NY.GDP.PCAP.PP.CD", "SP.URB.TOTL.IN.ZS")
wdi_data <- WDI(country = "all", indicator = wdi_vars, start = 2011, end = 2021)

wdi_clean <- wdi_data %>%
  rename(
    iso3 = iso2c,
    gdp_per_capita = NY.GDP.PCAP.PP.CD,
    urban_pop_pct = SP.URB.TOTL.IN.ZS
  ) %>%
  mutate(iso3 = countrycode(iso3, origin = "iso2c", destination = "iso3c")) %>%
  select(iso3, year, gdp_per_capita, urban_pop_pct)





# Standardize country names and years in Findex
findex_clean <- findex %>%
  rename(
    country = `Country name`,
    iso3 = `Country code`,
    year = Year,
    digital_payments = `Account (% age 15+)`  # Replace if you have a more precise digital payment variable
  ) %>%
  select(country, iso3, year, digital_payments) %>%
  filter(!is.na(digital_payments))

# Standardize CPI
cpi_long <- cpi %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "cpi_score"
  ) %>%
  rename(
    country = `Country / Territory`,
    iso3 = ISO3
  ) %>%
  mutate(
    year = as.integer(year),
    country = str_trim(country),
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 2011 & year <= 2021)

# Pivot to wide format: each row = country-year, each column = governance indicator
wgi_clean <- read_excel("/Users/swaraj/Downloads/wgidataset.xlsx", sheet = "Sheet1") %>%
  select(country = countryname, iso3 = code, year, indicator, estimate) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  rename(
    control_of_corruption = cc,
    government_effectiveness = ge,
    political_stability = pv,
    rule_of_law = rl,
    regulatory_quality = rq,
    voice_and_accountability = va
  ) %>%
  filter(year >= 2011 & year <= 2021)

# Check cleaned result
glimpse(wgi_clean)




# === Merge Findex and CPI first ===
panel_data <- findex_clean %>%
  inner_join(cpi_long, by = c("iso3", "year"))

# === Then merge WGI ===
panel_data <- panel_data %>%
  inner_join(wgi_clean, by = c("iso3", "year"))

# Merge in WDI indicators (GDP + Urbanization)
panel_data <- panel_data %>%
  inner_join(wdi_clean, by = c("iso3", "year"))

# Reorder for clarity
panel_data <- panel_data %>%
  select(country, iso3, year, digital_payments, cpi_score,
         gdp_per_capita, urban_pop_pct,
         rule_of_law, government_effectiveness, political_stability,
         regulatory_quality, voice_and_accountability)

# Check merged data summary
summary(panel_data)




# Summary Stats
summary_stats <- panel_data %>%
  summarise(
    n_obs = n(),
    digital_mean = mean(digital_payments, na.rm = TRUE),
    digital_sd = sd(digital_payments, na.rm = TRUE),
    digital_min = min(digital_payments, na.rm = TRUE),
    digital_max = max(digital_payments, na.rm = TRUE),
    
    cpi_mean = mean(cpi_score, na.rm = TRUE),
    gdp_mean = mean(gdp_per_capita, na.rm = TRUE)
  )

print(summary_stats)
glimpse(panel_data)

model1 <- feols(
  cpi_score ~ digital_payments * rule_of_law +
    gdp_per_capita + urban_pop_pct,
  data = panel_data,
  fixef = c("iso3", "year"),      # Corrected
  cluster = "iso3"
)

# View output
summary(model1) #Fixed Effects Already Absorb Most Variation (Collinearity)
etable(model1)






