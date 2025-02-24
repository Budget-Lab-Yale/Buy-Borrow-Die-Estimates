#------------------------------------------------------------------------------
# main.R
# 
# Entry point into calculations.
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(quantregForest)


#---------------
# Configuration
#---------------

# Random seed
set.seed(1)

# Set data dependency file paths
file_paths = list(
  
  # SCF data
  scf_2022       = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/2022/historical', 
  scf_2009_panel = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/2009/historical',
  
  # Macro projections file
  macro_projections = '/gpfs/gibbs/project/sarin/shared/model_data/Macro-Projections/v3/2024111415/baseline',
  
  # Root for output files 
  output_root = './output', 
  
  # Root for cached borrowing imputation values 
  cache_root = './cache'
)

# Whether to load cached borrowing imputation values or re-estimate
load_chached_imputation = T

# Years for simulation 
years = 2026:2055


#-----------------------------
# Calculate revenue estimates
#-----------------------------

source('./src/data.R')
source('./src/estimation.R')
source('./src/sim_option_1.R')
source('./src/sim_option_2.R')
source('./src/sim_option_3.R')


# Read macro projections
macro_projections = read_macro_projections()


# Process 2022 SCF 
augmented_scf = process_scf() %>% 
  
  # Project SCF though 2024
  age_scf_historical(macro_projections) %>% 
  
  # Add billionaires
  add_forbes_data() %>%
  
  # Impute positive net borrowing variable based on 2009 SCF
  impute_borrowing_flows(load_chached_imputation) %>% 
  
  # Impute life expectancy
  impute_expected_death_age()


# Run simulations
sims = list(
  option_1 = sim_option_1(augmented_scf, macro_projections), 
  option_2 = sim_option_2(augmented_scf, macro_projections), 
  option_3 = sim_option_3(augmented_scf, macro_projections)
)


# Build combined distribution tables
build_dist_tables(c('income', 'age', 'wealth'), c('option_1', 'option_2', 'option_3'), 2026)

sims[1:3] %>% 
  map(
    .f = ~ .x %>% 
      filter(year <= 2035) %>% 
      group_by(static) %>% 
      summarise(sum(net_revenue))
  )


# Build Revenue Tables
1:3 %>%
  map(
    .f = ~ sims[.x] %>%
      map_dfr(., ~as_tibble(.x)) %>%
      select(static, year, net_revenue) %>%
      pivot_wider(names_from = year, values_from = net_revenue) %>%
      filter(!static) %>%
      mutate(
        `2025` = 0
      ) %>%
      select(static, `2025`, everything()) %>%
      pivot_longer(
        !static,
        names_to = "year",
        values_to = "val"
      )  %>%
      mutate(
        fy_val = case_when(
          year == 2025 ~ 0,
          .x   != 3    ~ lag(val),
          T            ~ .75 * val + .25 * lag(val)
        )
      ) %>%
      select(!val) %>%
      pivot_wider(names_from = year, values_from = fy_val) %>%
      mutate(
        scenario = paste0("Option ", .x),
        budget_window = `2026` + `2027` + `2028` + `2029` + `2030` + `2031` + `2032` 
        + `2033` + `2034` + `2035`,
        second_decade = `2036` + `2037` + `2038` + `2039` + `2040` + `2041` + `2042` 
        + `2043` + `2044` + `2045`,
        third_decade  = `2046` + `2047` + `2048` + `2049` + `2050` + `2051` + `2052` 
        + `2053` + `2054` + `2055`
      ) %>%
      select(scenario, `2026`, `2027`, `2028`, `2029`, `2030`, `2031`, `2032`,
             `2033`, `2034`, `2035`,
             budget_window, second_decade, third_decade)
  ) %>%
  bind_rows() %>%
  pivot_longer(
    !scenario,
    names_to = 'year',
    values_to = 'net_revenue'
  ) %>%
  left_join(
    macro_projections$economic %>%
      filter(between(year, 2026, 2055)) %>%
      select(year, gdp) %>%
      mutate(
        year = as.character(year)
      ) %>%
      pivot_wider(names_from = year, values_from = gdp) %>%
      mutate(
        hold = 1,
        budget_window = `2026` + `2027` + `2028` + `2029` + `2030` + `2031` + `2032` 
        + `2033` + `2034` + `2035`,
        second_decade = `2036` + `2037` + `2038` + `2039` + `2040` + `2041` + `2042` 
        + `2043` + `2044` + `2045`,
        third_decade = `2046` + `2047` + `2048` + `2049` + `2050` + `2051` + `2052` 
        + `2053` + `2054` + `2055`
      )%>%
      select(hold, `2026`, `2027`, `2028`, `2029`, `2030`, `2031`, `2032`,
             `2033`, `2034`, `2035`,
             budget_window, second_decade, third_decade) %>%
      pivot_longer(!hold, names_to = "year", values_to = "gdp") %>%
      select(!hold)
  ) %>%
  mutate(
    share_of_gdp = net_revenue / gdp
  ) %>%
  select(!gdp) %>%
  pivot_wider(names_from = year, values_from = c(net_revenue, share_of_gdp)) %>%
  select(
    scenario, starts_with("net_revenue"),
    share_of_gdp_budget_window, share_of_gdp_second_decade, share_of_gdp_third_decade
  ) %>%
  rename_with(
    ~ str_sub(.x, 13, -1),
    starts_with("net_revenue")
  ) %>%
  select(!c(second_decade, third_decade))


#------------------------------------
# Calculate tax rate differentials
#------------------------------------

source('./src/etrs.R')

#------------------------------------

#------------------------------------

