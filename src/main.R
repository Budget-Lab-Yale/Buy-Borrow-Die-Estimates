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

sims[1:3] %>% 
  map(
    .f = ~ .x %>% 
      filter(year <= 2035) %>% 
      group_by(static) %>% 
      summarise(sum(net_revenue))
  )


#------------------------------------
# Calculate tax rate differentials
#------------------------------------

source('./src/etrs.R')

#------------------------------------
# Build combined distribution tables
#------------------------------------

build_dist_tables(c("income", "age", "wealth"), c("option_1", "option_2", "option_3"), 2026)

