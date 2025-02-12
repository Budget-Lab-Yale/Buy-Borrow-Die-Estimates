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
  output_root = './output'
)


# Ten-year budget window 
budget_window = 2026:2035


#-----------------------------
# Calculate revenue estimates
#-----------------------------

source('./src/data.R')
source('./src/estimation.R')
source('./src/sim_option_1.R')

# Read macro projections
macro_projections = read_macro_projections()

# Process 2022 SCF 
augmented_scf = process_scf() %>% 
  
  # Project SCF though 2024
  age_scf_historical(macro_projections) %>% 
  
  # Add billionaires
  add_forbes_data() %>%
  
  # Impute positive net borrowing variable based on 2009 SCF
  impute_borrowing_flows()

# Run simulations
sims = list(
  option_1 = sim_option_1(augmented_scf, macro_projections), 
  option_2 = -1, 
  option_3 = -1
)



#----------------------------------
# Calculate tax rate differentials
#----------------------------------

source('./src/calc_etrs.R')



