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
set.seed(76)

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

# Read macro projections
macro_projections = read_macro_projections()

# Process 2022 SCF 
augmented_scf = process_scf() %>% 
  
  # Project SCF though 2024
  age_scf_historical(macro_projections) %>% 
  
  # Add billionaires
  add_forbes_data() %>%
  
  # Impute net new borrowing based on 2009 SCF
  impute_borrowing_flows()




# 4) calculate tax liability under each reform option
#     - option (1) is easy (cross-sectional joint function of borrowing and gains)
#     - option (2) is tougher -- have to model probability of sale death over time....
#     - option (3) might not be SCF-based at all -- could just do aggregate data 
#   - all of these have to account for avoidance

# 5) calculate budget totals and maybe distribution summary tables? 


#----------------------------------
# Calculate tax rate differentials
#----------------------------------

source('./src/calc_etrs.R')

# 1) write functions for all the formulas

# 2) choose baseline and sensitivity parameter values

# 3) execute functions and produce tables

