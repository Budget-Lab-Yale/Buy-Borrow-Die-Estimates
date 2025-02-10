#------------------------------------------------------------------------------
# main.R
# 
# Entry point into calculations 
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)


#---------------
# Configuration
#---------------

# Set data dependency file paths
file_paths = list(
  
  # SCF data
  scf_2022       = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/2022/historical', 
  scf_2009_panel = '/gpfs/gibbs/project/sarin/shared/raw_data/SCF/v1/2009/historical',
  
  # TODO other data...
  
  # Root for output files 
  output_root = './output'
)


# Ten-year budget window 
budget_window = 2026:2035


#-----------------------------
# Calculate revenue estimates
#-----------------------------

# 1) read and process macro projections (also get from wealth-tax-simulator)

# 2) read, process, and age 2022 SCF through end of budget window (mostly copy wealth-tax-simulator here)

# 3) read 2009 SCF, estimate model of annual borrowing, and impute   

# 4) calculate tax liability under each reform option
#     - option (1) is easy (cross-sectional joint function of borrowing and gains)
#     - option (2) is tougher -- have to model probability of sale death over time....
#     - option (3) might not be SCF-based at all -- could just do aggregate data 
#   - all of these have to account for avoidance

# 5) calculate budget totals and maybe distribution summary tables? 


#----------------------------------
# Calculate tax rate differentials
#----------------------------------

# 1) write functions for all the formulas

# 2) choose baseline and sensitivity parameter values

# 3) execute functions and 

