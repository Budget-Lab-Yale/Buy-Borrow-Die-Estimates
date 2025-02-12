#------------------------------------------------------------------------------
# estimation.R
# 
# TODO
#------------------------------------------------------------------------------



estimate_borrowing_model = function() {
  
  #----------------------------------------------------------------------------
  # Using the 2009 SCF panel, estimates new positive borrowing as a function
  # of demographic and economic indicators using regression forests. 
  # Functional form has new borrowing scaled by mean taxable debt at t = 0.  
  # 
  # Parameters: N/A
  #
  # Output: quantile regression forest model object.
  #----------------------------------------------------------------------------
  
  # Use 2009 SCF panel as training data
  train = process_scf_panel() %>% 
    
    # 3x records to reduce randomness
    expand_grid(duplicate_id = 1:3) %>%
    mutate(weight = weight / 3) %>%
    
    mutate(
      
      # Create taxable debt category (assumes primary residence mortgage, 
      # credit card debt, student loans, auto loans, and installment loans don't count)
      taxable_debt_07 = credit_lines_07 + other_debt_07 + other_mortgage_07,
      taxable_debt_09 = credit_lines_09 + other_debt_09 + other_mortgage_09,
      
      # Interpolate change in taxable debt from 2007-2008. Assumes:
      # - alpha = 80% of 2-year extensive margin changes actually happened within one of the two years
      # - beta  = 50% of these changes happened in the first year, 2008 (vs the second year, 2009)
      alpha = 0.8, beta = 0.5,
      extensive_margin = runif(nrow(.)) < alpha, 
      first_year       = runif(nrow(.)) < beta, 
      chg_taxable_debt = case_when(
        
        # No change for those with no debt in either year
        taxable_debt_07 == 0 & taxable_debt_09 == 0 ~ 0, 
        
        # 0 -> positive extensive margin changes 
        taxable_debt_07 == 0 & taxable_debt_09 > 0 ~ case_when(
          first_year & extensive_margin  ~ taxable_debt_09, 
          first_year & !extensive_margin ~ taxable_debt_09 / 2,
          T                              ~ 0
        ),
        
        # Positive -> 0 extensive margin changes
        taxable_debt_07 > 0 & taxable_debt_09 == 0 ~ case_when(
            first_year & extensive_margin  ~ -taxable_debt_07, 
            first_year & !extensive_margin ~ -taxable_debt_07 / 2,
            T                              ~ 0
          ),
        
        # Positive in both years -- interpolate
        taxable_debt_07 > 0 & taxable_debt_09 > 0 ~ (taxable_debt_09 - taxable_debt_07) / 2
      ), 

      # Assign percentiles
      income       = income_07 + if_else(income_07 != 0, runif(nrow(.)), 0), # Add noise to create unique percentile cutoffs 
      assets       = assets_07,
      taxable_debt = taxable_debt_07 + if_else(taxable_debt_07 != 0, runif(nrow(.)), 0), # Add noise to create unique percentile cutoffs ,
      net_worth    = assets_07 - primary_mortgage_07 - other_mortgage_07 - credit_lines_07 - 
                     credit_cards_07 - student_loans_07 - auto_loans_07 - other_debt_07,
      across(
        .cols = c(income, assets, taxable_debt, net_worth), 
        .fns  = ~ cut(
          x      = ., 
          breaks = wtd.quantile(.[. > 0], weight[. > 0], c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.001))), 
          labels = c(seq(0.01, 0.99, 0.01), seq(0.991, 1, 0.001))
        ) %>% as.character() %>% as.numeric() %>% replace_na(0), 
        .names = 'pctile_{col}'
      ), 
      
      # Create other variables
      has_wages    = as.integer(wages_07 > 0),
      has_kids     = as.integer(n_kids > 0),
      taxable_debt = taxable_debt_07,
      norm_postive_borrowing = pmax(0, chg_taxable_debt) / weighted.mean(taxable_debt, weight)
    )
    
  # Estimate model parameters for those with taxable debt in t = 0
  qrf = quantregForest(
    x        = train[c('age', 'has_kids', 'married', 'has_wages',
                       'pctile_income', 'pctile_assets', 'pctile_taxable_debt', 'pctile_net_worth')],
    y        = train$norm_postive_borrowing, 
    nthreads = parallel::detectCores(),
    weights  = train$weight,
    mtry     = 8,
    nodesize = 5
  )

  return(qrf)
}




