#------------------------------------------------------------------------------
# estimation.R
# 
# TODO
#------------------------------------------------------------------------------



estimate_borrowing_model = function(augmented_scf) {
  
  #----------------------------------------------------------------------------
  # Using the 2009 SCF panel, estimates a quantile regression forest model of
  # net borrowing flows as a function of demographic and economic variables. 
  # 
  # Parameters: N/A
  #
  # Output: list of model objects -- one for those with debt at t = 0, one 
  #         for those without (list).
  #----------------------------------------------------------------------------
  
  # Use 2009 SCF panel as training data
  train = process_scf_panel() %>% 
    mutate(
      
      # Create taxable debt category (assumes primary residence mortgage, 
      # credit card debt, student loans, and auto loans don't count)
      taxable_debt_07 = credit_lines_07 + other_debt_07 + other_mortgage_07,
      taxable_debt_09 = credit_lines_09 + other_debt_09 + other_mortgage_09,
      
      # Interpolate for 2008 (TODO: something better?)
      taxable_debt_08 = (taxable_debt_09 + taxable_debt_07) / 2,
      
      # Calculate change (y variable) 
      taxable_debt_pct_chg = taxable_debt_08 / taxable_debt_07 - 1,
      
      # Assign percentiles
      income    = income_07 + if_else(income_07 != 0, runif(nrow(.)), 0), # Add noise to create unique percentile cutoffs 
      net_worth = cash_07 + equities_07 + bonds_07 + retirement_07 + life_ins_07 + 
                  annuities_07 + trusts_07 + other_fin_07 + pass_throughs_07 + 
                  primary_home_07 + other_home_07 + re_fund_07 + other_nonfin_07 - 
                  primary_mortgage_07 - other_mortgage_07 - credit_lines_07 - 
                  credit_cards_07 - student_loans_07 - auto_loans_07 - other_debt_07,
      across(
        .cols = c(income, net_worth), 
        .fns  = ~ cut(
          x      = ., 
          breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100), 
          labels = 1:100
        ) %>% as.character() %>% as.integer() %>% replace_na(0), 
        .names = 'pctile_{col}'
      ), 
      
      # Create other variables
      has_wages = as.integer(wages_07 > 0),
      has_kids  = as.integer(n_kids > 0),
      taxable_debt = taxable_debt_07
    )
    
  # Estimate model parameters for those with debt in t = 0
  # Y variable: percent change in debt from t = 0 to t = 1
  train_with_debt = train %>% filter(taxable_debt > 0)
  model_with_debt = quantregForest(
    x        = train_with_debt[c('age', 'has_kids', 'married', 'has_wages', 
                                 'pctile_income', 'pctile_net_worth', 'taxable_debt')],
    y        = train_with_debt$pct_chg_taxable_debt, 
    nthreads = parallel::detectCores(),
    weights  = train_with_debt$weight,
    mtry     = 7,
    nodesize = 5
  )
  
  # Estimate model parameters for those without debt in t = 0 
  # Y variable: debt in t = 1
  train_without_debt = train %>% filter(taxable_debt == 0)
  model_without_debt = quantregForest(
    x        = train_without_debt[c('age', 'has_kids', 'married', 'has_wages', 
                                    'pctile_income', 'pctile_net_worth')],
    y        = train_without_debt$taxable_debt_08, 
    nthreads = parallel::detectCores(),
    weights  = train_without_debt$weight,
    mtry     = 6,
    nodesize = 5
  )
  
  return(list(model_with_debt, model_without_debt))
}

