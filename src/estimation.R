#------------------------------------------------------------------------------
# estimation.R
# 
# TODO
#------------------------------------------------------------------------------



estimate_borrowing_model = function() {
  
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
      pct_chg_taxable_debt = taxable_debt_08 / taxable_debt_07 - 1,
      
      # Calculate taxable debt share of assets (y variable)
      taxable_debt_share = (taxable_debt_08 - taxable_debt_07) / assets_07,
      
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
      taxable_debt = taxable_debt_07
    )
    
  # Estimate model parameters for those with taxable debt in t = 0
  # Y variable: percent change in debt from t = 0 to t = 1
  train_with_debt = train %>% filter(taxable_debt > 0)
  model_with_debt = quantregForest(
    x        = train_with_debt[c('age', 'has_kids', 'married', 'has_wages',
                                 'pctile_income', 'pctile_assets', 'pctile_taxable_debt', 'pctile_net_worth')],
    y        = train_with_debt$pct_chg_taxable_debt, 
    nthreads = parallel::detectCores(),
    weights  = train_with_debt$weight,
    mtry     = 8,
    nodesize = 5
  )
  
  # Estimate model parameters for those without taxable debt in t = 0 but with assets
  # Y variable: taxable debt share of assets in t = 1
  train_without_debt_with_assets = train %>% filter(taxable_debt == 0, assets > 0)
  model_without_debt_with_assets = quantregForest(
    x        = train_without_debt_with_assets[c('age', 'has_kids', 'married', 'has_wages', 
                                                'pctile_income', 'pctile_assets', 'pctile_net_worth')],
    y        = train_without_debt_with_assets$taxable_debt_share, 
    nthreads = parallel::detectCores(),
    weights  = train_without_debt_with_assets$weight,
    mtry     = 7,
    nodesize = 5
  )
  
  # Estimate simple parameters for those without taxable debt or assets in t = 0:
  # 1) probability of having taxable debt in t = 1
  # 2) conditional on having taxable debt in t = 1, mean taxable debt value as 
  #    as a fraction of population-wide mean assets 
  train_without_debt_without_assets = train %>% filter(taxable_debt == 0, assets == 0)
  model_without_debt_without_assets = list()
  model_without_debt_without_assets$p = train_without_debt_without_assets %>% 
    summarise(p = sum((taxable_debt_08 > 0) * weight) / sum(weight)) %>% 
    deframe()
  model_without_debt_without_assets$mean_share = train_without_debt_without_assets %>% 
    filter(taxable_debt_08 > 0) %>% 
    summarise(group_mean_taxable_debt = weighted.mean(taxable_debt_08, weight)) %>% 
    bind_cols(
      train %>% 
        summarise(mean_assets = weighted.mean(assets, weight)) 
    ) %>% 
    mutate(mean_share = group_mean_taxable_debt / mean_assets) %>% 
    select(mean_share) %>% 
    deframe()
  
  # Return models as a list
  return(
    list(
      with_debt                   = model_with_debt, 
      without_debt_with_assets    = model_without_debt_with_assets, 
      without_debt_without_assets = model_without_debt_without_assets
    )
  )
}




