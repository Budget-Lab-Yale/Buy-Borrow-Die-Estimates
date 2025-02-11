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
  # Output: TODO
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
      
      # Calculate change (y variable), with special code for cases with no debt at t = 0 
      taxable_debt_pct_chg = if_else(
        taxable_debt_07 == 0, 
        -2,
        taxable_debt_08 / taxable_debt_07 - 1
      ),  
      
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
      has_wages = as.integer(wages_07 > 0) 
    )
    
  
  
    
 
  quantregForest()
   
}





  # Create percentile variables
  mutate(
    wages  = if_else(wages > 0,  wages  + runif(nrow(.)), 0),
    income = if_else(income > 0, income + runif(nrow(.)), 0),
    across(
      .cols = c(wages, income), 
      .fns  = ~ cut(
        x      = . , 
        breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100), 
        labels = 1:100
      ) %>% as.character() %>% as.integer() %>% replace_na(0), 
      .names = 'pctile_{col}'
    ) 
  ) %>% 
  
  # Recode kids and marital status variables to match PUF
  mutate(
    n_kids  = pmin(n_kids, 3), 
    married = as.integer(married == 1)
  )


# Estimate model of presence auto loan interest  
has_auto_qrf = quantregForest(
  x        = scf[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')],
  y        = as.factor(scf$has_auto_int_exp), 
  nthreads = parallel::detectCores(),
  weights  = scf$weight,
  mtry     = 4,
  nodesize = 1
)

# Estimate model of auto loan interest among those with loans
scf_int = scf %>% 
  filter(auto_int_exp > 0)
auto_qrf = quantregForest(
  x        = scf_int[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')],
  y        = scf_int$auto_int_exp, 
  nthreads = parallel::detectCores(),
  weights  = scf_int$weight,
  mtry     = 4,
  nodesize = 5
)


# Impute on tax unit data observations
auto_int_exp = tax_units %>% 
  mutate(
    n_kids = (
      (!is.na(dep_age1) & dep_age1 <= 24) + 
        (!is.na(dep_age2) & dep_age2 <= 24) + 
        (!is.na(dep_age3) & dep_age3 <= 24)
    ), 
    across(
      .cols = c(wages, income), 
      .fns  = ~ cut(
        x      = . , 
        breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100), 
        labels = 1:100
      ) %>% as.character() %>% as.integer() %>% replace_na(0), 
      .names = 'pctile_{col}'
    ) 
  ) %>% 
  select(id, weight, age1, n_kids, married, pctile_income, pctile_wages) %>% 
  mutate(
    p = predict(
      object  = has_auto_qrf, 
      newdata = (.),
      what    = function(x) mean(x - 1)
    ),
    auto_int_exp = pmin(max_ot, predict(
      object  = auto_qrf, 
      newdata = (.),
      what    = function(x) sample(x, 1)
    ) * (runif(nrow(.)) < p))
  )


