#------------------------------------------------------------------------------
# data.R
# 
# Contains functions to read, process, and project data.
#------------------------------------------------------------------------------


process_scf = function() {
  
  #----------------------------------------------------------------------------
  # Reads 2022 bulletin SCF, creates new net worth classes, and extracts 
  # required variables.
  # 
  # Parameters: N/A
  #
  # Output: processed 2022 SCF (df).
  #----------------------------------------------------------------------------
  
  # Read raw bulletin summary 2022 SCF
  file_paths$scf_2022 %>%
    file.path('SCFP2022.csv') %>% 
    read_csv(show_col_types = F) %>% 
    
    # Construct asset classes
    mutate(
      
      # Debt
      primary_mortgage  = MRTHEL, 
      other_mortgage    = RESDBT, 
      credit_lines      = OTHLOC,
      credit_cards      = CCBAL,
      student_loans     = EDN_INST,
      auto_loans        = VEH_INST,
      other_installment = OTH_INST,
      other_debt        = ODEBT 
    ) %>% 
    
    
    # Construct other variables
    mutate(
      married            = as.integer(MARRIED == 1), 
      pass_through_owner = as.integer(BUS > 0)
    ) %>% 
    
    # Subset to required variables
    select(
      
      # Demographics
      id     = Y1,
      weight = WGT, 
      age    = AGE, 
      n_kids = KIDS,
      married, 
      
      # Income
      income = INCOME,
      wages  = WAGEINC,
      
      # Unrealized gains
      kg_primary_home  = KGHOUSE,
      kg_other_re      = KGORE, 
      kg_pass_throughs = KGBUS, 
      kg_other         = KGSTMF, 
      
      # Net worth
      pass_through_owner,
      assets = ASSET, 
      primary_mortgage, other_mortgage, credit_lines, credit_cards, student_loans, auto_loans, other_installment, other_debt
      
    ) %>% 
    return()
}



age_scf_historical = function(augmented_scf, macro_projections) {
  
  #----------------------------------------------------------------------------
  # Ages 2022 SCF through 2024.
  # 
  # Parameters:
  # - augmented_scf     (df)  : processed and augmented 2022 SCF  
  # - macro_projections (lst) : economic and demographic projections 
  #                             (see read_macro_projections)
  #
  # Output: 2024-aged SCF.
  #----------------------------------------------------------------------------
  
  # Get historical growth factors from financial accounts
  asset_growth = read_csv('./resources/financial_accounts/b101.csv', show_col_types = F) %>% 
    
    # Pick start and end dates
    mutate(
      date = case_when(
        date == '2022:Q3' ~ 'start', 
        date == '2024:Q3' ~ 'end', 
        T                 ~ NA
      )
    ) %>% 
    filter(!is.na(date)) %>% 
    
    # Map concepts to our SCF categories
    select(-FL893131573.Q) %>% # no data, creates NA warning
    mutate(
      across(.cols = -date, .fns = as.numeric), 
      
      assets = FL152000005.Q,	
      
      # Debt
      primary_mortgage  = LM152010005.Q, 
      other_mortgage    = LM152010005.Q, 
      credit_lines      = FL154123005.Q,
      credit_cards      = FL153166000.Q,
      student_loans     = FL154123005.Q,
      auto_loans        = FL154123005.Q,
      other_installment = FL154123005.Q,
      other_debt        = FL154123005.Q, 
      
      # Unrealized capital gains
      kg_primary_home  = LM155035005.Q,
      kg_other_re      = LM155035005.Q, 
      kg_pass_throughs = LM153081115.Q, 
      kg_other         = FL154090005.Q, 
      
    ) %>% 
    select(-contains('.')) %>%
    
    # Adjust to be on a per-capita basis
    pivot_longer(cols = -date, names_to = 'variable') %>%
    left_join(
      macro_projections$demographic %>% 
        filter(year == 2022 | year == 2024) %>% 
        group_by(date = if_else(year == 2022, 'start', 'end')) %>% 
        summarise(population = sum(population)), 
      by = 'date'
    ) %>% 
    mutate(value = value * 1e6 / population) %>% 
    
    # Reshape wide in year and calulate growth rates
    select(-population) %>% 
    pivot_wider(names_from = date) %>% 
    mutate(asset_factor = end / start) %>% 
    select(variable, asset_factor)
  
  
  # Calculate income growth factor
  income_growth = macro_projections$economic %>% 
    filter(year == 2022 | year == 2024) %>%
    left_join(
      macro_projections$demographic %>% 
        group_by(year) %>% 
        summarise(population = sum(population)), 
      by = 'year'
    ) %>% 
    mutate(value = gdp * 1e9 / population) %>% 
    select(name = year, value) %>% 
    pivot_wider() %>% 
    mutate(income_factor = `2024` / `2022`) %>%
    select(income_factor)
  
  
  # Calculate demographic growth factors
  demographic_growth = macro_projections$demographic %>% 
    filter(year == 2024 | year == 2022) %>%
    pivot_wider(
      names_from  = year, 
      values_from = population
    ) %>% 
    mutate(demographic_factor = `2024` / `2022`) %>% 
    select(married, age, demographic_factor)
  
  # Apply growth factors for demographics and income
  augmented_scf %<>% 
    left_join(demographic_growth, by = c('married', 'age')) %>% 
    mutate(
      weight = weight * demographic_factor, 
      income = income * income_growth$income_factor
    ) %>% 
    select(-demographic_factor)
  
  # Apply growth factors for wealth variables
  for (var in asset_growth$variable) {
    augmented_scf[[var]] = augmented_scf[[var]] * (asset_growth %>% filter(variable == var))$asset_factor
  }
  
  return(augmented_scf)
}



add_forbes_data = function(augmented_scf) {
  
  #----------------------------------------------------------------------------
  # Adds Forbes 2024 billionaires data to the SCF data, filtering for American 
  # billionaires and removing existing billionaires from the SCF data.
  # Assumes net worth composition equal to that of those in the SCF with at 
  # least $500M in net worth. Also assumes billionaires are married.
  # 
  # Parameters: 
  # - augmented_scf (df) : processed SCF data
  #
  # Output: updated SCF data with Forbes 2024 billionaires added (df).
  #----------------------------------------------------------------------------
  
  net_worth_components = c('assets', 'primary_mortgage', 'other_mortgage', 
                           'credit_lines', 'credit_cards', 'student_loans', 
                           'auto_loans', 'other_installment', 'other_debt')
  
  # Add net worth to SCF
  augmented_scf = augmented_scf %>%
    mutate(
      net_worth = assets - primary_mortgage - other_mortgage - credit_lines - 
                  credit_cards - student_loans - auto_loans - other_installment - other_debt
    )
  
  # Estimate mean shares of net worth among SCF billionaires and near-billionaires
  wealth_composition = augmented_scf %>% 
    filter(net_worth >= 500e6) %>%
    summarise(
      across(
        .cols = c(all_of(net_worth_components), wages, income, pass_through_owner, starts_with('kg_')), 
        .fns  = ~ sum(. * weight) / sum(net_worth * weight)
      )
    )
  
  # Load the Forbes 2024 billionaires list
  forbes = read_csv('./resources/forbes/billionaires_2024.csv', show_col_types = F) %>% 
    
    # Filter to Americans
    filter(country_of_citizenship == 'United States') %>% 
    
    # Clean up net worth column
    mutate(net_worth = as.numeric(str_sub(net_worth, end = -2)) * 1e9) %>% 
    
    # Impute wealth composition
    bind_cols(wealth_composition) %>% 
    mutate(
      across(
        .cols = all_of(net_worth_components),
        .fns  = ~ . * net_worth
      ),
      pass_through_owner = as.integer(runif(nrow(.)) < pass_through_owner)
    ) %>% 
    
    # Add other variables and remove extraneous Forbes info. Caps age at 100 to
    # match CBO's demographic groups, and assigns average age for those with 
    # missing age. Assumes all are married and has one child (non-NA values are 
    # required for imputation models but have ~zero bearing on results).
    mutate(
      weight   = 1,
      age      = pmin(100, if_else(is.na(age), round(mean(age, na.rm = T)), age)),  
      married  = 1, 
      n_kids   = 1,
      id       = 1000000 + row_number()
    ) %>% 
    select(
      id, weight, name = full_name, age, married, n_kids,
      all_of(net_worth_components), wages, income, starts_with('kg'), pass_through_owner
    )
  
  # Remove billionaires from SCF, add forbes, and return
  augmented_scf %>%
    filter(net_worth < 1e9) %>% 
    select(-net_worth) %>% 
    bind_rows(forbes) %>% 
    relocate(name, .after = weight) %>% 
    return()
}



read_macro_projections = function() {
  
  #----------------------------------------------------------------------------
  # Reads macro projections file and subsets to required variables, splitting
  # output into economic and demograpic components. 
  # 
  # Parameters:
  # - scenario_id (str) : scenario ID
  # 
  # Output: list containing economic macro projections (df) and demographic
  #          projections (df). 
  #----------------------------------------------------------------------------
  
  # Read data
  raw = c('projections.csv', 'historical.csv') %>% 
    map(
      .f = ~ file_paths$macro_projections %>% 
        file.path(.x) %>% 
        read_csv(show_col_types = F) 
    ) %>% 
    bind_rows() %>% 
    arrange(year)
  
  # Create economic output
  macro_projections = list()
  macro_projections$economic = raw %>%  
    select(year, gdp, cpiu, ccpiu_irs)
  
  # Create demographic output
  macro_projections$demographic = raw %>% 
    select(year, contains('married')) %>% 
    pivot_longer(
      cols      = -year, 
      names_sep = '_', 
      names_to  = c('married', 'age'), 
      values_to = 'population'
    ) %>% 
    mutate(
      married = as.integer(married == 'married'), 
      age     = as.integer(age)
    ) 
  
  return(macro_projections)
}



process_scf_panel = function() {
  
  #----------------------------------------------------------------------------
  # Reads 2009 SCF panel, creates new net worth classes, and extracts 
  # required variables.
  # 
  # Parameters: N/A
  #
  # Output: processed 2009 SCF panel (df).
  #----------------------------------------------------------------------------
  
  # Read raw data
  file_paths$scf_2009_panel %>%
    file.path('SCFP2009panel.csv') %>% 
    read_csv(show_col_types = F) %>% 
    
    # Reshape long in year
    rename(id = Y1, weight = WGT09) %>% 
    select(id, weight, ends_with('07'), ends_with('09')) %>% 
    pivot_longer(cols = -c(id, weight)) %>% 
    mutate(
      year = str_sub(name, -2), 
      name = str_sub(name, 1, -3)
    ) %>% 
    pivot_wider() %>% 
    
    # Harmonize variable names and concepts with those of augmented SCF above
    mutate(
      assets            = ASSET,
      primary_mortgage  = MRTHEL, 
      other_mortgage    = RESDBT, 
      credit_lines      = OTHLOC,
      credit_cards      = CCBAL,
      student_loans     = EDNINST,
      auto_loans        = VEHINST,
      other_installment = OTHINST,
      other_debt        = ODEBT
    ) %>% 
    
    # Construct other variables
    mutate(
      married = as.integer(MARRIED == 1), 
    ) %>% 
    
    # Reshape wide in year
    pivot_wider(
      names_from  = year, 
      values_from = -c(id, weight)
    ) %>% 
    
    # Subset to required variables
    select(
      
      # Demographics
      id, 
      weight, 
      age     = AGE_07, 
      n_kids  = KIDS_07,
      married = married_07, 
      
      # Income
      income_07  = INCOME_07, 
      income_09  = INCOME_09,
      wages_07   = WAGEINC_07,
      wages_09   = WAGEINC_09,
      
      # Assets
      assets_07, assets_09,
      
      # Debt
      primary_mortgage_07,  primary_mortgage_09,  
      other_mortgage_07,    other_mortgage_09,  
      credit_lines_07,      credit_lines_09,  
      credit_cards_07,      credit_cards_09,  
      student_loans_07,     student_loans_09,  
      auto_loans_07,        auto_loans_09,  
      other_installment_07, other_installment_09,  
      other_debt_07,        other_debt_09
      
    ) %>% 
    return()
}

  

impute_borrowing_flows = function(augmented_scf, use_cache = TRUE) {
  
  #----------------------------------------------------------------------------
  # Imputes net new borrowing for all records in the current SCF, with option
  # to read/write results from CSV cache
  # 
  # Parameters:
  #   - augmented_scf (df) : SCF + Forbes data projected through 2024
  #   - use_cache (bool)   : whether to try reading from cache first
  #   - cache_path (str)   : path to cache CSV file
  #
  # Output: Augmented SCF with imputed borrowing flows.
  #----------------------------------------------------------------------------
  
  # Try reading from cache if requested
  cache_path = file.path(file_paths$cache_root, 'borrowing_imputations.csv')
  if (use_cache && file.exists(cache_path)) {
    message('Reading cached borrowing imputations')
    return(
      augmented_scf %>%
        mutate(taxable_debt = credit_lines + other_debt + other_mortgage) %>%
        left_join(read_csv(cache_path, show_col_types = FALSE), by = 'id')
    )
  }
  
  message('Computing new borrowing imputations')
  
  # Estimate model of positive new borrowing based on SCF panel from 2007-2009
  model = estimate_borrowing_model()
  
  # Add required X variables to 2024 SCF
  imputation_data = augmented_scf %>%
    mutate(
      has_wages    = as.integer(wages > 0),
      has_kids     = n_kids > 0, 
      taxable_debt = credit_lines + other_debt + other_mortgage,
      taxable_debt = taxable_debt + if_else(taxable_debt != 0, runif(nrow(.)), 0),
      income       = income + if_else(income != 0, runif(nrow(.)), 0),
      net_worth    = assets - primary_mortgage - other_mortgage - 
                     credit_lines - credit_cards - student_loans - 
                     auto_loans - other_installment - other_debt,
      across(
        .cols = c(income, assets, taxable_debt, net_worth), 
        .fns  = ~ cut(
          x      = ., 
          breaks = wtd.quantile(.[. > 0], weight[. > 0], c(seq(0, 0.99, 0.01), seq(0.991, 1, 0.001))), 
          labels = c(seq(0.01, 0.99, 0.01), seq(0.991, 1, 0.001))
        ) %>% as.character() %>% as.numeric() %>% replace_na(0), 
        .names = 'pctile_{col}'
      )
    )
  
  # Fit values
  imputations = imputation_data %>%
    select(
      id, weight, age, has_kids, married, has_wages, taxable_debt, 
      pctile_income, pctile_assets, pctile_taxable_debt, pctile_net_worth
    ) %>% 
    mutate(
      # Fit values 
      yhat = predict(
        object  = model,
        newdata = (.),
        what    = function(x) sample(x, 1)
      ), 
      
      # Scale to population mean of taxable debt
      positive_taxable_borrowing = weighted.mean(taxable_debt, weight) * yhat
    ) %>% 
    select(id, positive_taxable_borrowing)
  
  # Save to cache
  if (!dir.exists(dirname(cache_path))) {
    dir.create(dirname(cache_path, recursive = TRUE))
  }
  write_csv(imputations, cache_path)
  message('Saved borrowing imputations to ', cache_path)
  
  # Add to data and return 
  augmented_scf %>%
    mutate(taxable_debt = credit_lines + other_debt + other_mortgage) %>% 
    left_join(imputations, by = "id") %>% 
    return()
}



impute_expected_death_age = function(augmented_scf) {
  
  #----------------------------------------------------------------------------
  # Imputes expected death age using 2021 SSA life expectancy tables.
  # 
  # Parameters:
  #   - augmented_scf (df)  : SCF + Forbes data projected through 2024
  #
  # Output: input data with new variable for expected death age. 
  #----------------------------------------------------------------------------
  
  # Read SSA life expectancy data 
  ssa = read_csv('./resources/ssa/life_expectancy_2021.csv', show_col_types = F)
  
  # Add to SCF data and return
  augmented_scf %>% 
    left_join(ssa, by = 'age') %>% 
    relocate(age_expected_death, .after = age) %>% 
    mutate(age_expected_death = as.integer(round(age_expected_death))) %>% 
    return()
}

