#------------------------------------------------------------------------------
# sim_option_3.R
# 
# Contains functions to calculate taxes and simulate years for option 3 
# (excise tax on borrowing)
#------------------------------------------------------------------------------


sim_option_3 = function(augmented_scf, macro_projections, static_totals = NULL) {
  
  #----------------------------------------------------------------------------
  # Projects revenue under option 2. With default inputs, runs twice 
  # recursively -- static and non-static (with avoidance) modes. 
  # 
  # Parameters:
  #   - augmented_scf     (df)  : SCF+ data projected through 2024
  #   - macro_projections (lst) : economic and demographic projections
  #                               (see read_macro_projections)
  #   - static_totals     (df)  : results of this function, passed to recursive
  #                               call to simulate with avoidance
  #
  # Output: dataframe of aggregate output from simulation.
  #----------------------------------------------------------------------------
  
  # Initialize tracking objects
  totals         = tibble()
  revenue_offset = tibble()
  current_scf    = augmented_scf
  
  # For each projection year
  for (year in 2025:max(years)) {
    
    # Age data forward to this year
    # age_option_2() works for our purposes here, no need for a new function
    current_scf = current_scf %>% 
      age_option_2(year, macro_projections)
    
    # Skip if before enactment
    if (year < 2026) next
    
    # Calculate record-level taxes and credits
    year_results = calc_tax_option_3(current_scf, year, is.null(static_totals))
    
    # Calculate and write distribution tables for first year of policy
    if (year == 2026) get_distribution_option_3(year_results)
    
    # Calculate and store aggregate statistics
    year_totals = get_totals_option_3(year_results, year)
    totals = bind_rows(totals, year_totals)

  }
  
  # Calculate net revenue effect and return
  totals = totals %>%
    mutate(net_revenue = excise_tax) %>%
    mutate(static = is.null(static_totals), .before = everything())
  
  # Recurse
  if (is.null(static_totals)) {
    sim_option_3(augmented_scf, macro_projections, totals)
  } else {
    static_totals %>% 
      bind_rows(totals) %>% 
      return()
  }
}



calc_tax_option_3 = function(current_scf, year, static) {
  
  #----------------------------------------------------------------------------
  # Calculates excise tax liability under option 3.
  # 
  # Parameters:
  #   - current_scf       (df)   : SCF+ data projected through given year
  #   - year              (int)  : year of tax calculation
  #   - static            (bool) : whether running without avoidance
  #
  # Output: processed SCF with tax calculations
  #----------------------------------------------------------------------------
  
  # Define tax law parameters
  excise_rate = 0.01
  
  # Start with SCF...
  current_scf %>%
    
    # Do avoidance if non-static
    do_avoidance_option_3(year, static) %>%
    
    # Calculate tax
    mutate(
      year = !!year,
      
      excise_tax = taxable_debt * excise_rate
    ) %>%
    return()
}



do_avoidance_option_3 = function(current_scf, year, static) {
  
  #----------------------------------------------------------------------------
  # Applies avoidance responses to taxable debt, including both business 
  # sheltering and intertemporal smoothing. 
  # 
  # 
  # Parameters:
  #   - current_scf        (df) : SCF+ data projected through given year
  #   - year              (int) : year of tax calculation
  #   - exemption         (dbl) : borrowing exemption threshold for this year 
  #   - macro_projections (lst) : economic and demographic projections 
  #                               (see read_macro_projections)
  #   - static           (bool) : whether running without avoidance
  #
  # Output: SCF data with avoidance adjustments applied
  #----------------------------------------------------------------------------
  
  # Return if static!
  if (static) return(current_scf)
  
  
  # -----------
  # Parameters
  # -----------
  
  # Log-linear tax price elasticity for $1 of consumption from Leite (2024)
  # 31% of consumption shifts in response to a ~0.8pp net-of-tax price wedge
  elasticity = 0.31 / (1 / ((1 - 0.23) * (1 - 0.28)) - 1)  
  
  
  #------------------------------
  # Smoothing/retiming avoidance
  #------------------------------
  
  current_scf %>% 
  
    #-------------------------------
    # Business sheltering avoidance
    #-------------------------------
  
    mutate(
    
      # First, calculate marginal rate on a new dollar of borrowing
      tax  = calc_tax_option_3(current_scf, year, T)$excise_tax, 
      tax1 = calc_tax_option_3(current_scf %>% mutate(taxable_debt = taxable_debt + 1), year, T)$excise_tax,
      mtr  = tax1 - tax, 
    
      # Express as tax-price wedge
      tax_price = 1 / (1 - mtr) - 1,
    
      # Calculate fraction of borrowing shifted
      percent_shifted = tax_price * elasticity,
      
      # For pass-through owners, reduce taxable borrowing by sheltering rate
      taxable_debt = taxable_debt * (1 - percent_shifted * pass_through_owner)
      
    ) %>%
    select(-percent_shifted) %>% 
    return()
}



get_totals_option_3 = function(year_results, year) {
  
  #----------------------------------------------------------------------------
  # Calculates aggregate statistics for a given simulation year for option 3.
  # 
  # Parameters:
  #   - year_results (df) : record-level results from calc_tax_option_3
  #   - year        (int) : current simulation year
  #
  # Output: dataframe with aggregated results
  #----------------------------------------------------------------------------
  
  year_results %>%
    summarise(
      year = !!year,
      
      # Total unrealized gains eligible for future credits
      unrealized_kg = sum((kg_pass_throughs + kg_other) * weight) / 1e9,
      
      # Total positive borrowing
      taxable_positive_net_borrowing = sum(taxable_debt * weight) / 1e9,
      
      # Borrowing net of exemption
      taxable_borrowing_after_exemption = taxable_positive_net_borrowing,
      
      # Number of taxpayers
      n_taxpayers = sum((excise_tax > 0) * weight),
      share_taxpayers = weighted.mean(excise_tax > 0, weight),
      
      # Withholding tax revenue in billions
      excise_tax = sum(excise_tax * weight) / 1e9
    )
}



get_distribution_option_3 = function(year_results) {
  
  #----------------------------------------------------------------------------
  # Calculates and writes distributional impact of option 3 (excise tax) 
  # by income percentile, age group, and wealth percentile.
  # 
  # Parameters:
  # - year_results (df) : results from calc_tax_option_3
  #
  # Returns: list of three data frames containing distributional analysis
  #----------------------------------------------------------------------------
  
  # Define age groups
  age_groups = list(
    'Under 35' = c(0, 34),
    '35-44'    = c(35, 44),
    '45-54'    = c(45, 54),
    '55-64'    = c(55, 64),
    '65+'      = c(65, 100)
  )
  
  # Function to calculate percentile groups
  get_percentile_group = function(x, weights, breaks) {
    qtiles = wtd.quantile(x, weights, probs = breaks / 100)
    cut(x, 
        breaks = c(-Inf, qtiles, Inf),
        labels = c(0, breaks)
    )
  }
  
  # Function to calculate metrics for each group
  calc_group_metrics = function(df, group) {
    df %>%
      group_by(!!sym(group)) %>%
      summarise(
        n_taxpayers          = sum((excise_tax > 0) * weight),
        share_taxpayers      = weighted.mean(excise_tax > 0, weight),
        avg_tax             = weighted.mean(excise_tax, weight),
        avg_tax_if_positive = weighted.mean(excise_tax, weight * (excise_tax > 0)),
        pct_chg_income      = weighted.mean(-excise_tax / income, income * weight),
        .groups = 'drop'
      )
  }
  
  # Assign groups
  year_results = year_results %>%
    mutate(
      net_worth     = assets - primary_mortgage - other_mortgage - 
                      credit_lines - credit_cards - student_loans - 
                      auto_loans - other_installment - other_debt,
      person_weight = weight * (1 + married),
      income_group  = get_percentile_group(income, person_weight, c(20, 40, 60, 80, 90, 95, 99, 99.9)),
      wealth_group  = get_percentile_group(net_worth, weight, c(20, 40, 60, 80, 90, 95, 99, 99.9)),
      age_group     = cut(age,
                          breaks = c(0, 34, 44, 54, 64, 100),
                          labels = names(age_groups),
                          include.lowest = TRUE)
    )
  
  # Calculate distributions
  distributions = list(
    by_income = calc_group_metrics(year_results, 'income_group'),
    by_wealth = calc_group_metrics(year_results, 'wealth_group'),
    by_age    = calc_group_metrics(year_results, 'age_group')
  )
  
  # Create output directory if it doesn't exist
  dir.create(file.path(file_paths$output_root, 'option_3'), recursive = TRUE, showWarnings = FALSE)
  
  # Write each distribution table to a separate CSV
  for (name in names(distributions)) {
    distributions[[name]] %>%
      write_csv(
        file.path(file_paths$output_root,
                  'option_3',
                  paste0('distribution_', name, '_', year_results$year[1], '.csv'))
      )
  }
  
  return(distributions)
}
