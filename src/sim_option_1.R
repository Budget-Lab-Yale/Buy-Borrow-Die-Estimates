#------------------------------------------------------------------------------
# sim_option_1.R
# 
# Contains functions to calculate taxes and simulate years for option 1 
# (deemed realization for borrowing)
#------------------------------------------------------------------------------



sim_option_1 = function(augmented_scf, macro_projections, static_totals = NULL) {
  
  #----------------------------------------------------------------------------
  # Projects revenue under option 1. With default inputs, runs twice 
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
  
  # Initialize unrealized gain adjustment factors to 1 (pre-enactment) 
  micro_factors    = rep(1, nrow(current_scf))
  aggregate_factor = 1 
  
  # For each projection year
  for (year in 2025:max(years)) {
    
    # Age data forward to this year
    current_scf = current_scf %>% 
      age_option_1(year, macro_projections, micro_factors, aggregate_factor)
    
    # Skip if before enactment
    if (year < 2026) next
    
    # Calculate record-level taxes
    year_results = calc_tax_option_1(current_scf, year, macro_projections, is.null(static_totals))
    
    # Calculate and write distribution tables for first year of policy
    if (year == 2026) get_distribution_option_1(year_results)
    
    # Calculate and store aggregate statistics
    year_totals    = get_totals_option_1(year_results, year)
    totals         = bind_rows(totals, year_totals$totals)
    revenue_offset = bind_rows(revenue_offset, year_totals$revenue_offset)
    
    # Calculate record-level adjustments to capital gains based on deemed realization
    micro_factors = current_scf %>%
      mutate(
        unrealized_kg = kg_primary_home + kg_other_re + kg_pass_throughs + kg_other,
        micro_factor  = if_else(unrealized_kg > 0, 1 - year_results$deemed_realization / unrealized_kg, 1)
      ) %>% 
      pull(micro_factor)
    
    # Calculate aggregate adjustment factor for unrealized capital gains based on deemed realization 
    aggregate_factor = 1 - year_totals$totals$deemed_realization / year_totals$totals$unrealized_kg
  }
  
  # Incorporate revenue offset
  totals = totals %>%
    left_join(
      revenue_offset %>%
        group_by(year) %>%
        summarise(revenue_offset = sum(revenue_offset)),
      by = 'year'
    ) %>%
    mutate(net_revenue = borrowing_tax + replace_na(revenue_offset, 0)) %>%
    mutate(static = is.null(static_totals), .before = everything())
  
  # Recurse
  if (is.null(static_totals)) {
    sim_option_1(augmented_scf, macro_projections, totals)
  } else {
    static_totals %>% 
      bind_rows(totals) %>% 
      return()
  }
}



calc_tax_option_1 = function(current_scf, year, macro_projections, static) {
  
  #----------------------------------------------------------------------------
  # Calculates tax liability under option 1 (deemed realization for borrowing).
  # 
  # Parameters:
  #   - current_scf        (df) : SCF+ data projected through given year
  #   - year              (int) : year of tax calculation
  #   - macro_projections (lst) : economic and demographic projections 
  #                               (see read_macro_projections)
  #   - static           (bool) : whether running without avoidance
  #
  # Output: processed 2022 SCF (df).
  #----------------------------------------------------------------------------
  
  #----------------
  # Set parameters
  #----------------
  
  # Share of gains held to death among taxable borrowers
  share_death = 0.8
  
  # Tax law parameters
  base_exemption = 250000
  tax_rate  = 0.238
  
  # Adjust tax law parameters for inflation
  inflation_factor = macro_projections$economic %>%
    summarise(factor = ccpiu_irs[year == !!year] / ccpiu_irs[year == 2026]) %>%
    pull(factor)
  
  exemption = base_exemption * inflation_factor
  
  
  #------------------------------------------
  # Calculate taxes due at time of borrowing
  #------------------------------------------
  
  # Start with SCF... 
  initial_tax = current_scf %>% 
    
    # Do avoidance if non-static
    do_avoidance_option_1(year, exemption, macro_projections, static) %>%
    
    # Calculate tax
    mutate(
      year = !!year,
      
      # Subtract annual borrowing exemption
      borrowing_after_exemption = pmax(0, positive_taxable_borrowing - if_else(married == 1, exemption * 2, exemption)),
    
      # Calculate basis share for use in imputing deemed realization
      basis_share = pmin(1, pmax(0, 1 - if_else(assets > 0, (kg_primary_home + kg_other_re + kg_pass_throughs + kg_other) / assets, 1))),
      
      # Calculate deemed realization
      deemed_realization = borrowing_after_exemption * (1 - basis_share),
      
      # Calculate tax on deemed realization
      borrowing_tax = deemed_realization * tax_rate
      
    ) %>% 
    select(id, year, borrowing_after_exemption, basis_share, deemed_realization, borrowing_tax)
  
  
  #------------------------------------------------
  # Calculate implications for future tax payments
  #------------------------------------------------
  
  tax_adjustments = current_scf %>% 
    
    # Expand to future years among taxpayers
    left_join(initial_tax, by = 'id') %>%
    filter(borrowing_tax > 0) %>%
    select(id, age, age_expected_death, borrowing_tax, weight, year) %>%
    expand_grid(years_forward = 1:75) %>%  
    filter(age + years_forward <= age_expected_death) %>% 
    
    # Prevent double counting by removing tax on would-be realizations
    mutate(
      years_left = age_expected_death - age,
      tax_adjustment = if_else(
        years_left > 0, 
        -borrowing_tax * (1 - share_death) / years_left,
        0 
      )
    ) %>% 
    
    # Reshape wide in payment year
    select(id, years_forward, tax_adjustment) %>% 
    pivot_wider(
      names_from   = years_forward, 
      names_prefix = 'tax_adjustment.',
      values_from  = tax_adjustment
    )
  
  #--------------------
  # Combine and return
  #--------------------
  
  current_scf %>% 
    left_join(initial_tax, by = 'id') %>% 
    left_join(tax_adjustments, by = 'id') %>% 
    mutate(
      across(
        .cols = starts_with('tax_adjustment.'), 
        .fns  = ~ replace_na(., 0)
      )
    ) %>% 
    return()
  
}



calc_pv_tax_option_1 = function(year_results) {
  
  #----------------------------------------------------------------------------
  # Calculates the present value of tax changes under option 1 by combining 
  # the initial tax payment with discounted future tax adjustments. Uses a 
  # 4.5% discount rate.
  # 
  # Parameters:
  #   - year_results (df) : record-level results from calc_tax_option_1
  #
  # Output: PV of tax change (df). 
  #----------------------------------------------------------------------------
  
  # Calculate present value of future tax adjustments
  pv_tax_adjustments = year_results %>% 
    select(id, starts_with('tax_adjustment')) %>% 
    pivot_longer(
      cols            = starts_with('tax_adjustment.'),
      names_prefix    = 'tax_adjustment.', 
      names_transform = as.integer, 
      names_to        = 't'
    ) %>% 
    filter(value != 0) %>% 
    mutate(pv = value / (1.045 ^ t)) %>% 
    group_by(id) %>% 
    summarise(pv_tax_adjustments = sum(pv))
  
  # Combine with initial tax on borrowing and return
  year_results %>% 
    left_join(pv_tax_adjustments, by = 'id') %>% 
    mutate(
      pv_tax_change = borrowing_tax + replace_na(pv_tax_adjustments, 0)
    ) %>%
    select(id, pv_tax_change) %>% 
    return()
}
  


calc_mtr_option_1 = function(current_scf, year, macro_projections) {
  
  #----------------------------------------------------------------------------
  # Calculates the marginal tax rate under option 1 by comparing the present 
  # value of tax liability with and without an additional dollar of borrowing. 
  # Used for behavioral response calculations.
  # 
  # Parameters:
  #   - current_scf       (df)  : SCF+ data projected through given year
  #   - year             (int)  : year of tax calculation
  #   - macro_projections (lst) : economic and demographic projections
  #                              (see read_macro_projections)
  #
  # Output: vector of marginal tax rates for each taxpayer (dbl).
  #----------------------------------------------------------------------------
  
  
  # Calculate current PV of tax
  actual = current_scf %>% 
    calc_tax_option_1(year, macro_projections, T) %>% 
    calc_pv_tax_option_1()
  
  # Calculate counterfactually higher borrowing
  plus_one_dollar = current_scf %>% 
    mutate(positive_taxable_borrowing = positive_taxable_borrowing + 1) %>% 
    calc_tax_option_1(year, macro_projections, T) %>% 
    calc_pv_tax_option_1()
  
  return(plus_one_dollar$pv_tax_change - actual$pv_tax_change)
}



do_avoidance_option_1 = function(current_scf, year, exemption, macro_projections, static) {
  
  #----------------------------------------------------------------------------
  # Applies avoidance responses to positive taxable borrowing, including both
  # business sheltering and intertemporal smoothing. 
  # 
  # Based on Leite (2024), "The Firm As Tax Shelter", 
  # https://www.parisschoolofeconomics.eu/app/uploads/2024/11/LEITE-David-JMP-OK.pdf
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
  
  # % of exemption that can be retimed
  smoothing_factor = 1 
  
  # Log-linear tax price elasticity for $1 of consumption from Leite (2024)
  # 31% of consumption shifts in response to a ~0.8pp net-of-tax price wedge
  elasticity = 0.31 / (1 / ((1 - 0.23) * (1 - 0.28)) - 1)  
  
  
  #------------------------------
  # Smoothing/retiming avoidance
  #------------------------------
  
  current_scf %>% 
    mutate(
      
      # Calculate share of debt that can be retimed (non-residential share)
      share_retimable = if_else(
        taxable_debt > 0, 
        (credit_lines + other_debt) / taxable_debt,
        0
      ),
      
      # Calculate maximum amount that could be sheltered through retiming
      max_retimed = if_else(married == 1, exemption * 2, exemption) * smoothing_factor,
      
      # Calculate potential reduction in taxable borrowing from retiming
      potential_reduction = pmin(positive_taxable_borrowing * share_retimable, max_retimed),
      
      # Apply reduction to positive taxable borrowing
      positive_taxable_borrowing = positive_taxable_borrowing - potential_reduction
      
    ) %>% 
    
    #-------------------------------
    # Business sheltering avoidance
    #-------------------------------
  
    mutate(
      
      # First, calculate marginal rate on a new dollar of borrowing
      mtr = calc_mtr_option_1(current_scf, year, macro_projections), 
      
      # Express as tax-price wedge
      tax_price = 1 / (1 - mtr) - 1,
      
      # Calculate fraction of borrowing shifted
      percent_shifted = tax_price * elasticity,
      
      # For pass-through owners, reduce taxable borrowing by sheltering rate
      positive_taxable_borrowing = positive_taxable_borrowing * (1 - percent_shifted * pass_through_owner)
      
    ) %>%
    select(-share_retimable, -max_retimed, -potential_reduction, -percent_shifted) %>% 
    return()
}



age_option_1 = function(current_scf, target_year, macro_projections, micro_factors, 
                        aggregate_factor) {
  
  #----------------------------------------------------------------------------
  # Ages SCF data forward one year for option 1 simulation, applying both micro
  # and aggregate adjustments to unrealized gains.
  # 
  # Parameters:
  #   - current_scf       (df)   : current year's wealth data
  #   - target_year       (int)  : year to age to
  #   - macro_projections (lst)  : economic and demographic projections
  #   - micro_factors     (vec)  : record-level adjustment factors for gains
  #   - aggregate_factor  (dbl)  : aggregate adjustment factor for gains
  #
  # Output: wealth_tax data aged to target year.
  #----------------------------------------------------------------------------
  
  # Parameter: weight on micro vs aggregate adjustment (0-1)
  beta = 0.5
  
  # Calculate demographic growth factors
  demographic_factors = macro_projections$demographic %>% 
    filter(year %in% c(target_year - 1, target_year)) %>%
    mutate(year = if_else(year == target_year, 'target_year', 'base_year')) %>% 
    pivot_wider(
      names_from  = year, 
      values_from = population
    ) %>% 
    mutate(demographic_factor = target_year / base_year) %>% 
    select(married, age, demographic_factor)
  
  
  # Calculate per-capita economic growth rate
  economic_factor = macro_projections$economic %>% 
    filter(year %in% c(target_year - 1, target_year)) %>%
    left_join(
      macro_projections$demographic %>% 
        group_by(year) %>% 
        summarise(population = sum(population)), 
      by = 'year'
    ) %>% 
    mutate(
      gdp_pc = gdp * 1e9 / population,
      year = if_else(year == target_year, 'target_year', 'base_year')
    ) %>%
    select(year, gdp_pc) %>% 
    pivot_wider(
      names_from  = year, 
      values_from = gdp_pc
    ) %>% 
    mutate(economic_factor = target_year / base_year) %>% 
    pull(economic_factor)
  
  
  # Calculate combined adjustment factor for unrealized gains
  kg_adjustment_factor = beta * micro_factors + (1 - beta) * aggregate_factor
  
  
  current_scf %>%
    
    # Join demographic factors
    left_join(demographic_factors, by = c('married', 'age')) %>%
    
    mutate(
      
      # Update weights for population growth
      weight = weight * demographic_factor,
      
      # Update unrealized gains with combined micro/aggregate adjustment
      across(
        .cols = starts_with('kg_'),
        .fns = ~ . * kg_adjustment_factor * economic_factor
      ),
      
      # Update all other monetary variables with economic growth
      across(
        .cols = c(
          income, wages, starts_with('kg_'), assets, 
          primary_mortgage, other_mortgage, credit_lines, 
          credit_cards, student_loans, auto_loans, 
          other_installment, other_debt, taxable_debt,
          positive_taxable_borrowing
        ),
        .fns = ~ . * economic_factor
      )
    ) %>%
    
    # Remove temporary columns
    select(-demographic_factor) %>%
    return()
}



get_totals_option_1 = function(year_results, year) {
  
  #----------------------------------------------------------------------------
  # Calculates aggregate statistics for a given simulation year for option 1.
  # 
  # Parameters:
  #   - year_results (df) : record-level results from calc_option_1_tax
  #   - year        (int) : current simulation year
  #
  # Output: list of initial-year totals and future revenue offsets (list).
  #----------------------------------------------------------------------------
  
  totals = year_results %>%
    summarise(
      year = !!year,
      
      # Total unrealized gains for assets that are eligible for deemed realization
      unrealized_kg = sum((kg_pass_throughs + kg_other) * weight) / 1e9,
      
      # Total positive borrowing
      taxable_positive_net_borrowing = sum(positive_taxable_borrowing * weight) / 1e9, 
      
      # Borrowing net of exemption 
      taxable_borrowing_after_exemption = sum(borrowing_after_exemption * weight) / 1e9,
      
      # Total deemed realization
      deemed_realization = sum(deemed_realization * weight) / 1e9,
      
      # Share of eligible gains constructively realized
      realization_rate = deemed_realization / unrealized_kg,
      
      # Number of taxpayers
      n_taxpayers     = sum((borrowing_tax > 0) * weight),
      share_taxapyers = weighted.mean(borrowing_tax > 0, weight),
      
      # Tax revenue in billions
      borrowing_tax = sum(borrowing_tax * weight) / 1e9
    )
  
  
  # Calculate total future tax adjustments
  revenue_offset = year_results %>% 
    select(id, weight, starts_with('tax_adjustment')) %>% 
    pivot_longer(
      cols            = starts_with('tax_adjustment.'),
      names_prefix    = 'tax_adjustment.', 
      names_transform = as.integer, 
      names_to        = 't'
    ) %>% 
    filter(value != 0) %>% 
    mutate(year = year + t) %>% 
    group_by(initial_year = !!year, year) %>% 
    summarise(revenue_offset = sum(value * weight) / 1e9, .groups = 'drop')
  
  return(
    list(
      totals         = totals,
      revenue_offset = revenue_offset
    )
  )
    
}



get_distribution_option_1 = function(year_results) {
  
  #----------------------------------------------------------------------------
  # Calculates and writes distributional impact of option 1 (deemed 
  # realization) by income percentile, age group, and wealth percentile.
  # 
  # Parameters:
  # - year_results (df) : microdata results from calc_tax_option_1
  #
  # Returns: void.
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
        n_taxpayers         = sum((pv_tax_change > 0) * weight),
        share_taxpayers     = weighted.mean(pv_tax_change > 0, weight),
        avg_tax             = weighted.mean(pv_tax_change, weight),
        avg_tax_if_positive = weighted.mean(pv_tax_change, weight * (pv_tax_change > 0)),
        pct_chg_income      = weighted.mean(-pv_tax_change / income, income * weight),
        .groups = 'drop'
      )
  }
  
  # Calculate PV of tax change and assign groups 
  year_results = year_results %>% 
    left_join(calc_pv_tax_option_1(year_results), by = 'id') %>% 
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
  dir.create(file.path(file_paths$output_root, 'option_1'), recursive = TRUE, showWarnings = FALSE)
  
  # Write each distribution table to a separate CSV
  for (name in names(distributions)) {
    distributions[[name]] %>%
      write_csv(
        file.path(file_paths$output_root,
                  'option_1',
                  paste0('distribution_', name, '_', year_results$year[1], '.csv'))
      )
  }
  
  return(distributions)
}


