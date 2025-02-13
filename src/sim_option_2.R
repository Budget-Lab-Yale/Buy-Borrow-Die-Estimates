#------------------------------------------------------------------------------
# sim_option_2.R
# 
# Contains functions to calculate taxes and simulate years for option 2 
# (withholding tax on borrowing)
#------------------------------------------------------------------------------


sim_option_2 = function(augmented_scf, macro_projections, static_totals = NULL) {
  
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
    current_scf = current_scf %>% 
      age_option_2(year, macro_projections)
    
    # Skip if before enactment
    if (year < 2026) next
    
    # Calculate record-level taxes and credits
    year_results = calc_tax_option_2(current_scf, year, macro_projections, is.null(static_totals))
    
    # Calculate and store aggregate statistics
    year_totals = get_totals_option_2(year_results, year)
    totals = bind_rows(totals, year_totals)
    
    # Calculate credit schedule
    revenue_offset = bind_rows(
      revenue_offset,
      calc_revenue_offset_option_2(year_results)
    )
  }
  
  # Calculate net revenue effect and return
  totals = totals %>%
    left_join(
      revenue_offset %>%
        group_by(year) %>%
        summarise(
          across(
            .cols = starts_with('revenue_offset'), 
            .fns  = sum
          ),
          .groups = 'drop'
        ),
      by = 'year'
    ) %>%
    mutate(net_revenue = withholding_tax + replace_na(revenue_offset, 0)) %>%
    mutate(static = is.null(static_totals), .before = everything())
  
  # Recurse
  if (is.null(static_totals)) {
    sim_option_2(augmented_scf, macro_projections, totals)
  } else {
    static_totals %>% 
      bind_rows(totals) %>% 
      return()
  }
}



calc_tax_option_2 = function(current_scf, year, macro_projections, static) {
  
  #----------------------------------------------------------------------------
  # Calculates withholding tax liability under option 2. Applies withholding tax
  # to positive taxable borrowing above exemption amount. Credits are tracked
  # separately and applied against future capital gains tax liability.
  # 
  # Parameters:
  #   - current_scf       (df)   : SCF+ data projected through given year
  #   - year              (int)  : year of tax calculation
  #   - macro_projections (lst)  : economic and demographic projections 
  #   - static            (bool) : whether running without avoidance
  #
  # Output: processed SCF with tax calculations
  #----------------------------------------------------------------------------
  
  # Define tax law parameters
  base_exemption = 250000
  withholding_rate = 0.10  # 10% withholding rate
  
  # Adjust parameters for inflation
  inflation_factor = macro_projections$economic %>%
    summarise(factor = ccpiu_irs[year == !!year] / ccpiu_irs[year == 2026]) %>%
    pull(factor)
  
  exemption = base_exemption * inflation_factor
  
  # Start with SCF... 
  current_scf %>% 
    
    # Do avoidance if non-static
    do_avoidance_option_2(year, exemption, macro_projections, static) %>%
    
    # Calculate tax
    mutate(
      year = !!year,
      
      # Subtract annual borrowing exemption
      borrowing_after_exemption = pmax(0, positive_taxable_borrowing - if_else(married == 1, exemption * 2, exemption)),
      
      # Calculate withholding tax (τ_w = 0.10)
      withholding_tax = borrowing_after_exemption * withholding_rate,
      
      # Calculate death tax adjustment
      basis_share = 1 - if_else(assets > 0, (kg_primary_home + kg_other_re + kg_pass_throughs + kg_other) / assets, 1)

    ) %>% 
    return()
}



do_avoidance_option_2 = function(current_scf, year, exemption, macro_projections, static) {
  
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
      tax  = calc_tax_option_2(current_scf, year, macro_projections, T)$withholding_tax, 
      tax1 = calc_tax_option_2(current_scf %>% mutate(positive_taxable_borrowing = positive_taxable_borrowing + 1), year, macro_projections, T)$withholding_tax,
      mtr  = tax1 - tax, 
      
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



age_option_2 = function(current_scf, target_year, macro_projections) {
  
  #----------------------------------------------------------------------------
  # Ages SCF data forward one year for option 2 simulation. Similar to option 1
  # but without micro/aggregate adjustment factors since we're not tracking
  # basis adjustments.
  # 
  # Parameters:
  #   - current_scf       (df)  : current year's wealth data
  #   - target_year      (int)  : year to age to
  #   - macro_projections (lst) : economic and demographic projections
  #
  # Output: wealth data aged to target year
  #----------------------------------------------------------------------------
  
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
  
  current_scf %>%
    # Join demographic factors
    left_join(demographic_factors, by = c('married', 'age')) %>%
    
    mutate(
      # Update weights for population growth
      weight = weight * demographic_factor,
      
      # Update all monetary variables with economic growth
      across(
        .cols = c(
          income, wages, kg_pass_throughs, kg_other, assets, 
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



calc_revenue_offset_option_2 = function(year_results, prob_hold_to_death = 0.8) {
  
  #----------------------------------------------------------------------------
  # Calculates schedule of revenue offsets, with a portion at death and the rest
  # spread evenly over remaining years.
  # 
  # Parameters:
  #   - year_results       (df) : record-level results from calc_tax_option_2
  #   - prob_hold_to_death (dbl): share of tax adjustment occurring at death
  #
  # Output: schedule of revenue offsets by year
  #----------------------------------------------------------------------------
  
  year_results %>%
    
    # Select taxpayers
    filter(withholding_tax > 0) %>%
    select(id, year, age, age_expected_death, withholding_tax, basis_share, borrowing_after_exemption, weight) %>%
    expand_grid(years_forward = 1:75) %>%  
    filter(age + years_forward <= age_expected_death) %>% 
    mutate(
      years_left = age_expected_death - age,
      
      # Calculate total tax adjustment on this year's borrowing
      tax_adjustment = borrowing_after_exemption * (1 - basis_share) * 0.238 - withholding_tax,
      
      # Spread non-death portion evenly over years
      revenue_offset_life = if_else(
        years_left > 0,
        tax_adjustment * (1 - prob_hold_to_death) / years_left,
        0
      ),
      
      # Add death portion in death year
      revenue_offset_death = if_else(
        year + years_forward == age_expected_death,
        tax_adjustment * prob_hold_to_death,
        0
      ),
      
      # Total revenue offset 
      revenue_offset = revenue_offset_life + revenue_offset_death
      
    ) %>%
    
    # Aggregate and return
    group_by(
      source_year = year,
      year = year + years_forward
    ) %>%
    summarise(
      across(
        .cols = starts_with('revenue_offset'), 
        .fns  = ~ sum(. * weight) / 1e9
      ),
      .groups = 'drop'
    ) %>%
    return()
}



get_totals_option_2 = function(year_results, year) {
  
  #----------------------------------------------------------------------------
  # Calculates aggregate statistics for a given simulation year for option 2.
  # 
  # Parameters:
  #   - year_results (df) : record-level results from calc_tax_option_2
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
      taxable_positive_net_borrowing = sum(positive_taxable_borrowing * weight) / 1e9,
      
      # Borrowing net of exemption
      taxable_borrowing_after_exemption = sum(borrowing_after_exemption * weight) / 1e9,
      
      # Number of taxpayers
      n_taxpayers = sum((withholding_tax > 0) * weight),
      share_taxpayers = weighted.mean(withholding_tax > 0, weight),
      
      # Withholding tax revenue in billions
      withholding_tax = sum(withholding_tax * weight) / 1e9
    )
}

