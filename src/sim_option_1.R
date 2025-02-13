#------------------------------------------------------------------------------
# sim_option_1.R
# 
# Contains functions to calculate taxes and simulate years for option 1 
# (deemed realization for borrowing)
#------------------------------------------------------------------------------



sim_option_1 = function(augmented_scf, macro_projections, beta = 0.5) {
  
  #----------------------------------------------------------------------------
  # Projects 10-year revenue under option 1, accounting for reduction in future
  # unrealized gains using a mixture of record-level and aggregate adjustments.
  # 
  # Parameters:
  #   - augmented_scf      (df)  : SCF+ data projected through 2024
  #   - macro_projections  (lst) : economic and demographic projections
  #                                (see read_macro_projections)
  #   - beta               (dbl) : weight on record-level adjustment (0-1)
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
      age_option_1(year, macro_projections, micro_factors, aggregate_factor, beta)
    
    # Skip if before enactment
    if (year < 2026) next
    
    # Calculate record-level taxes
    year_results = calc_tax_option_1(current_scf, year, macro_projections)
    
    # Calculate and store aggregate statistics
    year_totals = get_totals_option_1(year_results, year)
    totals      = bind_rows(totals, year_totals) 
    
    # Calculate revenue offsets
    revenue_offset = bind_rows(
      revenue_offset, 
      calc_revenue_offset_option_1(year_results)
    )
    
    # Calculate record-level adjustments to capital gains based on deemed realization
    micro_factors = current_scf %>%
      mutate(
        micro_factor = 1 - year_results$deemed_realization / (kg_pass_throughs + kg_other), 
        micro_factor = if_else(kg_pass_throughs + kg_other <= 0, 1, micro_factor)
      ) %>% 
      pull(micro_factor)
    
    # Calculate aggregate adjustment factor for unrealized capital gains based on deemed realization 
    aggregate_factor = 1 - year_totals$deemed_realization / year_totals$unrealized_kg
  }
  
  # Calculate net revenue effect and return
  totals %>%
    left_join(
      revenue_offset %>%
        group_by(year) %>%
        summarise(revenue_offset = sum(would_be_tax)),
      by = 'year'
    ) %>%
    mutate(net_revenue = borrowing_tax - replace_na(revenue_offset, 0)) %>%
    return()
}



calc_tax_option_1 = function(projected_scf, year, macro_projections) {
  
  #----------------------------------------------------------------------------
  # Calculates tax liability under option 1 (deemed realization for borrowing).
  # Treats loan proceeds as a realization event for unrealized capital gains,
  # with an annual borrowing exemption. Tax applies to positive taxable
  # borrowing above exemption amount, up to total unrealized gains. Updates
  # basis after realization.
  # 
  # Parameters:
  #   - projected_scf      (df) : SCF+ data projected through given year
  #   - year              (int) : year of tax calculation
  #   - macro_projections (lst) : economic and demographic projections 
  #                               (see read_macro_projections)
  #
  # Output: processed 2022 SCF (df).
  #----------------------------------------------------------------------------
  
  # Define tax law parameters
  base_exemption = 250000
  tax_rate  = 0.238
  
  # Adjust parameters for inflation
  inflation_factor = macro_projections$economic %>%
    summarise(factor = ccpiu_irs[year == !!year] / ccpiu_irs[year == 2026]) %>%
    pull(factor)
  
  exemption = base_exemption * inflation_factor
  
  # Calculate tax and update basis
  projected_scf %>% 
    mutate(
      
      year = !!year,
      
      # Subtract annual borrowing exemption
      borrowing_after_exemption = pmax(0, positive_taxable_borrowing - if_else(married == 1, exemption * 2, exemption)),
      
      # Calculate deemed realization (limited by borrowing and available gains)
      deemed_realization = pmax(0, borrowing_after_exemption - pmax(0, kg_pass_throughs - kg_other)),
      
      # Calculate tax on deemed realization
      borrowing_tax = deemed_realization * tax_rate
      
    ) %>% 
    return()
}



age_option_1 = function(scf, target_year, macro_projections, micro_factors, 
                        aggregate_factor, beta = 0.5) {
  
  #----------------------------------------------------------------------------
  # Ages SCF data forward one year for option 1 simulation, applying both micro
  # and aggregate adjustments to unrealized gains.
  # 
  # Parameters:
  #   - current_scf       (df)  : current year's wealth data
  #   - target_year       (int) : year to age to
  #   - macro_projections (lst) : economic and demographic projections
  #   - micro_factors     (vec) : record-level adjustment factors for gains
  #   - aggregate_factor  (dbl) : aggregate adjustment factor for gains
  #   - beta              (dbl) : weight on micro vs aggregate adjustment (0-1)
  #
  # Output: wealth_tax data aged to target year.
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
  
  
  # Calculate combined adjustment factor for unrealized gains
  kg_adjustment_factor = beta * micro_factors + (1 - beta) * aggregate_factor
  
  
  scf %>%
    
    # Join demographic factors
    left_join(demographic_factors, by = c('married', 'age')) %>%
    
    mutate(
      
      # Update weights for population growth
      weight = weight * demographic_factor,
      
      # Update unrealized gains with combined micro/aggregate adjustment,
      # for gains that are actually realizable against the tax
      across(
        .cols = c(kg_pass_throughs, kg_other),
        .fns = ~ . * kg_adjustment_factor * economic_factor
      ),
      
      # Update all other monetary variables with economic growth
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



calc_revenue_offset_option_1 = function(year_results, prob_hold_to_death = 0.8) {
  
  #----------------------------------------------------------------------------
  # Calculates schedule of would-be tax payments for each record with positive
  # borrowing tax, based on age and assumed probability of holding to death.
  # 
  # Parameters:
  #   - year_results       (df) : record-level results from calc_tax_option_1
  #   - prob_hold_to_death (dbl): probability gains would be held until death
  #
  # Output: schedule of would-be tax payments by year
  #----------------------------------------------------------------------------
  
  year_results %>%
    
    # Expand to future years among taxpayers
    filter(borrowing_tax > 0) %>%
    select(id, age, age_expected_death, borrowing_tax, weight, year) %>%
    expand_grid(years_forward = 1:75) %>%  
    filter(age + years_forward <= age_expected_death) %>% 
    mutate(
      
      # For gains not held to death, spread tax evenly over remaining years
      years_left = age_expected_death - age,
      would_be_tax = if_else(
        years_left > 0, 
        borrowing_tax * (1 - prob_hold_to_death) / years_left,
        0 
      )
    ) %>%
    
    # Aggregate and return
    group_by(
      source_year = year, 
      year        = year + years_forward
    ) %>%
    summarise(
      would_be_tax = sum(would_be_tax * weight) / 1e9, 
      .groups = 'drop'
    ) %>% 
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
  # Output: dataframe with aggregated results.
  #----------------------------------------------------------------------------
  
  year_results %>%
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
      
      # Number of taxpayers
      n_taxpayers     = sum((borrowing_tax > 0) * weight),
      share_taxapyers = weighted.mean(borrowing_tax > 0, weight),
      
      # Tax revenue in billions
      borrowing_tax = sum(borrowing_tax * weight) / 1e9,

      # Share of eligible gains constructively realized
      realization_rate = deemed_realization / unrealized_kg
    )
}



