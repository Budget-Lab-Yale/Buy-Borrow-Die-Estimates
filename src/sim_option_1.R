#------------------------------------------------------------------------------
# sim_option_1.R
# 
# Contains functions to calculate taxes and simulate years for option 1 
# (deemed realization for borrowing)
#------------------------------------------------------------------------------



sim_option_1 = function(projected_scf, macro_projections, beta = 0.5) {
  
  #----------------------------------------------------------------------------
  # Projects 10-year revenue under option 1, accounting for reduction in future
  # unrealized gains using a mixture of record-level and aggregate adjustments.
  # 
  # Parameters:
  #   - projected_scf      (df)  : SCF+ data projected through start year
  #   - macro_projections  (lst) : economic and demographic projections
  #                                (see read_macro_projections)
  #   - beta               (dbl) : weight on record-level adjustment (0-1)
  #
  # Output: TODO
  #----------------------------------------------------------------------------
  
  # Initialize tracking objects
  totals = tibble()
  current_scf = projected_scf
  
  # Initialize unrealized gain adjustment factors to 1 (pre-enactment) 
  micro_factors    = rep(1, nrow(current_scf))
  aggregate_factor = 1 
  
  # For each projection year
  for (year in 2025:max(budget_window)) {
    
    # Age data forward to this year
    current_scf = current_scf %>% 
      age_option_1(year, macro_projections, micro_factors, aggregate_factor, beta)
    
    # Skip if before enactment
    if (year < 2026) {
      next
    }
    
    # Calculate record-level taxes
    year_results = calc_tax_option_1(current_scf, year, macro_projections)
    
    # Calculate and store aggregate statistics
    year_totals = get_totals_option_1(year_results, year)
    totals      = bind_rows(totals, year_totals) 
    
    # Calculate record-level adjustments to capital gains based on deemed realization
    micro_factors = current_scf %>%
      mutate(
        micro_factor = 1 - year_results$deemed_realization / (kg_pass_throughs + kg_other), 
        micro_factor = if_else(kg_pass_throughs + kg_other <= 0, 1, micro_factor)
      ) %>% 
      pull(micro_factor)
    
    # Calculate aggregate adjustment factor for unrealized capital gains based on deemed realization 
    aggregate_factor = 1 - year_totals$total_realization / year_totals$kg
  }
  
  return(totals)
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
  exemption = 500000
  tax_rate  = 0.238
  
  # Adjust parameters for inflation
  inflation_factor = macro_projections$economic %>%
    filter(year %in% c(2026, !!year)) %>%
    summarise(factor = last(ccpiu_irs) / first(ccpiu_irs)) %>%
    pull(factor)
  
  exemption = exemption * inflation_factor
  
  # Calculate tax and update basis
  projected_scf %>% 
    mutate(
      
      # Subtract annual borrowing exemption
      borrowing_after_exemption = pmax(0, positive_taxable_borrowing - exemption),
      
      # Calculate deemed realization (limited by borrowing and available gains)
      deemed_realization = pmin(borrowing_after_exemption, kg_pass_throughs + kg_other),
      
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
  demographic_growth = macro_projections$demographic %>% 
    filter(year %in% c(target_year - 1, target_year)) %>%
    mutate(year = if_else(year == target_year, 'target_year', 'base_year')) %>% 
    pivot_wider(
      names_from  = year, 
      values_from = population
    ) %>% 
    mutate(demographic_factor = target_year / base_year) %>% 
    select(married, age, demographic_factor)
  
  # Calculate per-capita economic growth rate
  economic_growth = macro_projections$economic %>% 
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
  
  # Age the SCF forward
  scf %>%
    # Join demographic factors
    left_join(demographic_growth, by = c('married', 'age')) %>%
    
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



get_totals_option_1 = function(year_results, year) {
  
  #----------------------------------------------------------------------------
  # Calculates aggregate statistics for a given simulation year for option 1.
  # 
  # Parameters:
  #   - year_results (df) : record-level results from calc_option_1_tax
  #   - year        (int) : current simulation year
  #
  # Output: Data frame with aggregated results
  #----------------------------------------------------------------------------
  
  year_results %>%
    summarise(
      year = !!year,
      
      # Total unrealized gains
      kg = sum((kg_pass_throughs + kg_other) * weight),
      
      # Total deemed realization
      deemed_realization = sum(deemed_realization * weight),
      
      # Tax revenue in billions
      tax = sum(borrowing_tax * weight) / 1e9,
      
      # Effective tax rate (tax/gains)
      effective_rate = tax / kg,
      
      # Share of gains realized
      realization_rate = deemed_realization / kg
    )
}
