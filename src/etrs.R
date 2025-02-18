#------------------------------------------------------------------------------
# etrs.R
#
# Contains functions to calculate stylized ETRs on sales and borrowing under 
# different assumptions about financial conditions and tax law.
#------------------------------------------------------------------------------


calc_etr = function(df) {
  
  #----------------------------------------------------------------------------
  # Calculates the difference in effective tax rates (ETRs) between financing 
  # consumption via borrowing versus asset sales.  
  #
  # Parameters:
  #   - df: A tibble containing the following columns:
  #       - C       : Consumption in period 1 
  #       - B_share : share of consumption financed by borrowing
  #       - b       : Basis share of assets in period 1
  #       - tau_B   : Tax rate on deemed realization of borrowing
  #       - tau_W   : Withholding tax rate on borrowing
  #       - tau_S   : Capital gains tax rate on sales
  #       - tau_D   : Death tax rate on unrealized gains
  #       - tau_e   : Annual excise tax rate on debt
  #       - n       : Number of years until death
  #       - r       : Real asset growth rate
  #       - i       : Real interest rate on debt
  #       - pi      : Inflation rate
  #
  # Output: a tibble with the original columns plus:
  #       - B          : Borrowing required for given C
  #       - S          : Asset sales required for given C
  #       - T_B        : Tax on borrowing (deemed realization)
  #       - T_W        : Tax on borrowing (withholding)
  #       - T_D        : Tax adjustment for borrowing paid at time of death
  #       - V          : Final estate value
  #       - pv_V       : Present value of estate 
  #       - pv_V_notax : Present value of estate were there no taxes
  #       - ETR        : Effective tax rate
  #----------------------------------------------------------------------------
  
  df %>%
    mutate(
      
      # Calculate borrowing and sales required to finance given consumption
      B = b_share       * C / (1 - (1 - b) * tau_B - tau_W),
      S = (1 - b_share) * C / (1 - (1 - b) * tau_S),
      
      # Calculate taxes due at time of consumption
      T_B = B * (1 - b) * tau_B,
      T_W = B * tau_W,
      T_S = S * (1 - b) * tau_S,
      
      # Calculate assets and liabilities at death
      A = (1 - S) * exp((r + pi) * n),
      L = B * exp((i + pi + tau_e) * n),
      
      # Calculate taxes at death
      T_D = B * (1 - b) * tau_D - T_W,
      
      # Calculate value and present value  
      V    = A - L - T_D, 
      pv_V = V * exp(-(i + pi) * n),
      
      # Calculate ETR
      pv_V_notax = (1 - C * (1 - b_share)) * exp((r - i) * n) - (C * b_share),
      etr        = 1 - (C + pv_V) / (C + pv_V_notax)
      
    ) %>% 
    return()
}



calc_delta_etr = function(df) {
  
  #----------------------------------------------------------------------------
  # Calculates the difference in effective tax rates (ETRs) between financing 
  # consumption via borrowing versus asset sales.  
  #
  # Parameters:
  #   - df: A tibble containing all necessary input variables for `calc_etr()`
  #
  # Output: a tibble with the original columns plus:
  #       - V.borrow/sell    : Estate value when financing via borrowing
  #       - pv_V.borrow/sell : PV of estate value when financing via sales
  #       - delta_etr        : Difference in ETRs (sales minus borrowing)
  #----------------------------------------------------------------------------

  df %>%
    
    # Calculate output under borrowing scenario
    bind_cols(
      df %>% 
        mutate(b_share = 1) %>% 
        calc_etr() %>% 
        select(V, pv_V, etr) %>% 
        rename_with(~paste0(., '.borrow'))
    ) %>% 
    
    # Calculate output under sales scenario
    bind_cols(
      df %>% 
        mutate(b_share = 0) %>% 
        calc_etr() %>% 
        select(V, pv_V, etr) %>% 
        rename_with(~paste0(., '.sell'))
    ) %>% 
    
    # Calculate tax preference for borrowing (+ -> borrowing is preferred)
    mutate(
      borrowing_advantage = etr.sell - etr.borrow 
    ) %>% 
    return()
}


calc_comparative_advantage = function(config) {
  
  b  = as.numeric(config$b)
  n  = as.numeric(config$n)
  r  = as.numeric(config$r)
  i  = as.numeric(config$i)
  pi = as.numeric(config$pi)
  
  baseline = tibble(
    id = "baseline",
    C = 0.5,
    b = b,
    tau_B = 0,
    tau_W = 0,
    tau_S = 0.238,
    tau_D = 0,
    tau_e = 0,
    n = n,
    r = r,
    i  = i,
    pi = pi
  )
  
  option_1 = tibble(
    id = "Option 1",
    C = 0.5,
    b = b,
    tau_B = 0.238,
    tau_W = 0,
    tau_S = 0.238,
    tau_D = 0,
    tau_e = 0,
    n = n,
    r = r,
    i  = i,
    pi = pi
  )
  
  option_2 = tibble(
    id = "Option 2",
    C = 0.5,
    b = b,
    tau_B = 0,
    tau_W = 0.1,
    tau_S = 0.238,
    tau_D = 0.2,
    tau_e = 0,
    n = n,
    r = r,
    i  = i,
    pi = pi
  )
  
  option_3 = tibble(
    id = "Option 3",
    C = 0.5,
    b = b,
    tau_B = 0,
    tau_W = 0,
    tau_S = 0.238,
    tau_D = 0,
    tau_e = 0.01,
    n = n,
    r = r,
    i  = i,
    pi = pi
  )
  
  scenarios = bind_rows(baseline, option_1, option_2, option_3)
  
  1:4 %>%
    map(., 
        .f = ~ calc_delta_etr(scenarios[.x,]) %>%
               select(
                 id, etr.borrow, etr.sell, borrowing_advantage,
                 b, n, r, i, pi
               )
        ) %>%
    bind_rows() %>%
    return()
    
  
}


run_etr_scenarios = function() {
  base = c(
    config = "Baseline",
    b = .5,
    n = 10,
    r = .07,
    i = .04,
    pi = .02
  )
  
  basis = c(
    config = "Higher Basis",
    b = .75,
    n = 10,
    r = .07,
    i = .04,
    pi = .02
  )
  
  lower_return = c(
    config = "Lower Return",
    b = .5,
    n = 10,
    r = .02,
    i = .04,
    pi = .02
  )
  
  equal = c(
    config = "Equal RoR and Interest",
    b = .5,
    n = 10,
    r = .05,
    i = .05,
    pi = .02
  )
  
  inflation = c(
    config = "High Inflation",
    b = .5,
    n = 10,
    r = .07,
    i = .04,
    pi = .05
  )
  
  death = c(
    config = "Short Horizon",
    b = .5,
    n = 2,
    r = .07,
    i = .04,
    pi = .02
  )
  
  configs = bind_rows(base, basis, lower_return, equal, inflation, death)
  
  1:6 %>%
    map(., .f = ~ calc_comparative_advantage(configs[.x,]) %>%
          mutate(
            macro = configs[.x, "config"]
          )
    ) %>%
    bind_rows() %>%
    select(macro, !macro) %>%
    #arrange(borrowing_advantage) %>%
    return()
}

run_etr_scenarios() %>%
  print(n=24)



