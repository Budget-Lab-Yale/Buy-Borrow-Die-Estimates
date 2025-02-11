#------------------------------------------------------------------------------
# calc_etrs.R
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
      T_S = 0,
      
      # Calculate assets and liabilities at death
      A = (1 - S) * exp((r + pi) * n),
      L = B * exp((i + pi + tau_e) * n),
      
      # Calculate taxes at death
      T_D = B * (1 - b) * tau_D - T_B - T_W,
      
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
      borrowing_advatage = etr.sell - etr.borrow 
    ) %>% 
    return()
}




#tibble(
#   C = 0.5,
#   b_share = 0,
#   b = 0.5,
#   tau_B = 0,
#   tau_W = 0,
#   tau_S = 0.238,
#   tau_D = 0,
#   tau_e = 0,
#   n = 10,
#   r = 0.07,
#   i  = 0.03,
#   pi = 0.02
# ) %>%
#   calc_delta_etr() %>% 
#   pivot_longer(everything()) %>% 
#   print(n = 50)
# 

