# Function to compute life table
# Returns standard columns: age, n, mx, qx, px, lx, dx, Lx, Sx, Tx, ex
# mx: Age-specific mortality rate
# age: Start age of the interval
# ax: Fraction of interval lived by those who die (default 0.5)
lifetable <- function(mx, age, ax = 0.5){
  
  # Ensure sorted by age
  ord <- order(age)
  mx <- mx[ord]
  age <- age[ord]
  
  # Interval length
  n <- c(diff(age), Inf) # Last is Inf (open interval) or assumed closed if mx provided?
  # If we are doing single age up to 100, usually last is 100+ (Inf)
  # If the user provides mx for 0..100, n is 1 for all except last.
  
  # Probability of dying (qx)
  # qx = n*mx / (1 + n*(1-ax)*mx)
  # For last interval (Inf), qx = 1
  qx <- (n * mx) / (1 + n * (1 - ax) * mx)
  qx[length(qx)] <- 1
  
  # Probability of survival (px)
  px <- 1 - qx
  
  # Survivors (lx) at exact age x
  lx <- vector("numeric", length(qx))
  lx[1] <- 100000
  for(i in 2:length(qx)){
    lx[i] <- lx[i-1] * px[i-1]
  }
  
  # Deaths (dx) (in the interval)
  dx <- lx * qx
  
  # Person-years lived (Lx) in the interval
  Lx <- vector("numeric", length(qx))
  
  # For closed intervals:
  # Lx = n * lx - n * (1 - ax) * dx
  # This relies on the assumption of linearity/ax separation of deaths
  # n is numeric, but last is Inf.
  
  for(i in 1:(length(qx)-1)){
    Lx[i] <- n[i] * (lx[i] - (1 - ax) * dx[i])
  }
  
  # Last open interval (Open-ended)
  # assumption: constant force of mortality or Lx = lx / mx
  # If mx is 0, we have a problem (immortality), but usually mx > 0.
  Lx[length(qx)] <- ifelse(mx[length(qx)] > 0, lx[length(qx)] / mx[length(qx)], 0)
  
  # Total person-years (Tx) above age x
  Tx <- rev(cumsum(rev(Lx)))
  
  # Life expectancy (ex)
  ex <- Tx / lx
  ex[is.nan(ex)] <- 0
  
  # Survival Ratio (Sx)
  # Probability of surviving from age group x to x+n (Projected survival)
  # Sx = L_{x+n} / L_x
  # For the last closed group (becoming the open group): S_{last-1} = T_{last} / T_{last-1} ?
  # Or L_{last} / L_{last-1} if we map group to group.
  # Let's use L_{x+1}/L_{x} logic (shifting vector).
  Sx <- vector("numeric", length(qx))
  Sx_ratio <- c(Lx[-1], NA) / Lx
  
  # Handling the end is tricky and method-dependent.
  # Usually: Sx = L_{x+n}/L_x. 
  # For last *closed* group (going to open): S = T_{open} / (T_{open} + L_{last_closed})?
  # Let's provide L_{next}/L_{current} and leave last as NA or specific logic.
  # For single age: L_{x+1}/L_x.
  # Last group is open, so Sx is typically T_{x}/T_{x}. No meaning.
  # Let's fill with Ratio.
  Sx[1:(length(Sx)-1)] <- Lx[2:length(Lx)] / Lx[1:(length(Lx)-1)]
  
  # Fix for survival into the open interval (Penultimate -> Last)
  # S_{x} -> {x+n+} = T_{x+n} / T_{x}? No.
  # Let's stick to Lx+1 / Lx. The user can interpret or we use T approximation if needed.
  # Actually, for projection matrices (Leslie), usually:
  # S = L_{x+n} / L_x (for intermediate)
  # S_{last_closed} = L_{open} / L_{last_closed} (if expanding?)
  # The most common for single year projection is L_{x+1}/L_x.
  
  # Adjust ax notation for output if ax was scalar
  ax_out <- rep(ax, length(mx))
  
  return(data.table(age=age, n=n, mx=mx, qx=qx, px=px, lx=lx, dx=dx, Lx=Lx, Sx=Sx, Tx=Tx, ex=ex, ax=ax_out))
}
