# Distribution Factor Chart
# Compare to the following https://personal.vanguard.com/us/insights/retirement/estimate-your-rmd-tool
#
# Returns a data frame w/ the balance, distribution and growth information over n years
# growth.perc can be either a single value with n set to the number of years or a vector 
# with n the same length as the growth.perc and decuctions vectors.  The actual deduction
# will be the max of the specified deduction or minimum distribution.
calc_rmd <- function(growth.perc = 5, n = 20, start.balance = 142743, deductions = 0) {
  convert_growth <- function(growth.perc) round((100 + growth.perc) / 100, 2)
  if (n <= 0) stop(warning("n cannot be less than or equal to 0"))
  if (n > 46) {
    warning("n cannot be greater than 46;  Setting to 46")
    n <- 46
  }
  if (length(deductions) == 1) deductions <- rep(deductions, n)
  if (length(growth.perc) == 1) growth.perc <- rep(growth.perc, n)
  growth <- sapply(growth.perc, convert_growth)
  balance <- start.balance
  x <- data.frame(Year = (2018:2063)[1:n], Age = (70:115)[1:n], Factor = c(27.4, 26.5, 25.6, 24.7, 23.8, 22.9, 22.0, 21.2,
                  20.3, 19.5, 18.7, 17.9, 17.1, 16.3, 15.5, 14.8, 14.1, 13.4, 12.7, 12.0, 11.4, 10.8, 10.2, 9.6, 9.1, 8.6, 
                  8.1, 7.6, 7.1, 6.7, 6.3, 5.9, 5.5, 5.2, 4.9, 4.5, 4.2, 3.9, 3.7, 3.4, 3.1, 2.9, 2.6, 2.4, 2.1, 1.9)[1:n],
                  Min.Dist = rep(-1, n), Balance = rep(-1, n),
                  Growth1 = growth.perc[1:n], Growth2 = growth[1:n])

  for (i in 1:n) {
    min.dist <- round(balance / x$Factor[i], 2)  # The RMD is based on the previous year's balance on 12/31
    balance <- balance * growth[i]
    balance <- balance - max(min.dist, deductions[i])
    x$Min.Dist[i] <- max(min.dist, deductions[i])
    x$Balance[i] <- round(balance)
  }

  return(x[1:n, ])
}

plot_rmd <- function(x) {
  par(mfrow = c(1,2))
  plot(x$Balance / 1000, type = 'b', pch = 19, col = 'grey', #ylim = c(0, max(x$Balance)),
       xlab = 'Year (n)', ylab = 'Balance ($1K)', main = 'IRA Balance')
  plot(x$Min.Dist, type = 'b', pch = 19, col = 'blue', #ylim = c(0, max(x$Balance)),
       xlab = 'Year (n)', ylab = 'Distribution ($)', main = 'Min Distribution')
  par(mfrow = c(1,1))
}

# Possible Scenarios
plot_rmd(print(calc_rmd(4)))   # 4% growth should leave the IRA account w/ 100K at the end of 20 years
plot_rmd(print(calc_rmd(6)))   # 6% growth should leave the IRA account slightly better off
plot_rmd(print(calc_rmd(8)))   # 8% growth should leave over 230K at the end of 20 years
plot_rmd(print(calc_rmd(10)))  # 10% growth should leave over 340K at the end of 20 years
                               # This level of growth causes the IRA to escape the RMD amount

# Bitcoin level volatility
plot_rmd(print(calc_rmd(growth.perc = runif(n = 46, min = -30, max = 50))))

# Historical stock market volatility
for (i in 1:10) {
  (x <- calc_rmd(growth.perc = round(runif(n = 46, min = -15, max = 15) + 5, 2), n = 46))
  plot_rmd(x)
}

# Historical stock market volatility w/ 100 iterations
x <- replicate(1000, calc_rmd(growth.perc = round(runif(n = 21, min = -15, max = 15) + 5, 2)))
x.end <- sapply(1:100, function(n) as.data.frame(x[ , n])$Balance[21])
summary(x.end)


# Comparing Fidelity to the annuity using the historical stock market volatility
calc_fidelity <- function(market.perc = 5, n = 20, start.balance = 142743, deductions = 0)
  calc_rmd(growth.perc = market.perc * .8, n = n, start.balance = start.balance, deductions = deductions)


calc_annuity <- function(market.perc = 5, n = 20, start.balance = 142743, deductions = 0) {
  growth.perc <- sapply(market.perc, function(m) ifelse(m <= 0, 0, min(m, 6)))
  calc_rmd(growth.perc = growth.perc, n = n, start.balance = start.balance, deductions = deductions)
}

plot_comparison <- function(x) {
  par(mfrow = c(1,3))
  with(x[[1]], plot(Balance / 1000, type = 'l', pch = 19, col = 'black', xlab = 'Year (n)', ylab = 'Balance ($1K)', main = 'IRA / Annuity Balance'))
  with(x[[2]], points(Balance / 1000, type = 'l', pch = 19, col = 'blue', xlab = 'Year (n)', ylab = 'Balance ($1K)', main = 'IRA / Annuity Balance'))
  with(x[[3]], points(Balance / 1000, type = 'l', pch = 19, col = 'red'))
  abline(h = 142, col = 'grey', lwd = 2)
  legend("topright", legend = c('Market', 'Fidelity', 'Midland National'), fill = c('black', 'blue', 'red'))
  with(x[[1]], plot(Min.Dist, type = 'l', pch = 19, col = 'black', xlab = 'Year (n)', ylab = 'Distribution ($)', main = 'Min Distribution'))
  with(x[[2]], points(Min.Dist, type = 'l', pch = 19, col = 'blue'))
  with(x[[3]], points(Min.Dist, type = 'l', pch = 19, col = 'red'))
  with(x[[1]], plot(cumsum(Min.Dist) / 1000, type = 'l', pch = 19, col = 'black', xlab = 'Year (n)', ylab = 'Distribution ($K)', main = 'Cumulative Distributions'))
  with(x[[2]], points(cumsum(Min.Dist) / 1000, type = 'l', pch = 19, col = 'blue'))
  with(x[[3]], points(cumsum(Min.Dist) / 1000, type = 'l', pch = 19, col = 'red'))
  par(mfrow = c(1,1))
}

for (i in 1:5) {
  n <- 30
  market.perc <- round(runif(n = n, min = -15, max = 15) + 5, 2)
  x <- list("market" = calc_rmd(growth.perc = market.perc, n = n),
            "fidelity" = calc_fidelity(market.perc = market.perc, n = n),
            "annuity" = calc_annuity(market.perc = market.perc, n = n))
  plot_comparison(x)
}


# Cash flow Analysis
inflow <-  rev(c(7667, 5713, 5359, 7909, 7988))
inflow2 <- rep(4400, 5)
outflow <- rev(c(2107+885+4502, 125+1341+3741, 289+1461+4218, 38+2509+4609, 318+3591+4237))

x <- rbind(outflow, inflow2, outflow - inflow2)
colnames(x) <- c('Oct', 'Nov', 'Dec', 'Jan', 'Feb')

