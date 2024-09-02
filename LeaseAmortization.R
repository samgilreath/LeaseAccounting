# Input Parameters
lease_term <- 5  # Lease term in years
annual_discount_rate <- 6  # Annual discount rate in percent
monthly_payment <- 2000  # Monthly lease payment
start_date <- as.Date("2024-09-01")  # Lease start date

# Calculations
monthly_rate <- (annual_discount_rate / 100) / 12
n_payments <- lease_term * 12

# Calculate the lease liability (Present Value of Lease Payments)
lease_liability <- monthly_payment * ((1 - (1 + monthly_rate)^(-n_payments)) / monthly_rate)

# ROU Asset initially equals lease liability
ROU_asset <- lease_liability

# Initialize vectors for storing schedule details
schedule <- data.frame(
  Month = seq.Date(from = start_date, by = "month", length.out = n_payments),
  Payment = 1:n_payments,
  LeasePayment = rep(monthly_payment, n_payments),
  InterestExpense = numeric(n_payments),
  PrincipalRepayment = numeric(n_payments),
  LeaseLiability = numeric(n_payments),
  ROUAmortization = numeric(n_payments),
  ROUAsset = numeric(n_payments)
)

# Initial balances
balance <- lease_liability
ROU_balance <- ROU_asset

# Populate the schedule
for (i in 1:n_payments) {
  interest_expense <- balance * monthly_rate
  principal_repayment <- monthly_payment - interest_expense
  ROU_amortization <- ROU_asset / n_payments
  
  balance <- balance - principal_repayment
  ROU_balance <- ROU_balance - ROU_amortization
  
  schedule$InterestExpense[i] <- round(interest_expense, 2)
  schedule$PrincipalRepayment[i] <- round(principal_repayment, 2)
  schedule$LeaseLiability[i] <- round(balance, 2)
  schedule$ROUAmortization[i] <- round(ROU_amortization, 2)
  schedule$ROUAsset[i] <- round(ROU_balance, 2)
}

# Write the schedule to a csv file
write.csv(schedule,file='schedule.csv',quote=FALSE)

# Explanation:
# Lease Liability Calculation: The present value of future lease payments is computed using the monthly discount rate and the total number of payments.
# Interest Expense: This is the interest on the remaining lease liability.
# Principal Repayment: The portion of the lease payment that reduces the lease liability.
# ROU Asset: The right-of-use asset is amortized on a straight-line basis over the lease term.
# ROU Amortization: The amortization of the ROU asset is evenly spread across the lease term.

# Output:
# Month: The date of each payment.
# Payment: Payment number.
# LeasePayment: The fixed monthly lease payment.
# InterestExpense: The interest portion of the lease payment.
# PrincipalRepayment: The portion of the payment that reduces the lease liability.
# LeaseLiability: The remaining lease liability after each payment.
# ROUAmortization: The amortization of the right-of-use asset.
# ROUAsset: The remaining right-of-use asset after each period.
