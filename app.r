# Elvis Sun
# 250696535
# Feb 8th

#function params
dcfFunc(108249, 7696, 1814, 0.242157579,
        0.1, 76615, 200000, TRUE, #8
        500, 929.3, 0.1, 0,       #12
        0.07, 1, 0.02, 0.05,      #16
        0.025, 0.03, 0.8165, 0.01,#20
        1, 0.15, TRUE, 0.08,      #24
        1.1,
        c(rep(0.06,7),rep(0.03,13)),
        c(rep(0.82,20)),
        c(0.02,rep(0.06,4),rep(0.03,15)),
        c(-0.07,rep(0.06,5),rep(0.03,14)),
        c(rep(0.05,20)),
        20,5,1
)


dcfFunc <- function(
  currentRevenue, currentCapitalSpending, currentDepreciation, taxRateIncome,
  bookValueDebt, bookValueEquity, nolCarriedForward, publiclyTradedFlag,     #8
  lastTradedPrice, numberSharesOutstanding, marketValueDebt, debtCapitalRatio,
  costEquity, betaStock, riskFreeRate, riskPremium,   #16
  costDebt, growthRateStablePeriod, operatingExpenseStablePeriod, workingCapitalStablePeriod,   #20
  betaStablePeriod, debtRatioStablePeriod, costDebtFlag, costDebtStablePeriod,   #24
  capitalExpendituresStablePeriod, growthRateRevenueRng, #26
  operatingExpenseRng, growthRateCapitalSpendingRng, 
  growthRateDepreciationRng, workingCapitalRng,  #30
  excessReturnPeriods = 10, capPeriods = 5, output = 1
){
  
  #transpose if horizontal, and raise an error 
  if (nrow(growthRateRevenueRng) == 1){
    growthRateRevenueRng <- t(growthRateRevenueRng)
  }
  if (nrow(growthRateRevenueRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
  
  if (nrow(operatingExpenseRng) == 1){
    operatingExpenseRng <- t(operatingExpenseRng)
  }
  if (nrow(operatingExpenseRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
  
  
  if (nrow(growthRateCapitalSpendingRng) == 1){
    growthRateCapitalSpendingRng <- t(growthRateCapitalSpendingRng)
  }
  if (nrow(growthRateCapitalSpendingRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
  
  
  if (nrow(growthRateDepreciationRng) == 1){
    growthRateDepreciationRng <- t(growthRateDepreciationRng)
  }
  if (nrow(growthRateDepreciationRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
  
  
  if (nrow(workingCapitalRng) == 1){
    workingCapitalRng <- t(workingCapitalRng)
  }
  if (nrow(workingCapitalRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
  
  #rename variables
  growthRateRevenueVector <- growthRateRevenueRng
  operatingExpenseVector <- operatingExpenseRng
  growthRateCapitalSpendingVector <- growthRateCapitalSpendingRng
  growthRateDepreciationVector <- growthRateDepreciationRng
  workingCapitalVector <- workingCapitalRng
  
  #if else to calculate some constant
  costEquityVal <- riskFreeRate + betaStock * riskPremium
  if (costEquity != 0 ){
    costEquityVal <- costEquity
  }
  
  temp <- 1 - debtCapitalRatio
  if (debtCapitalRatio == 0){
    temp <- 1 - bookValueDebt
  }
  equityDebtRatio <- temp
  if (publiclyTradedFlag) {
    equityDebtRatio <- lastTradedPrice * numberSharesOutstanding / (marketValueDebt + lastTradedPrice * numberSharesOutstanding)
  }
  
  #calculate more constant
  afterTaxCostDebt <- costDebt * (1 - taxRateIncome)
  debtCapitalVal <- 1 - equityDebtRatio
  costCapitalVal <- costEquityVal * equityDebtRatio + afterTaxCostDebt * debtCapitalVal
  
  #make matrix
  temp <- matrix(c(), nrow = 33, ncol = excessReturnPeriods)
  
  
  for(j in 1:excessReturnPeriods) {
    temp[1,j] <- j
    
    ifelse(j > 1, temp[2,j] <- temp[2,j-1], temp[2,j] <= currentRevenue * (1 + growthRateRevenueVector[j,1]))
    temp[3,j] <- temp[2,j] * operatingExpenseVector[j,1]
    temp[4,j] <- temp[2,j] - temp[3,j]
    
    if(j > 1) {
      temp[5,j] <- ifelse(temp[4,j] > 0, ifelse(temp[12,j-1] > temp[4,j], 0, (temp[4,j] - temp[12,j-1])) * TAX_RATE_INCOME, 0)
      temp[7,j] <- temp[7,j-1] * (1 + growthRateDepreciationVector[j,1])
      temp[8,j] <- temp[8,j-1] * (1 + growthRateCapitalSpendingVector[j,1])
      temp[9,j] <- (temp[2,j] - temp [2,j-1]) * workingCapitalVector[j,1]
      temp[12,j] <- ifelse(temp[12,j-1] > temp[4,j], temp[12,j-1] - temp[4,j], 0)
    }
    else {
      temp[5,j] <- ifelse(temp[4,j] > 0, ifelse(nolCarriedForward > temp[4,j], 0, (temp[4,j] - nolCarriedForward) * taxRateIncome), 0)
      temp[7,j] <- currentDepreciation * (1 + growthRateDepreciationVector[j,1])
      temp[8,j] <- currentCapitalSpending * (1 + growthRateCapitalSpendingVector[j,1])
      temp[9,j] <- (temp[2,j] - currentRevenue) * workingCapitalVector[j,1]
      temp[12,j] <- ifelse(nolCarriedForward > 0, ifelse(nolCarriedForward < temp[2,j], 0, nolCarriedForward - temp[2,j]), 0)
    }
    
    if(j <= capPeriods) {
      temp[14,j] <- betaStock
      temp[17,j] <- debtCapitalVal
    }
    else {
      temp[14,j] <- ifelse(betaStablePeriod != 0, temp[14,capPeriods] - ((temp[14,capPeriods] - betaStablePeriod) / capPeriods) * (j - capPeriods), betaStock)
    }
  }
  
  
  
  
  
}


