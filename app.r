# Elvis Sun
# 250696535
# Feb 8th

#function params



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
  temp <- matrix(, nrow = 33, ncol = excessReturnPeriods + 1)
  
  
  for(j in 1:excessReturnPeriods) {
    temp[1,j] <- j
    
    ifelse(j > 1, temp[2,j] <- temp[2,j-1], temp[2,j] <- currentRevenue * (1 + growthRateRevenueVector[j,1]))
    temp[3,j] <- temp[2,j] * operatingExpenseVector[j,1]
    temp[4,j] <- temp[2,j] - temp[3,j]
    
    if(j > 1) {
      temp[5,j] <- ifelse(temp[4,j] > 0, ifelse(temp[12,j-1] > temp[4,j], 0, (temp[4,j] - temp[12,j-1])) * taxRateIncome, 0)
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
    
    
    temp[6,j] <- temp[4,j] - temp[5,j]
    temp[10,j] <- temp[6,j] + temp[7,j] + temp[8,j] - temp[9,j]
    temp[13,j] <- temp[5,j] / temp[4,j]
    
    if(j <= capPeriods) {
      temp[14,j] <- betaStock
      temp[17,j] <- debtCapitalVal
    }
    else {
      temp[14,j] <- ifelse(betaStablePeriod != 0, temp[14,capPeriods] - ((temp[14,capPeriods] - betaStablePeriod) / capPeriods) * (j - capPeriods), betaStock)
      temp[17,j] <- ifelse(debtRatioStablePeriod != 0, temp[17,capPeriods] - ((temp[17,capPeriods] - debtCapitalVal) / capPeriods) * (j - capPeriods), debtCapitalVal)
    }
    
    temp[15,j] <- ifelse(costEquity == 0, riskFreeRate + temp[14,j] * riskPremium, costEquity)
    temp[16,j] <- costDebtStablePeriod * (1 - taxRateIncome)
    temp[18,j] <- temp[15,j] * (1 - temp[17,j]) + temp[16,j] * temp[17,j]
    ifelse(j > 1, temp[19,j] <- temp[19,j-1] * (1 + temp[18,j]), temp[19,j] <- 1 + temp[18,j])
    temp[11,j] <- temp[10,j] / temp[19,j]
    ifelse(is.na(temp[28,1]), temp[28,1] <- temp[11,j], temp[28,1] <- temp[28,1] + temp[11,j])
  }
  
  
  
  #temp[1,excessReturnPeriods+1] <- "TN"
  temp[2,excessReturnPeriods+1] <- temp[2,excessReturnPeriods] * (1+ growthRateStablePeriod)
  temp[3,excessReturnPeriods+1] <- temp[2,excessReturnPeriods+1] * operatingExpenseStablePeriod
  temp[4,excessReturnPeriods+1] <- temp[2,excessReturnPeriods+1] - temp[3,excessReturnPeriods+1]
  temp[5,excessReturnPeriods+1] <- ifelse(temp[4,excessReturnPeriods+1] > 0, ifelse(temp[12,excessReturnPeriods] > temp[4,excessReturnPeriods+1], 0, (temp[4,excessReturnPeriods+1] - temp[12,excessReturnPeriods]) * taxRateIncome), 0)
  temp[6,excessReturnPeriods+1] <- temp[4,excessReturnPeriods+1] - temp[5,excessReturnPeriods+1]
  temp[7,excessReturnPeriods+1] <- temp[7,excessReturnPeriods]
  temp[8,excessReturnPeriods+1] <- ifelse(capitalExpendituresStablePeriod == 0, temp[7,excessReturnPeriods+1], temp[7,excessReturnPeriods+1] * capitalExpendituresStablePeriod)
  temp[9,excessReturnPeriods+1] <- (temp[2,excessReturnPeriods+1] - temp[2,excessReturnPeriods]) * workingCapitalStablePeriod
  temp[10,excessReturnPeriods+1] <- temp[6,excessReturnPeriods+1] + temp[7,excessReturnPeriods+1] - temp[8,excessReturnPeriods+1] - temp[9,excessReturnPeriods+1]
  temp[13,excessReturnPeriods+1] <- temp[5,excessReturnPeriods+1] / temp[4,excessReturnPeriods+1]
  temp[14,excessReturnPeriods+1] <- temp[14,excessReturnPeriods]
  temp[15,excessReturnPeriods+1] <- ifelse(costEquity == 0, riskFreeRate + temp[14,excessReturnPeriods+1] * riskPremium, costEquity)
  temp[16,excessReturnPeriods+1] <- temp[16,excessReturnPeriods]
  temp[17,excessReturnPeriods+1] <- temp[17,excessReturnPeriods]
  temp[18,excessReturnPeriods+1] <- temp[15,excessReturnPeriods+1] * (1 - temp[17,excessReturnPeriods+1]) + temp[16,excessReturnPeriods+1] * temp[17,excessReturnPeriods+1]
  temp[20,1] <- growthRateStablePeriod
  temp[21,1] <-temp[10,excessReturnPeriods+1]
  temp[22,1] <- ifelse(betaStablePeriod == 0, riskFreeRate + betaStock * riskPremium, riskFreeRate + betaStablePeriod * riskPremium)
  temp[23,1] <- ifelse(debtRatioStablePeriod != 0, 1 - debtRatioStablePeriod, equityDebtRatio)
  temp[24,1] <- ifelse(costDebtFlag == TRUE, costDebtStablePeriod * (1 - taxRateIncome), afterTaxCostDebt)
  temp[25,1] <- 1 - temp[23,1]
  temp[26,1] <- temp[22,1] * temp[23,1] + temp[24,1] * temp[25,1]
  temp[27,1] <- temp[21,1] / (temp[26,1] - temp[20,1])
  temp[29,1] <- temp[27,1] / temp[19,excessReturnPeriods]
  temp[30,1] <- temp[28,1] + temp[29,1]
  temp[31,1] <- ifelse(publiclyTradedFlag == TRUE, marketValueDebt, bookValueDebt)
  temp[32,1] <- temp[30,1] - temp[31,1]
  temp[33,1] <- temp[32,1] / numberSharesOutstanding
  
  intrinsicValueShare <- temp[33,1]

  
  if(output == 0) {
    intrinsicValueShare
  }
  else if(output == 1) {
    temp
  }
}


dcfFunc(108249, 7696, 1814, 0.242157579,
        0.1, 76615, 200000, TRUE, #8
        500, 929.3, 0.1, 0,       #12
        0.07, 1, 0.02, 0.05,      #16
        0.025, 0.03, 0.8165, 0.01,#20
        1, 0.15, TRUE, 0.08,      #24
        1.1,
        matrix(c(rep(0.06,7),rep(0.03,13)),ncol=1),
        matrix(c(rep(0.82,20)),ncol=1),
        matrix(c(0.02,rep(0.06,4),rep(0.03,15)),ncol=1),
        matrix(c(-0.07,rep(0.06,5),rep(0.03,14)),ncol=1),
        matrix(c(rep(0.05,20)),ncol=1),
        20,5,0
)
