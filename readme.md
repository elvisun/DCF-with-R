Introduction
============

This report outlines how the DCF function written in R works and shows a brief demonstration.

Usage
=====

Load the function as an external package, or copy and paste the function into your project, then call the function with the following parameters in the exact order:

    currentRevenue, currentCapitalSpending, currentDepreciation, taxRateIncome, bookValueDebt, bookValueEquity, nolCarriedForward, publiclyTradedFlag, lastTradedPrice, numberSharesOutstanding, marketValueDebt, debtCapitalRatio, costEquity, betaStock, riskFreeRate, riskPremium, costDebt, growthRateStablePeriod, operatingExpenseStablePeriod, workingCapitalStablePeriod,betaStablePeriod, debtRatioStablePeriod, costDebtFlag, costDebtStablePeriod,capitalExpendituresStablePeriod, growthRateRevenueRng,operatingExpenseRng, growthRateCapitalSpendingRng, growthRateDepreciationRng, workingCapitalRng,excessReturnPeriods = 10, capPeriods = 5, output = 1

Note that the variables `excessReturnPeriods`, `capPeriods`, `output` are optional parameters and have default values as shown.

Workflow
========

### Preprocessing

First, some of the parameters are passed as matrices. The function first verify its column dimension has a length of 1, if not, it would transpose the matrix. It then verifies the length of the matrix is the same with the `excessReturnPeriods` specified.

``` r
  if (nrow(growthRateRevenueRng) == 1){
    growthRateRevenueRng <- t(growthRateRevenueRng)
  }
  if (nrow(growthRateRevenueRng) != excessReturnPeriods){
    stop("Different Dimension Than Excess Return Periods")
  }
```

The function then calculates some neccessary values that are later used in the calculation.

### Calculation based on growth rate

The function uses a for loop that loops through the `excessReturnPeriods` and calculate the information needed every year:

``` r
  for(j in 1:excessReturnPeriods) {
    ifelse(j > 1, temp[2,j] <- temp[2,j-1], temp[2,j] <- currentRevenue * (1 + growthRateRevenueVector[j,1]))
    # ...
    if(j > 1) {
      temp[5,j] <- ifelse(temp[4,j] > 0, ifelse(temp[12,j-1] > temp[4,j], 0, (temp[4,j] - temp[12,j-1])) * taxRateIncome, 0)
      # ...
    }
    else {
      temp[5,j] <- ifelse(temp[4,j] > 0, ifelse(nolCarriedForward > temp[4,j], 0, (temp[4,j] - nolCarriedForward) * taxRateIncome), 0)
      # ...
    }
    ifelse(is.na(temp[28,1]), temp[28,1] <- temp[11,j], temp[28,1] <- temp[28,1] + temp[11,j])
  }
```

An if statement `if(j>1)` is used to determine the first year's values, then the subsequent values are determined by the growth rate input.

### Terminal value calculation

Finally, the terminal values for all items are calculated after the for loop using different logics.

``` r
  temp[2,excessReturnPeriods+1] <- temp[2,excessReturnPeriods] * (1+ growthRateStablePeriod)
  temp[3,excessReturnPeriods+1] <- temp[2,excessReturnPeriods+1] * operatingExpenseStablePeriod
  #...
```

Then the result is saved in the `intrinsicValueShare`

### Output

The output can be given in multiple formats, the intrinsic value per share as well as a more detailed output. This can be chosen by setting the value of the `output` parameter.

Example
=======

An example with the following parameter is included:

``` r
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
```

    #Result
    [1] 467.4989

Improvements
============

In the future, it might be worth it to add an interactive user interface for users to easily input data into a web form, and the calculation can be done easily from there. This can be implemented using the shiny package, and will be done for the project.


License
============
Copyright 2017 Elvis Sun

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
