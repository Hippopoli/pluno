## Fix for NOTEs from R CMD check
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("bankWACD", "fedFundsRate", "CPA", "PSF", "pbad", "pprepay", "attributedEquity",
                           "originationsExpense", "servicingExpense", "collectionsExpense",
                           "variableTechExpense", "fixedExpenses", "recoveryRate", "taxRate", "hurdleRate",
                           "assumptionsCOF", "assumptionsOthers", "assumptionsPSF", "assumptionsPrepayRates",
                           "assumptionsLossRates", "assumptionsCPA"))
}

#' @title summarize by year, all revenue and loss drivers and cash flows
#'
#' @description Outputs are needed for financial planning
#' @param loanAmt Loan amount as a numeric number greater than 0
#' @param APR value is a numeric number between 0 and 1
#' @param tenor Length of the loan and can only be 24 36 48 60 or 72
#' @param FICO Customer credit profile can only be 660-700 700-740 or 740-850
#' @param channel Customer acquisition channel a string can only be PaidSearch or Affiliates or DirectMail or Email or Homesite
#' @return dataframe
#' @export

annualSummary <- function(loanAmt = NULL, APR = NULL, tenor = NULL, FICO = NULL, channel = NULL) {
  if(APR < 0 | APR > 1 | is.null(APR) | (!(is.numeric(APR))))
    stop("Error: APR input should be a numeric number between 0 and 1")
  
  if(loanAmt <= 0 | is.null(loanAmt) | (!(is.numeric(loanAmt))))
    stop("Error: loanAmt should be a numeric number greater than 0")
  
  if(!(FICO %in% c("660-700","700-740","740-850")) | is.null(FICO) | (!(is.character(FICO))))
    stop("Error: FICO should be a string, and can be one of these three '660-700', '700-740', '740-850' ")
  
  if(!(channel %in% c("PaidSearch", "Affiliates", "DirectMail", "Email", "Homesite")) | is.null(channel) | (!(is.character(channel))))
    stop("Error: channel should be a string, and can be one of these five 'PaidSearch', 'Affiliates', 
         'DirectMail','Email','Homesite' ")
  
  if(!(tenor %in% c(24,36,48,60,72)) | is.null(tenor) | (!(is.numeric(tenor))))
    stop("Error: tenor should be a integer, and can be 24, 36, 48, 60 or 72")
  
  cashFlowDF <- cashFlowAllDrivers(loanAmt, APR, tenor, FICO, channel)
  year       <- c(1:(tenor/12))
  # initiate outputs
  annualAverageEndBal    <- list()
  annualAverageBegBal    <- list()
  annualAverageBal       <- list()
  annualGrossRevenue     <- list()
  annualInterestExpenses <- list()
  annualNetLoss          <- list()
  annualVariableExpenses <- list()
  annualContrMargin      <- list()
  annualCashFlow         <- list()
  annualAfterTaxCF       <- list()
  annualFixedExpenses    <- list()
  annualPreTaxIncome     <- list()
  annualPreTaxPVexlCPA   <- list()
  annualPreTaxPVexlCPA   <- list()
  annualAfterTaxNPV      <- list()
  # loop through year  
  for (i in year){
    p = 1+12 * (i-1)
    q = 12 * i
    annualAverageEndBal[i] <- round(as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$endBal, na.rm = TRUE)/12)), 1)
    annualAverageBegBal[i] <- round(as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$begBal, na.rm = TRUE)/12)), 1)
    annualAverageBal[i]    <- round((as.numeric(unlist(annualAverageBegBal[i])) + as.numeric(unlist(annualAverageEndBal[i])))/2, 1)
    annualGrossRevenue[i] <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$ipmt, na.rm = TRUE)))
    annualInterestExpenses[i] <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$fixedIE, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$floatIE, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$rebatedIE, na.rm = TRUE)))
    annualNetLoss[i] <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$grossCO, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$recoveries,na.rm = TRUE)))-
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$reserveD, na.rm = TRUE)))
    annualVariableExpenses[i] <- as.numeric(unlist(sum(cashFlowDF[c(p:q),]$origEX, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$servEX, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$collEX, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$techEX, na.rm = TRUE))) +
      as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$acqsEX, na.rm = TRUE)))
    annualContrMargin[i] <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$contrMargin, na.rm = TRUE)))
    annualCashFlow[i]    <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$contrMarginWoRes, na.rm = TRUE)))
    annualAfterTaxCF[i]  <- round(as.numeric(unlist(annualCashFlow[i])) * (1 - assumptionsOthers$taxRate), 1)
    annualFixedExpenses[i]  <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$fixedEX, na.rm = TRUE)))
    annualPreTaxIncome[i]   <- as.numeric(unlist(sum(cashFlowDF[c(p:q), ]$preTaxIncome, na.rm = TRUE)))
    annualPreTaxPVexlCPA[i] <- round((as.numeric(unlist(annualCashFlow[i])) - cashFlowDF[i,]$acqsEX)/((1 + assumptionsOthers$hurdleRate)^i), 1)
    annualAfterTaxNPV[i]    <- round(as.numeric(unlist(annualAfterTaxCF[i]))/((1 + assumptionsOthers$hurdleRate)^(i-1)), 1)
  } # end of looping over years
  
  # assemble annual pieces into data frame
  annualSummaryDF <- data.frame(as.numeric(year), as.numeric(unlist(annualAverageBal)), as.numeric(unlist(annualAverageBegBal)),
                                as.numeric(unlist(annualAverageEndBal)), as.numeric(unlist(annualGrossRevenue)),
                                as.numeric(unlist(annualInterestExpenses)), as.numeric(unlist(annualNetLoss)),
                                as.numeric(unlist(annualVariableExpenses)), as.numeric(unlist(annualContrMargin)),
                                as.numeric(unlist(annualCashFlow)), as.numeric(unlist(annualAfterTaxCF)),
                                as.numeric(unlist(annualFixedExpenses)), as.numeric(unlist(annualPreTaxIncome)),
                                as.numeric(unlist(annualPreTaxPVexlCPA)), as.numeric(unlist(annualAfterTaxNPV)))
  names(annualSummaryDF) <- c("year","annualAverageBal", "annualAverageBegBal", "annualAverageEndBal",
                              "annualGrossRevenue", "annualInterestExpenses", "annualNetLoss", 
                              "annualVariableExpenses","annualContrMargin", "annualCashFlow", "annualAfterTaxCF" ,
                              "annualFixedExpenses", "annualPreTaxIncome", "annualPreTaxPVexlCPA",
                              "annualAfterTaxNPV")
  return(annualSummaryDF)
}