## Fix for NOTEs from R CMD check
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("bankWACD", "fedFundsRate", "CPA", "PSF", "pbad", "pprepay", "attributedEquity", 
                           "originationsExpense", "servicingExpense", "collectionsExpense",
                           "variableTechExpense", "fixedExpenses", "recoveryRate", "taxRate", "hurdleRate",
                           "assumptionsCOF", "assumptionsOthers", "assumptionsPSF", "assumptionsPrepayRates",
                           "assumptionsLossRates", "assumptionsCPA"))
}

#' @title Over All Summary of a person loan investment
#'
#' @description Summarize revenue loss and cash flow metrics of a loan origination
#' @param loanAmt Loan amount as a numeric number greater than 0
#' @param APR value is a numeric number between 0 and 1
#' @param tenor Length of the loan, a numberic number and can only be one of these 24, 36, 48, 60, 70
#' @param FICO Customer credit profile can only be 660-700 700-740 or 740-850
#' @param channel Customer acquisition channel a string can only be PaidSearch or Affiliates or DirectMail or Email or Homesite
#' @return dataframe
#' @export

overallSummary <- function(loanAmt = NULL, APR = NULL, tenor = NULL, FICO = NULL, channel = NULL) {
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
  
  annualSummaryDF <- annualSummary(loanAmt, APR, tenor, FICO, channel)
  APRmonthly      <- APR/12
  SumPVCashFlow   <- round(sum(annualSummaryDF$annualPreTaxPVexlCPA), 2)
  afterTaxNPV     <- round(sum(annualSummaryDF$annualAfterTaxNPV), 2)
  ROA             <- round(sum(annualSummaryDF$annualContrMargin)/(tenor/12)/(sum(annualSummaryDF$annualAverageBegBal)/(tenor/12)), 4)
  monthlyPayment   <- round(loanAmt * APRmonthly * ((1 + APRmonthly)^tenor)/(1 - ((1 + APRmonthly)^tenor)), 2)
  CPAd             <- assumptionsCPA[assumptionsCPA$channel==channel,]$CPA
  CPAp             <- round(CPAd/loanAmt, 4)
  overallSummaryDF <- data.frame(as.numeric(SumPVCashFlow), as.numeric(afterTaxNPV),
                                as.numeric(ROA), as.numeric(monthlyPayment),
                                as.numeric(CPAd), as.numeric(CPAp))
  names(overallSummaryDF) <- c("SumPVCashFlow", "afterTaxNPV", "ROA", "monthlyPayment", "CPAdollar", "CPApct")
  return(overallSummaryDF)
}