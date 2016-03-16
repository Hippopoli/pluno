## Fix for NOTEs from R CMD check
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("bankWACD", "fedFundsRate", "CPA", "PSF", "pbad", "pprepay", "attributedEquity", 
                           "originationsExpense", "servicingExpense", "collectionsExpense",
                           "variableTechExpense", "fixedExpenses", "recoveryRate", "taxRate", "hurdleRate",
                           "assumptionsCOF", "assumptionsOthers", "assumptionsPSF", "assumptionsPrepayRates",
                           "assumptionsLossRates", "assumptionsCPA"))
}

#' @title Cash Flow and Revenue and Losses drivers by month on the book 
#'
#' @description Calculate by stmt, losses driver and all cash flows, and then combine with other drivers
#' @param loanAmt Loan amount as a numeric number greater than 0
#' @param APR value is a numeric number between 0 and 1
#' @param tenor Length of the loan, a numberic number and can only be one of these 24, 36, 48, 60, 70
#' @param FICO Customer credit profile can only be 660-700 700-740 or 740-850
#' @param channel Customer acquisition channel a string can only be PaidSearch or Affiliates or DirectMail or Email or Homesite
#' @return dataframe
#' @export

cashFlowAllDrivers <- function(loanAmt = NULL, APR = NULL, tenor = NULL, FICO = NULL, channel = NULL) {
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
  
  # generate other drivers for this part of forward looking calculation
  balanceCOFOpsExpDF <- balanceCOFOpsExpDrivers(loanAmt, APR, tenor, FICO, channel)
  month      <- c(1:tenor)
  # initiate output lists
  co12       <- list()
  begALLL    <- list()
  grossCO    <- list()
  recoveries <- list()
  provisions <- list()
  endALLL    <- list()
  reserveD   <- list()
  
  # loop through month
  for (i in month) {
    if (i==1) {
      begALLL[i]  <- 0
      }
    if (i>1) {
      begALLL[i]  <- round(as.numeric(unlist(endALLL[i-1])), 1)
      }
    co12[i]       <- ifelse((i+12) < tenor, sum(balanceCOFOpsExpDF[c((i+1):(i+12)), ]$co),
                            sum(balanceCOFOpsExpDF[c((i+1):tenor), ]$co, na.rm = TRUE))
    grossCO[i]    <- round(as.numeric(unlist(balanceCOFOpsExpDF$co[i])), 1)
    recoveries[i] <- round(as.numeric(unlist(balanceCOFOpsExpDF$co[i])) * (-1) 
                           * assumptionsOthers$recoveryRate, 1)
    provisions[i]  <- round(as.numeric(unlist(co12[i])) * (-1) 
                            - as.numeric(unlist(begALLL[i]))
                            - as.numeric(unlist(grossCO[i])) 
                            - as.numeric(unlist(recoveries[i])), 1)
    endALLL[i]    <- round(as.numeric(unlist(begALLL[i])) 
                           + as.numeric(unlist(provisions[i])) 
                           + as.numeric(unlist(grossCO[i])) 
                           + as.numeric(unlist(recoveries[i])), 1)
    reserveD[i]   <- round(as.numeric(unlist(endALLL[i])) 
                           - as.numeric(unlist(begALLL[i])), 1)
  } #end of looping through month
  
  lossDF <- data.frame(as.numeric(unlist(begALLL)), as.numeric(unlist(provisions)), 
                       as.numeric(unlist(reserveD)), as.numeric(unlist(grossCO)), 
                       as.numeric(unlist(recoveries)), as.numeric(unlist(endALLL)))
  names(lossDF) <- c("begALLL", "provisions", "reserveD", "grossCO", 
                     "recoveries", "endALLL")
  
  cashflowDF <- cbind(balanceCOFOpsExpDF, lossDF)
  
  cashflowDF$contrMargin <- cashflowDF$ipmt+cashflowDF$fixedIE+
    cashflowDF$floatIE+cashflowDF$rebatedIE-cashflowDF$reserveD+
    cashflowDF$grossCO+cashflowDF$recoveries+cashflowDF$origEX+
    cashflowDF$servEX+cashflowDF$collEX+cashflowDF$techEX+cashflowDF$acqsEX
  
  cashflowDF$contrMarginWoRes <- round(cashflowDF$ipmt+cashflowDF$fixedIE+
                                         cashflowDF$floatIE+cashflowDF$rebatedIE+
                                         cashflowDF$grossCO+cashflowDF$recoveries+
                                         cashflowDF$origEX+cashflowDF$servEX+
                                         cashflowDF$collEX+cashflowDF$techEX+
                                         cashflowDF$acqsEX, 2)
  
  cashflowDF$fixedEX <- round(rep(assumptionsOthers$fixedExpenses * (-1)/12, tenor), 1)
  cashflowDF$preTaxIncome <- round(cashflowDF$contrMargin + cashflowDF$fixedEX, 1)
  return(cashflowDF)
}