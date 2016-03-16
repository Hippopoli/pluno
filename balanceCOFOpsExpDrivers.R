## Fix for NOTEs from R CMD check
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(c("bankWACD", "fedFundsRate", "CPA", "PSF", "pbad", "pprepay", "attributedEquity", 
                           "originationsExpense", "servicingExpense", "collectionsExpense",
                           "variableTechExpense", "fixedExpenses", "recoveryRate", "taxRate", "hurdleRate",
                           "assumptionsCOF", "assumptionsOthers", "assumptionsPSF", "assumptionsPrepayRates",
                           "assumptionsLossRates", "assumptionsCPA"))
}

#' @title Balance COF Ops Expense drivers
#'
#' @description An intermediate step and salculate by stmt, balance drivers, Cost of Funds drivers, Variable Operating Expenses drivers
#' @param loanAmt Loan amount as a numeric number greater than 0
#' @param APR value is a numeric number between 0 and 1
#' @param tenor Length of the loan, a numberic number and can only be one of these 24, 36, 48, 60, 70
#' @param FICO Customer credit profile can only be 660-700 700-740 or 740-850
#' @param channel Customer acquisition channel a string can only be PaidSearch or Affiliates or DirectMail or Email or Homesite
#' @return dataframe
#' @export

balanceCOFOpsExpDrivers <- function(loanAmt = NULL, APR = NULL, tenor = NULL, FICO = NULL, channel = NULL) {
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
  
  # generate month list, APRmonthly, pmt single number
  month      <- c(1:tenor)
  APRmonthly <- APR/12
  
  # initiate outputs
  pmt        <- list() # negative
  begBal     <- list() # positive
  ppmt       <- list() # negative
  ipmt       <- list() # positive
  prepay     <- list() # negative
  co         <- list() # negative
  endBal     <- list() # positive
  fixedIE    <- list() # negative
  floatIE    <- list() # negative
  rebatedIE  <- list() # negative
  
  # start calculation by month
  for (i in month){
    # balance driver
    if (i==1) {
      begBal[i] <- loanAmt
      }
    if (i>1)  {
      begBal[i] <- round(as.numeric(unlist(endBal[i-1])), 1)
      }
    pmt[i]    <- round(as.numeric(unlist(begBal[i])) * APRmonthly * ((1+APRmonthly)^(tenor-i+1))/(1-((1+APRmonthly)^(tenor-i+1))), 1)
    ipmt[i]   <- round(as.numeric(unlist(begBal[i])) * APRmonthly, 1)
    ppmt[i]   <- as.numeric(unlist(pmt[i])) + as.numeric(unlist(ipmt[i]))
    prepay[i] <- round((-1) * loanAmt * (assumptionsPrepayRates[(assumptionsPrepayRates$FICO==FICO)&
                                                              (assumptionsPrepayRates$tenor==tenor)&
                                                              (assumptionsPrepayRates$month==i), ]$pprepay) * (assumptionsPSF[(assumptionsPSF$FICO==FICO)&(assumptionsPSF$tenor==tenor), ] $PSF), 1)
    co[i]      <- round((-1) * loanAmt*(assumptionsLossRates[(assumptionsLossRates$FICO==FICO)&
                                                            (assumptionsLossRates$tenor==tenor)&
                                                            (assumptionsLossRates$month==i), ]$pbad), 1)
    endBal[i]  <- as.numeric(unlist(begBal[i])) + as.numeric(unlist(ppmt[i])) + as.numeric(prepay[i]) + as.numeric(co[i])
    if (i==tenor) {
      endBal[i]=0
    }
    # COF driver
    fixedIE[i] <-   round((as.numeric(unlist(begBal[i])) + as.numeric(unlist(endBal[i]))) * (-1/2) * assumptionsCOF[assumptionsCOF$month==i, ]$bankWACD/(10000 * 12), 1)
    floatIE[i] <-   round((as.numeric(unlist(begBal[i]))+ as.numeric(unlist(endBal[i]))) * (-1/2) * assumptionsCOF[assumptionsCOF$month==i, ]$fedFundsRate/(10000 * 12), 1)
    rebatedIE[i] <- round((as.numeric(unlist(fixedIE[i])) + as.numeric(unlist(floatIE[i]))) * (-1) * assumptionsOthers$attributedEquity, 1)
  } # end of looping through month
  
  # operating expense Driver
  origEX <- round(c(assumptionsOthers$originationsExpense * (-1), rep(0, tenor-1)), 1)
  servEX <- round(rep(assumptionsOthers$servicingExpense * (-1)/12, tenor), 1)
  collEX <- round(rep(assumptionsOthers$collectionsExpense * (-1)/12, tenor), 1)
  techEX <- round(rep(assumptionsOthers$variableTechExpense * (-1)/12, tenor), 1)
  acqsEX <- c(assumptionsCPA[assumptionsCPA$channel==channel, ]$CPA * (-1), rep(0, tenor-1))
  
  # assemble everything together
  balanceCOFOpsExpDF <- data.frame(as.numeric(month), as.numeric(unlist(begBal)), 
                                   as.numeric(unlist(pmt)), as.numeric(unlist(ppmt)),
                                   as.numeric(unlist(ipmt)), as.numeric(unlist(prepay)),
                                   as.numeric(unlist(co)), as.numeric(unlist(endBal)),
                                   as.numeric(unlist(fixedIE)), as.numeric(unlist(floatIE)), 
                                   as.numeric(unlist(rebatedIE)),origEX,servEX,collEX,techEX,acqsEX)
  names(balanceCOFOpsExpDF) <- c("month", "begBal", "pmt", "ppmt", "ipmt", "prepay", "co", 
                                 "endBal","fixedIE", "floatIE", "rebatedIE",
                                 "origEX", "servEX", "collEX", "techEX", "acqsEX")
  
  return(balanceCOFOpsExpDF)
}