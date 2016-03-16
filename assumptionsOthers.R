assumptionsOthers<-
  data.frame(
    attributedEquity    =0.131,
    originationsExpense =16.3,
    servicingExpense    =18.7,
    collectionsExpense  =15.2,
    variableTechExpense =17.1,
    fixedExpenses       =221.2,
    # potentially allow users to change the following three fields
    recoveryRate        =0.08,
    taxRate             =0.382,
    hurdleRate          =0.11)
  names(assumptionsOthers)<-c("attributedEquity","originationsExpense","servicingExpense",
                              "collectionsExpense","variableTechExpense","fixedExpenses",
                              "recoveryRate","taxRate","hurdleRate")
saveRDS(assumptionsOthers,file="/Users/Summer/Desktop/accountNPV/assumptionsOthers.Rda")