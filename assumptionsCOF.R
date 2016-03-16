assumptionsCOF<-
  data.frame(
    month<-c(1:72),
    bankWACD<-rep(119,72),
    fedFundsRate<-c(rep(55,12),rep(89,12),rep(126,12),rep(151,12),rep(170,24))
  )
names(assumptionsCOF)<-c("month","bankWACD","fedFundsRate")
saveRDS(assumptionsCOF,file="/Users/Summer/Desktop/accountNPV/assumptionsCOF.Rda")

    
      