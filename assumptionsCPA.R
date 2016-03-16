assumptionsCPA<-
  data.frame(
    channel<-c("Affiliates","DirectMail","Email","Homesite","PaidSearch"),
    CPA<-c(381.2,470.7,7.9,3.7,381.2))
names(assumptionsCPA)<-c("channel","CPA")
saveRDS(assumptionsCPA,file="/Users/Summer/Desktop/accountNPV/assumptionsCPA.Rda")