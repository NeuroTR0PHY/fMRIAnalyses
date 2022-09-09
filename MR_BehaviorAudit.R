require(xlsx)

#subjects = c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
subname = c("01","03","04","05","06","07","09","10","11","12","13","14","15","16","17","18","19","20","21")
sub = list()
#s = 1
for(s in 1:19){
  subject = subname[s]
  new = list()
  print(subject)
 # subname[s]
  for(r in 1:8){
    print(r)
    run = paste0(0,r)
    file = paste0(getwd(), "/Frey06_", subject,"/evs/Frey06_", subject,"_EV_run",run,".xlsx")
    data = read.xlsx(file, 1, rowIndex = 21:37, colIndex = 6:10)
    data$TrialType = as.factor(data$TrialType)
    data$MVtime =  data$Bpress..ms. - data$Blift..ms.
    cat = data.frame(rep(subject, 16),data$TrialType, data$Blift..ms., data$MVtime)
    new[[r]] <- cat
  }
  result = do.call(rbind,new)
  agg = aggregate(result, by=list(result$data.TrialType), FUN=mean, na.rm = T)
  agg = data.frame(rep(subject,4), agg)
  sub[[s]] = agg
}
final = do.call(rbind,sub)

