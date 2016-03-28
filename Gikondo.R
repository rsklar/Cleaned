setwd("/Users/rachel/Dropbox/Sourcing/2. Data/Survey/Cleaned")
#gikondo results
resultsmar14<-read.csv("./HH_mar14_.csv", header=TRUE, sep=",")
resultsmar15<-read.csv("./HH_mar15_.csv", header=TRUE, sep=",")
resultsmar16<-read.csv("./HH_mar16_.csv", header=TRUE, sep=",")
resultsmar17<-read.csv("./HH_mar17_.csv", header=TRUE, sep=",")

#gitega results
resultsmar18<-read.csv("./HH_mar18_.csv", header=TRUE, sep=",")
resultsmar21<-read.csv("./HH_mar21_.csv", header=TRUE, sep=",")



gikresult<-rbind(resultsmar14,resultsmar15, resultsmar16, resultsmar17)

#Gikondo mean cost of current WM practices 
gikemptycost<-gikresult$cost_empty[!gikresult$cost_empty == 99]
gikemptycost1<-gikemptycost[!gikemptycost==0]
gik_mean_emptycostRWF<-mean(gikemptycost1,na.rm=TRUE)
gik_mean_emptycostUSD<-gik_mean_emptycostRWF*0.0013

#counts of empty practice
  gik_counts<-table(gikresult$emptypract)
  barplot(gik_counts, ylab="Number of Respondents (n=126)", xlab="FSM Practice",main="Emptying Practices, Gikondo")

 
#prices of different emptying practices
  #average price of sealing a latrine then building a new one yourself 
  
  #respondents that chose empty practice 1
  gik_emptypract1<-subset(gikresult,gikresult$emptypract==1)
  gik_costpract1<-gik_emptypract1$cost_empty[!gik_emptypract1$cost_empty==99]
  gikmeancost1<-mean(gik_costpract1,na.rm=TRUE)
  
  gik_emptypract2<-subset(gikresult,gikresult$emptypract==2)
  gik_costpract2<-gik_emptypract2$cost_empty[!gik_emptypract2$cost_empty==99]
  gikmeancost2<-mean(gik_costpract2,na.rm=TRUE)
  
  gik_emptypract3<-subset(gikresult,gikresult$emptypract==3)
  gik_costpract3<-gik_emptypract3$cost_empty[!gik_emptypract3$cost_empty==99]
  gikmeancost3<-mean(gik_costpract3,na.rm=TRUE)
  
  gik_emptypract5<-subset(gikresult,gikresult$emptypract==5)
  gik_costpract5<-gik_emptypract5$cost_empty[!gik_emptypract5$cost_empty==99]
  gikmeancost5<-mean(gik_costpract5,na.rm=TRUE)
  
  gik_emptypract7<-subset(gikresult,gikresult$emptypract==7)
  gik_costpract7<-gik_emptypract7$cost_empty[!gik_emptypract7$cost_empty==99]
  gikmeancost7<-mean(gik_costpract7,na.rm=TRUE)
  
  #removing it because for "hire a builder to empty it and dump the contents in the environment/wetland/drainage area" the only price reported is 0.
  #gik_emptypract8<-subset(gikresult,gikresult$emptypract==8)
  #gik_costpract8<-gik_emptypract8$cost_empty[!gik_emptypract8$cost_empty==99]
  #gikmeancost8<-mean(gik_costpract8,na.rm=TRUE)
  
  gik_emptypract9<-subset(gikresult,gikresult$emptypract==9)
  gik_costpract9<-gik_emptypract9$cost_empty[!gik_emptypract9$cost_empty==99]
  gikmeancost9<-mean(gik_costpract9,na.rm=TRUE)
  
  gik_emptypract10<-subset(gikresult,gikresult$emptypract==10)
  gik_costpract10<-gik_emptypract10$cost_empty[!gik_emptypract10$cost_empty==99]
  gikmeancost10<-mean(gik_costpract10,na.rm=TRUE)
  
  gik_emptypract11<-subset(gikresult,gikresult$emptypract==11)
  gik_costpract11<-gik_emptypract11$cost_empty[!gik_emptypract11$cost_empty==99]
  gikmeancost11<-mean(gik_costpract11,na.rm=TRUE)
  
  gik_emptypract14<-subset(gikresult,gikresult$emptypract==14,na.rm=TRUE)
  gik_costpract14<-gik_emptypract14$cost_empty[!gik_emptypract14$cost_empty==99]
  gikmeancost14<-mean(gik_costpract14,na.rm=TRUE)
  
  gik_emptypract77<-subset(gikresult,gikresult$emptypract==77)
  gik_costpract77<-gik_emptypract77$cost_empty[!gik_emptypract77$cost_empty==99]
  gikmeancost77<-mean(gik_costpract77,na.rm=TRUE)
  
  gik_emptypract99<-subset(gikresult,gikresult$emptypract==99)
  gik_costpract99<-gik_emptypract99$cost_empty[!gik_emptypract99$cost_empty==99]
  gikmeancost99<-mean(gik_costpract99,na.rm=TRUE)
 
labelmeancosts<-c("1","2","3","5","7","9","10","11","14")
gik_meancostempty<-c(gikmeancost1,gikmeancost2,gikmeancost3,gikmeancost5,gikmeancost7,gikmeancost9,gikmeancost10,gikmeancost11,gikmeancost14)

gik_meancostempty
barplot(gik_meancostempty,ylab="Price",names.arg =labelmeancosts,xlab="FSM Practice",main = "Mean Price of Emptying Practices, Gikondo")

#residence time versus pit fill frequency
gik_resyr<-gikresult$hh_residtime/12
gik_pitfillfreq<-gikresult$pitfillfreq
gik_resyr_v_pitfill<-cbind(gik_resyr,gik_pitfillfreq)
plot(resyr_v_pitfill)

