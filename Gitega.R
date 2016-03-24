setwd("/Users/rachel/Dropbox/Sourcing/2. Data/Survey/Cleaned")

#gitega results
resultsmar18<-read.csv("./HH_mar18_.csv", header=TRUE, sep=",")
resultsmar21<-read.csv("./HH_mar21_.csv", header=TRUE, sep=",")

gitresult<-rbind(resultsmar18,resultsmar21)

#Gitega mean cost of current WM practices 
gitemptycost<-gitresult$cost_empty[!gitresult$cost_empty == 99]
gitemptycost1<-gitemptycost[!gitemptycost==0]
git_mean_emptycostRWF<-mean(gitemptycost1,na.rm=TRUE)
git_mean_emptycostUSD<-git_mean_emptycostRWF*0.0013

#counts of empty practice

  #empty practices not including Dont know/no response
git_counts<-table(gitresult$emptypract[!gitresult$emptypract==99])
barplot(git_counts,xlab="Emptying Practices",ylab="Respondent Count")
#alternative
countsdf<-as.data.frame(git_counts)

#prices of different emptying practices
#average price of sealing a latrine then building a new one yourself 

#respondents that chose empty practice 1
git_emptypract1<-subset(gitresult,gitresult$emptypract==1)
git_costpract1<-git_emptypract1$cost_empty[!git_emptypract1$cost_empty==99]
gitmeancost1<-mean(git_costpract1)

git_emptypract5<-subset(gitresult,gitresult$emptypract==5)
git_costpract5<-git_emptypract5$cost_empty[!git_emptypract5$cost_empty==99]
gitmeancost5<-mean(git_costpract5)

git_emptypract7<-subset(gitresult,gitresult$emptypract==7)
git_costpract7<-git_emptypract7$cost_empty[!git_emptypract7$cost_empty==99]
gitmeancost7<-mean(git_costpract7)

git_emptypract9<-subset(gitresult,gitresult$emptypract==9)
git_costpract9<-git_emptypract9$cost_empty[!git_emptypract9$cost_empty==99]
gitmeancost9<-mean(git_costpract9)


git_emptypract13<-subset(gitresult,gitresult$emptypract==13)
git_costpract13<-git_emptypract13$cost_empty[!git_emptypract13$cost_empty==99]
gitmeancost13<-mean(git_costpract13)

#14 removed from list of means to display in graph because the only values are 99
git_emptypract14<-subset(gitresult,gitresult$emptypract==14)
git_costpract14<-git_emptypract14$cost_empty[!git_emptypract14$cost_empty==99]
gitmeancost14<-mean(git_costpract14,na.rm=TRUE)

git_emptypract16<-subset(gitresult,gitresult$emptypract==16)
git_costpract16<-git_emptypract16$cost_empty[!git_emptypract16$cost_empty==99]
gitmeancost16<-mean(git_costpract16)

git_emptypract99<-subset(gitresult,gitresult$emptypract==99)
git_costpract99<-git_emptypract99$cost_empty[!git_emptypract99$cost_empty==99]
gitmeancost99<-mean(git_costpract99)

#labelmeancosts<-c("1","2","3","5","7","8","9","10","11","14")
labelmeancosts<-c("1","5","9","13","16")
#git_meancostempty<-c(gitmeancost1,gitmeancost5,gitmeancost7,gitmeancost9,gitmeancost13,gitmeancost14,gitmeancost16)
git_meancostempty<-c(gitmeancost1,gitmeancost5,gitmeancost9,gitmeancost13,gitmeancost16)

barplot(git_meancostempty,names.arg=c("1","5","9","13","16"),xlab="Emptying Practice",ylab="Price RWF")

#things to work out:
  #git_emptypract7$costempty[!git_emptypract7$cost_empty==99]....why isn't it removing the record==99?
  #they didn't know the cost of empty 7 so don't include it in the price summary

#also had to take out 14 because gitmeancost14=NA
#the only values for 14 were 99, so the mean ended up being Nan 
#how can i take out categories whose mean endsup being NaN? 
