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
#git_counts<-table(gitresult$emptypract[!gitresult$emptypract==99])
git_counts<-table(gitresult$emptypract)
barplot(git_counts,xlab="Emptying Practices",ylab="Number of Respondents (n=79)",main="Emptying Practices, Gitega")
#alternative
countsdf<-as.data.frame(git_counts)

#prices of different emptying practices
#average price of sealing a latrine then building a new one yourself 

# definition of function
costofpract<-function(result,practice){
  emptypract<-subset(result,result$emptypract==practice)
  rowemptypract<-nrow(subset(gitresult,gitresult$emptypract==practice))
  
  if(rowemptypract==0){
    return(NA)
  }
    
  cost<-emptypract$cost_empty[!emptypract$cost_empty==99]
  meancost<-mean(cost,na.rm=TRUE)
  return(meancost)
}

#usage of function
gitmeancost1 <- costofpract(gitresult, 1)
gitmeancost2 <- costofpract(gitresult, 2)
gitmeancost3 <- costofpract(gitresult, 3)
gitmeancost4 <- costofpract(gitresult, 4)
gitmeancost5 <- costofpract(gitresult, 5)
gitmeancost6 <- costofpract(gitresult, 6)
gitmeancost7 <- costofpract(gitresult, 7)
gitmeancost8 <- costofpract(gitresult, 8)
gitmeancost9 <- costofpract(gitresult, 9)
gitmeancost10 <- costofpract(gitresult, 10)
gitmeancost11 <- costofpract(gitresult, 11)
gitmeancost12 <- costofpract(gitresult, 12)
gitmeancost13 <- costofpract(gitresult, 13)
gitmeancost14 <- costofpract(gitresult, 14)
gitmeancost15 <- costofpract(gitresult, 15)
gitmeancost16 <- costofpract(gitresult, 16)
gitmeancost17 <- costofpract(gitresult, 77)
gitmeancost18 <- costofpract(gitresult, 99)

gitmeancost_vect<-c(gitmeancost1,
                    gitmeancost2,
                    gitmeancost3,
                    gitmeancost4,
                    gitmeancost5,
                    gitmeancost6,
                    gitmeancost7,
                    gitmeancost8,
                    gitmeancost9,
                    gitmeancost10,
                    gitmeancost11,
                    gitmeancost12,
                    gitmeancost13,
                    gitmeancost14, 
                    gitmeancost15,
                    gitmeancost16,
                    gitmeancost17,
                    gitmeancost18)

gitmeancost_labs<-c("1",
                    "2",
                    "3",
                    "4",
                    "5",
                    "6",
                    "7",
                    "8",
                    "9",
                    "10",
                    "11",
                    "12",
                    "13",
                    "14", 
                    "15",
                    "16",
                    "17",
                    "18")

gitmeancost_labs<-gitmeancost_labs[!is.na(gitmeancost_vect)]
gitmeancost_vect<-gitmeancost_vect[!is.na(gitmeancost_vect)]

X<-as.data.frame(cbind(gitmeancost_labs,gitmeancost_vect))
#d<-X[!(X$gitmeancost_vect=="NaN"),]


meancost_git<-barplot(gitmeancost_vect,names.arg=gitmeancost_labs,xlab="FSM Practice",main="Mean Price of Emptying Practices, Gitega")
options(scipen=10)



