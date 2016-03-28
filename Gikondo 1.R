setwd("/Users/rachel/Dropbox/Sourcing/2. Data/Survey/Cleaned")

#gikondo results
resultsmar14<-read.csv("./HH_mar14_.csv", header=TRUE, sep=",")
resultsmar15<-read.csv("./HH_mar15_.csv", header=TRUE, sep=",")
resultsmar16<-read.csv("./HH_mar16_.csv", header=TRUE, sep=",")
resultsmar17<-read.csv("./HH_mar17_.csv", header=TRUE, sep=",")

gikresult<-rbind(resultsmar14,resultsmar15, resultsmar16, resultsmar17)

#Gikondo mean cost of current WM practices 
gikemptycost<-gikresult$cost_empty[!gikresult$cost_empty == 99]
gikemptycost1<-gikemptycost[!gikemptycost==0]
gik_mean_emptycostRWF<-mean(gikemptycost1,na.rm=TRUE)
gik_mean_emptycostUSD<-gik_mean_emptycostRWF*0.0013

#counts of empty practice

#empty practices not including Dont know/no response
gik_counts<-table(gikresult$emptypract[!gikresult$emptypract==99])
gik_counts<-table(gikresult$emptypract)
barplot(gik_counts,xlab="Emptying Practices",ylab="Respondent Count")
#alternative
countsdf<-as.data.frame(gik_counts)

#prices of different emptying practices
#average price of sealing a latrine then building a new one yourself 

# definition of function
costofpract<-function(result,practice){
  emptypract<-subset(result,result$emptypract==practice)
  rowemptypract<-nrow(subset(gikresult,gikresult$emptypract==practice))
  
  if(rowemptypract==0){
    return(NA)
  }
  
  cost<-emptypract$cost_empty[!emptypract$cost_empty==99]
  meancost<-mean(cost,na.rm=TRUE)
  return(meancost)
}

#usage of function
gikmeancost1 <- costofpract(gikresult, 1)
gikmeancost2 <- costofpract(gikresult, 2)
gikmeancost3 <- costofpract(gikresult, 3)
gikmeancost4 <- costofpract(gikresult, 4)
gikmeancost5 <- costofpract(gikresult, 5)
gikmeancost6 <- costofpract(gikresult, 6)
gikmeancost7 <- costofpract(gikresult, 7)
gikmeancost8 <- costofpract(gikresult, 8)
gikmeancost9 <- costofpract(gikresult, 9)
gikmeancost10 <- costofpract(gikresult, 10)
gikmeancost11 <- costofpract(gikresult, 11)
gikmeancost12 <- costofpract(gikresult, 12)
gikmeancost13 <- costofpract(gikresult, 13)
gikmeancost14 <- costofpract(gikresult, 14)
gikmeancost15 <- costofpract(gikresult, 15)
gikmeancost16 <- costofpract(gikresult, 16)
gikmeancost17 <- costofpract(gikresult, 77)
gikmeancost18 <- costofpract(gikresult, 99)

gikmeancost_vect<-c(gikmeancost1,
                    gikmeancost2,
                    gikmeancost3,
                    gikmeancost4,
                    gikmeancost5,
                    gikmeancost6,
                    gikmeancost7,
                    gikmeancost8,
                    gikmeancost9,
                    gikmeancost11,
                    gikmeancost12,
                    gikmeancost13,
                    gikmeancost14, 
                    gikmeancost15,
                    gikmeancost16,
                    gikmeancost17,
                    gikmeancost18)

gikmeancost_labs<-c("1",
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

gikmeancost_labs<-gikmeancost_labs[!is.na(gikmeancost_vect)]
gikmeancost_vect<-gikmeancost_vect[!is.na(gikmeancost_vect)]

X<-as.data.frame(cbind(gikmeancost_labs,gikmeancost_vect))
#d<-X[!(X$gikmeancost_vect=="NaN"),]


meancost_gik<-barplot(gikmeancost_vect,names.arg=gikmeancost_labs)
