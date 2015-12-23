# PraMachineLearning
renlanchuke  
2015年12月23日  

##Personal activity analysis
<font size=4>Load data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.
The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har</font>

```r
#set the workpath
setwd("F://gitRespository//PraMachineLearning")
#download the training and testing data
#trainingUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#testingUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(trainingUrl,destfile = "./trainingData.csv",method = "wininet")
#download.file(testingUrl,destfile = "./testingData.csv",method = "wininet")
#read the data file
trainingData<-read.csv("./trainingData.csv")
testingData<-read.csv("./testingData.csv")
```

<font size=4>Exploratory data analysis</font>


```r
summary(trainingData)
head(trainingData)
names(trainingData)
sum(complete.cases(trainingData))
```
