library("R.matlab")
fileName<-file.path("C:/Users/Hansel/Downloads/2016试题/B/B题附件","genotype.dat")
genotype <- read.table(fileName,header = TRUE)
phenotype <- read.table("C:/Users/Hansel/Downloads/2016试题/B/B题附件/phenotype.txt",header=FALSE,sep="\n")
library(data.table)
geno_phenotype <- data.table(genotype)
geno_phenotype <- geno_phenotype[,phenotype:=phenotype]
col_name <- colnames(geno_phenotype)
table(geno_phenotype[,col_name[1]])
test <- data.frame(geno_phenotype)
for(i in 1:(length(col_name)-1)){
   if(levels(test[,i])==c("DD","ID","II")) {
       levels(test[,i]) <- c("CC","TC","TT")
   }
       
}




for(i in 1:(length(col_name)-1)){
    name <- col_name[i]
    curTableALL <- table(test[,i])
    curTableTrue <- table(test[test$phenotype==1,i])
    dataFrame <- data.frame(colname=name,curTableTrue/curTableALL)
    if(i==1){
        conditionPro <- dataFrame
    }else{
        conditionPro <- rbind(conditionPro,dataFrame)
    }
    
}
#根据图像，阈值取0.75
qplot(1:28335,conditionPro$Freq,conditionPro)

#验证每列取值都为3种
count=0
for(i in 1:(length(col_name)-1)){
    
    len <- length(table(test[,i]))
    if(len==3){
       count=count+1 
    }
}

#筛选属性
conditionProFilter <- conditionPro[conditionPro$Freq>=0.6,]
#conditionProFilter <- conditionPro
filterCol <- conditionProFilter$colname
dataFilter <- test[,filterCol]
dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
dataFilter$phenotype <- as.factor(dataFilter$phenotype)

#决策树训练
#library(caret)
set.seed(2333)
inTrain <- createDataPartition(y=dataFilter$phenotype,p=0.8,list = FALSE)

training <- dataFilter[inTrain,]
testing <- dataFilter[-inTrain,]

modFit <- train(phenotype~.,method="rf",data=training, 
                trControl=trainControl(method = "cv",number = 3,verboseIter = TRUE))

Pred <- predict(modFit,newdata=testing)
confusionMatrix(testing$phenotype,Pred)



##检测
data_test <- test[,c("rs1888759","phenotype")]  
data_test_df <- tbl_df(data_test)
data_test_grouo <- group_by(data_test_df,rs1888759,phenotype)
summarise(data_test_grouo,count=n())

snpseq=c()
for(i in 1:(length(col_name)-1)){
    
    len <- as.character(unique(test[,i]))
    snpseq=c(snpseq,len)
    snpseq=unique(snpseq)
    
}


for(i in 1:(length(col_name)-1)){
    
    len <- as.character(unique(test[,i]))
    snpseq=c(snpseq,len)
    snpseq=unique(snpseq)
    
}


for(i in 1:(length(col_name)-1)){
    if(sum(test[,i]=="ID")>=1){
        print(i)
        break
    }
    
}

#回归分析
re_data <- test

for(i in 1:(length(col_name)-1)){
    
    if(i==1){
        levels_frame=levels(re_data[,i])
    }else{
        levels_frame=rbind(levels_frame,levels(re_data[,i]))
    }
    
    
}
unique(levels_frame)

for(i in 1:(length(col_name)-1)){
    
    levels(re_data[,i]) <- c("-1","0","1")
}
for(i in 1:(length(col_name)-1)){
    
    re_data[,i] <- as.integer(as.character(re_data[,i]))
}
#pca
preObjPCA<-preProcess(re_data[,1:(length(col_name)-1)],method="pca",pcaComp = 2)
dataPCA <- predict(preObjPCA,re_data)
qplot(PC1,PC2,data=dataPCA,col=phenotype)


#筛选属性
conditionProFilter2 <- conditionPro[conditionPro$Freq>=0.55,]
#conditionProFilter <- conditionPro
filterCol2 <- conditionProFilter2$colname
dataFilter2 <- re_data[,filterCol2]
dataFilter2 <- cbind(dataFilter2,phenotype=test$phenotype)
dataFilter2$phenotype <- as.factor(dataFilter$phenotype)

#library(caret)
set.seed(2333)
inTrain <- createDataPartition(y=dataFilter2$phenotype,p=0.8,list = FALSE)

training <- dataFilter2[inTrain,]
testing <- dataFilter2[-inTrain,]

preObjPCA<-preProcess(dataFilter2[,1:(dim(dataFilter2)[2]-1)],method="pca",pcaComp = 2)
dataPCA <- predict(preObjPCA,dataFilter2)
qplot(PC1,PC2,data=dataPCA,col=phenotype)

modFit <- train(phenotype~.,method="",data=training, 
                trControl=trainControl(method = "cv",number = 10,verboseIter = TRUE))
Pred <- predict(modFit,newdata=testing)
confusionMatrix(testing$phenotype,Pred)

Pred <- predict(modFit,newdata=training)
confusionMatrix(training$phenotype,Pred)

library(e1071)
svmfit <- svm(phenotype ~ ., data = training, 
              kernel = "linear", cost = 10, scale = FALSE)
Pred <- predict(svmfit,newdata=testing)
confusionMatrix(testing$phenotype,Pred)

Pred <- predict(svmfit,newdata=training)
confusionMatrix(training$phenotype,Pred)


#select the importance varianle
#筛选属性
conditionProFilter <- conditionPro[conditionPro$Freq>=0.6,]
#conditionProFilter <- conditionPro

filterCol <- conditionProFilter$colname
dataFilter <- test[,filterCol]
dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
dataFilter$phenotype <- as.factor(dataFilter$phenotype)
set.seed(2333)
inTrain <- createDataPartition(y=dataFilter$phenotype,p=0.7,list = FALSE)
training <- dataFilter[inTrain,]
testing <- dataFilter[-inTrain,]

result_container <- list();

while(length(filterCol)>2){
    
    #modFit <- train(phenotype~.,method="rf",data=training,
                    #trControl=trainControl(method = "cv",number = 3,seeds = 2333,verboseIter = TRUE))
    modFit <- randomForest(phenotype~.,ntree=1500,data=training,importance=TRUE,do.trace=FALSE)
    Pred <- predict(modFit,newdata=testing)
    accuracy_cur <- sum(testing$phenotype == Pred)/length(Pred)
    print(accuracy_cur)
    val_Imp <- importance(modFit,1)
    tem_colNames <- row.names(val_Imp)
    new_colNames <- tem_colNames[val_Imp<=quantile(val_Imp,0.9)]
    
    result_container <- c(result_container,list(valNames=filterCol,accuracy=accuracy_cur))
    filterCol=new_colNames
    training <- cbind(training[,filterCol],phenotype=training$phenotype)
    testing <- cbind(testing[,filterCol],phenotype=testing$phenotype)
    
}




