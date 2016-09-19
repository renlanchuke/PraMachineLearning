#######################################################################
##########读取数据并处理
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

#############################################################################
######                      question1
#############################################################################
re_data <- test
for(i in 1:(length(col_name)-1)){
    
    levels(re_data[,i]) <- c("-1","0","1")
}
for(i in 1:(length(col_name)-1)){
    
    re_data[,i] <- as.integer(as.character(re_data[,i]))
}


# #图像
# ggplot(data=conditionPro,aes(x=1:28335,y=conditionPro$Freq))+geom_point()+xlab("位点")+ylab("致病条件概率")
# 
# #验证每列取值都为3种
# count=0
# for(i in 1:(length(col_name)-1)){
#     
#     len <- length(table(test[,i]))
#     if(len==3){
#        count=count+1 
#     }
# }

#筛选属性
conditionProFilter <- conditionPro[conditionPro$Freq>=0.65,]
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
                trControl=trainControl(method = "cv",number = 5,verboseIter = TRUE))

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
conditionProFilter2 <- conditionPro[conditionPro$Freq>=0.65,]
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

#############################################################################
#########question2
############################################################################
#########计算位点碱基型条件概率
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

conditionProFilter <- conditionPro[conditionPro$Freq>=0.53,]

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


#画图比较
#####0.53
len <- length(result_NodeOOP_0.53)
i=1
SNPnum <- c()
accuracy_Gini_0.53 <- c()
accuracy_OOP_0.53 <- c()
while(i <= len){
    SNPnum <- c(SNPnum,length(result_NodePurity_0.53[[i]]))
    accuracy_Gini_0.53 <- c (accuracy_Gini_0.53,result_NodePurity_0.53[[i+1]])
    accuracy_OOP_0.53 <- c (accuracy_OOP_0.53,result_NodeOOP_0.53_new[[i+1]])
    
    i=i+2
}
dataFrame_test_0.53 <- data.frame(SNPnum=SNPnum,
                                  accuracy_Gini_0.53=accuracy_Gini_0.53,
                                  accuracy_OOP_0.53 =accuracy_OOP_0.53)



dataFrame_test_0.53_re <- melt(dataFrame_test_0.53,id="SNPnum")
ggplot(data=dataFrame_test_0.53_re, aes(x=SNPnum, y=value, colour=variable)) + geom_line()+
    geom_point(size=2, shape=20)+scale_x_continuous(trans = c("log10"))

####0.55

len <- length(result_NodeOOP_0.55)
i=1
SNPnum <- c()
accuracy_Gini_0.55 <- c()
accuracy_OOP_0.55 <- c()
while(i <= len){
    SNPnum <- c(SNPnum,length(result_NodePurity_0.55[[i]]))
    accuracy_Gini_0.55 <- c (accuracy_Gini_0.55,result_NodePurity_0.55[[i+1]])
    accuracy_OOP_0.55 <- c (accuracy_OOP_0.55,result_NodeOOP_0.55[[i+1]])
    
    i=i+2
}
dataFrame_test_0.55 <- data.frame(SNPnum=SNPnum,
                                  accuracy_Gini_0.55=accuracy_Gini_0.55,
                                  accuracy_OOP_0.55 =accuracy_OOP_0.55)




dataFrame_test_0.55_re <- melt(dataFrame_test_0.55,id="SNPnum")
ggplot(data=dataFrame_test_0.55_re, aes(x=SNPnum, y=value, colour=variable)) + geom_line()+
    geom_point(size=2, shape=20)+scale_x_continuous(trans = c("log10"))


####0.60
len <- length(result_NodeOOP_0.60)
i=1
SNPnum <- c()
accuracy_Gini_0.6 <- c()
accuracy_OOP_0.6 <- c()
while(i <= len){
    SNPnum <- c(SNPnum,length(result_NodePurity_0.6[[i]]))
    accuracy_Gini_0.6 <- c (accuracy_Gini_0.6,result_NodePurity_0.6[[i+1]])
    accuracy_OOP_0.6 <- c (accuracy_OOP_0.6,result_NodeOOP_0.6[[i+1]])
    
    i=i+2
}
dataFrame_test_0.6 <- data.frame(SNPnum=SNPnum,
                                  accuracy_Gini_0.6=accuracy_Gini_0.6,
                                  accuracy_OOP_0.6 =accuracy_OOP_0.6)

dataFrame_test_0.6_re <- melt(dataFrame_test_0.6,id="SNPnum")
ggplot(data=dataFrame_test_0.6_re, aes(x=SNPnum, y=value, colour=variable)) + geom_line()+
    geom_point(size=2, shape=20)+scale_x_continuous(trans = c("log10"))


####0.65
len <- length(result_NodeOOP_0.65)
i=1
SNPnum <- c()
accuracy_Gini_0.65 <- c()
accuracy_OOP_0.65 <- c()
while(i <= len){
    SNPnum <- c(SNPnum,length(result_NodePurity_0.65[[i]]))
    accuracy_Gini_0.65 <- c (accuracy_Gini_0.65,result_NodePurity_0.65[[i+1]])
    accuracy_OOP_0.65 <- c (accuracy_OOP_0.65,result_NodeOOP_0.65[[i+1]])
    
    i=i+2
}
dataFrame_test_0.65 <- data.frame(SNPnum=SNPnum,
                                 accuracy_Gini_0.65=accuracy_Gini_0.65,
                                 accuracy_OOP_0.65 =accuracy_OOP_0.65)

dataFrame_test_0.65_re <- melt(dataFrame_test_0.65,id="SNPnum")
ggplot(data=dataFrame_test_0.65_re, aes(x=SNPnum, y=value, colour=variable)) + geom_line()+
    geom_point(size=2, shape=20)+scale_x_continuous(trans = c("log10"))

####合并图像
dataFrame_all <- rbind(dataFrame_test_0.53_re,dataFrame_test_0.55_re,dataFrame_test_0.6_re,dataFrame_test_0.65_re)
ggplot(data=dataFrame_all, aes(x=SNPnum, y=value, colour=variable)) + geom_line()+
    geom_point(size=3, shape=20)+scale_x_continuous(trans = c("log10"))

##################结果验证question2
val_col_select <- result_NodePurity_0.6[[59]]
filterCol <- val_col_select
dataFilter <- test[,filterCol]
dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
dataFilter$phenotype <- as.factor(dataFilter$phenotype)

set.seed(2333)
inTrain <- createDataPartition(y=dataFilter$phenotype,p=0.7,list = FALSE)

training <- dataFilter[inTrain,]
testing <- dataFilter[-inTrain,]
modFit <- randomForest(phenotype~.,ntree=1500,data=training,importance=TRUE,do.trace=FALSE)
#modFit <- train(phenotype~.,method="rf",data=training, 
#trControl=trainControl(method = "cv",number = 5,verboseIter = TRUE))
Pred <- predict(modFit,newdata=testing)
confusionMatrix(testing$phenotype,Pred)

###############################################################################
#########                        question3 wrong
##############################################################################
set.seed(2333)
directory <- "D:/2016试题/B/B题附件/gene_info"
fileList <- list.files(directory)

dirPaths <- paste(directory,"/",fileList,sep = "")
temFile <- read.table(dirPaths[30])
temFile
gene_info_list <- list()

for(i in 1:length(dirPaths)){
    temFile <- read.table(dirPaths[i])
    gene_info_list <- c(gene_info_list,list(temFile[,1]))
}
names(gene_info_list) <- fileList

# All_SNPs <- colnames(genotype)
# length(All_SNPs)
# gene_info_intersect_list <- list()
# 
# for(i in 1:length(dirPaths)){
#     
#     gene_info_intersect_list <- c(gene_info_intersect_list,list(intersect(All_SNPs,gene_info_list[[i]])))
# }
names(gene_info_intersect_list) <- fileList

library(caret)
acc_container <- list()
for(i in 1:300){
    set.seed(2333)
    
    filterCol <- gene_info_list[[i]]
    dataFilter <- test[,filterCol]
    dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
    dataFilter$phenotype <- as.factor(dataFilter$phenotype)
    
    
    inTrain <- createDataPartition(y=test$phenotype,p=0.7,list = FALSE)
    training <- dataFilter[inTrain,]
    testing <- dataFilter[-inTrain,]
    
    modFit <- train(phenotype~.,method="rf",data=training, 
                    trControl=trainControl(method = "cv",number = 5,verboseIter = FALSE,
                                           seeds = list(1:3,1:3,1:3,1:3,1:3,2333)))
    
    Pred <- predict(modFit,newdata=testing)
    accuracy_cur <- confusionMatrix(Pred,testing$phenotype,positive = "1")
    print(i)
    
    acc_container=c(acc_container,list(id=i,conMatrix=accuracy_cur))
}

library(ggplot2)
data_frame_qes2 <- data.frame(id=1:300,accuracy=acc_container)
ggplot(data=data_frame_qes2,aes(x=id,y=accuracy)) + geom_line()

gene_select <- fileList[acc_container>0.55]
gene_info_list[gene_select][[1]]

set.seed(2333)

filterCol <- gene_info_list[[4]]
dataFilter <- test[,filterCol]
dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
dataFilter$phenotype <- as.factor(dataFilter$phenotype)


inTrain <- createDataPartition(y=test$phenotype,p=0.7,list = FALSE)
training <- dataFilter[inTrain,]
testing <- dataFilter[-inTrain,]

modFit <- train(phenotype~.,method="rf",data=training, 
                trControl=trainControl(method = "cv",number = 5,verboseIter = FALSE,
                                       seeds = list(1:3,1:3,1:3,1:3,1:3,2333)))

Pred <- predict(modFit,newdata=testing)
confusionMatrix(Pred,testing$phenotype,positive = "1")

#######################################################################
#######                     question3
#######################################################################
set.seed(2333)
directory <- "C:/Users/Hansel/Downloads/2016试题/B/B题附件/gene_info"
fileList <- list.files(directory)

dirPaths <- paste(directory,"/",fileList,sep = "")
temFile <- read.table(dirPaths[30])
temFile
gene_info_list <- list()

for(i in 1:length(dirPaths)){
    temFile <- read.table(dirPaths[i])
    gene_info_list <- c(gene_info_list,list(temFile[,1]))
}
names(gene_info_list) <- fileList

# library(caret)
# acc_container <- list()
# for(i in 1:300){
#     set.seed(2333)
#     
#     filterCol <- gene_info_list[[i]]
#     dataFilter <- test[,filterCol]
#     dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
#     dataFilter$phenotype <- as.factor(dataFilter$phenotype)
#     
#     
#     inTrain <- createDataPartition(y=test$phenotype,p=0.7,list = FALSE)
#     training <- dataFilter[inTrain,]
#     testing <- dataFilter[-inTrain,]
#     
#     #modFit <- train(phenotype~.,method="rf",data=training, 
#                     #trControl=trainControl(method = "cv",number = 5,verboseIter = FALSE,
#                                            #seeds = list(1:3,1:3,1:3,1:3,1:3,2333)))
#     modFit <- randomForest(phenotype~.,ntree=1500,
#                            data=training,importance=TRUE,do.trace=FALSE)
#     
#     Pred <- predict(modFit,newdata=testing)
#     accuracy_cur <- confusionMatrix(Pred,testing$phenotype,positive = "1")
#     print(i)
#     
#     acc_container=c(acc_container,list(id=i,conMatrix=accuracy_cur))
# }
# 
# 
# dataFrame_plot <- data.frame(id=acc_container[[1]],accuracy=acc_container[[2]]$overall[1],
#                              PValue=acc_container[[2]]$overall[6])
# i=3
# while(i<600){
#     dataFrame_plot <- rbind(dataFrame_plot,
#                             data.frame(id=acc_container[[i]],
#                                        accuracy=acc_container[[i+1]][["overall"]][1],
#                                        PValue=acc_container[[i+1]][["overall"]][6]))
#     i=i+2
# }
# 
# ggplot(dataFrame_plot,aes(x=id,y=accuracy))+geom_line()
#########################################
#####z找到与问题2位点相关基因
gene_index <- c()
gene_snp_list <- c()
for(i in 1:300){
    len <- length(intersect(as.character(gene_info_list[[i]]), result_NodePurity_0.6[[57]]))
    if(len >0){
        gene_index <- c(gene_index,i)
        gene_snp_list <- c(gene_snp_list,as.character(gene_info_list[[i]]))
    }
}

#####计算所有筛选基因联合预测值
filterCol <- gene_snp_list
dataFilter <- test[,filterCol]
dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
dataFilter$phenotype <- as.factor(dataFilter$phenotype)


inTrain <- createDataPartition(y=test$phenotype,p=0.7,list = FALSE)
training <- dataFilter[inTrain,]
testing <- dataFilter[-inTrain,]

#modFit <- train(phenotype~.,method="rf",data=training, 
#trControl=trainControl(method = "cv",number = 5,verboseIter = FALSE,
#seeds = list(1:3,1:3,1:3,1:3,1:3,2333)))
modFit <- randomForest(phenotype~.,ntree=1500,
                       data=training,importance=TRUE,do.trace=FALSE)
Pred <- predict(modFit,newdata=testing)
conFusinMat <- confusionMatrix(Pred,testing$phenotype,positive = "1")

#######依次去掉一个基因，计算剩余基因的预测值
acc_container_2 <- c()
for(i in 1:24){
    filterCol <- unlist(gene_info_list[gene_index[-i]])
    dataFilter <- test[,filterCol]
    dataFilter <- cbind(dataFilter,phenotype=test$phenotype)
    dataFilter$phenotype <- as.factor(dataFilter$phenotype)
    
    
    inTrain <- createDataPartition(y=test$phenotype,p=0.7,list = FALSE)
    training <- dataFilter[inTrain,]
    testing <- dataFilter[-inTrain,]
    
    #modFit <- train(phenotype~.,method="rf",data=training, 
    #trControl=trainControl(method = "cv",number = 5,verboseIter = FALSE,
    #seeds = list(1:3,1:3,1:3,1:3,1:3,2333)))
    modFit <- randomForest(phenotype~.,ntree=1500,
                           data=training,importance=TRUE,do.trace=FALSE)
    Pred <- predict(modFit,newdata=testing)
    conFusinMat <- confusionMatrix(Pred,testing$phenotype,positive = "1")
    acc_container_2 <- c(acc_container_2,conFusinMat$overall[1])
}

ques3_plot_list=data.frame(id=1:24,accuracy=acc_container_2)

gplot_ques3 <- ggplot(ques3_plot_list, aes(x=id,y=accuracy))+geom_point()
gplot_ques3+geom_hline(aes(yintercept=0.6033), colour="#990000")

ques3_gene_selct <- names(gene_info_list[gene_index])
########################################################################
#######                           question4
#########################################################################
multi_phenos <- read.table("C:/Users/Hansel/Downloads/2016试题/B/B题附件/multi_phenos.txt")

prob_matrix <- matrix(0,nrow = 10,ncol=9445)
for(i in 1:9445){
    for(j in 1:10){
        multi_phenos_val <- multi_phenos[,j]==0
        col_val <- test[,i]
        col_levels <-levels(col_val)
        pro_row <- c()
        for(l in 1:3){
            col_val_true <- col_val==col_levels[l]
            tem_Prob <- sum(multi_phenos_val==col_val_true)/1000
            pro_row <- c(pro_row,tem_Prob,1-tem_Prob)
            
        }
        
        prob_matrix[j,i] <- max(pro_row)
    }
}

# multi_phenos_one_true <- multi_phenos[,7]==0
# multi_phenos_one_false <- multi_phenos[,7]==1
# col_val <- test[,9]
# col_levels <-levels(col_val) 
# col_val_true <- col_val==col_levels[2]
# sum(multi_phenos_one_true==col_val_true)/1000
# sum(multi_phenos_one_false==col_val_true)/1000


######随机贪心算法
res_row <- c(-1:-10,0)
prob_matrix_test <- prob_matrix

res_data_list <- c()
for(i in 1:1000){
    res_row <- c(-1:-10,0)
    prob_matrix_test <- prob_matrix
    rand_seq <- sample(1:10,10,replace = FALSE)
    
    for(m in 1:10){
        res_row[rand_seq[m]] <- which.max(prob_matrix_test[rand_seq[m],])
        res_row[11]=+res_row[11]+prob_matrix_test[rand_seq[m],res_row[rand_seq[m]]]
        prob_matrix_test=prob_matrix_test[,-res_row[rand_seq[m]]]
    }
    res_data_list <- c(res_data_list,res_row)
}

res_data_frame <- data.frame(matrix(res_data_list,nrow = 1000,ncol = 11,byrow = TRUE))

ggplot(data=res_data_frame,aes(x=1:1000,y=X11)) + geom_line()

##############################################
#####将表型进行编码

# multi_labels=c()

# for(i in 1:1000){
#     multi_labels <- c(multi_labels,1*multi_phenos[i,1]+
#                           2*multi_phenos[i,2]+
#                           4*multi_phenos[i,3]+
#                           8*multi_phenos[i,4]+
#                           16*multi_phenos[i,5]+
#                           32*multi_phenos[i,6]+
#                           64*multi_phenos[i,7]+
#                           128*multi_phenos[i,8]+
#                           256*multi_phenos[i,9]+
#                           512*multi_phenos[i,10])
# }
snp_index <- unique(unlist(c(res_data_frame[1:1000,1:10])))
data_ques4 <- re_data
# data_ques4$phenotype <- multi_labels

dataFilter <- data_ques4[,snp_index]
#X_matrix <- as.matrix(data_ques4[,1:9445])
X_matrix <- as.matrix(data_ques4[,snp_index])
Y_matrix <- as.matrix(multi_phenos)
# dataFilter <- cbind(dataFilter,phenotype=data_ques4$phenotype)
# dataFilter$phenotype <- dataFilter$phenotype


# set.seed(2333)
# inTrain <- createDataPartition(y=dataFilter$phenotype,p=0.7,list = FALSE)
# training <- dataFilter[inTrain,]
# testing <- dataFilter[-inTrain,]
# modFit <- randomForest(phenotype~.,ntree=1000,
#                        data=training,importance=TRUE,do.trace=TRUE)

pls_modFit <- plsr(Y_matrix~X_matrix,ncomp=10,validation="LOO",jackknife=TRUE)
summary(pls_modFit,what = "all")

coef(pls_modFit)
predplot(pls_modFit)

ques4_result <- jack.test(pls_modFit,ncomp = 10)
ques4_pvalue <- as.data.frame(ques4_result$pvalues)
names(ques4_pvalue) <-  c("V1","V2","v3","v4","v5",
                          "v6","v7","v8","v9","v10")

ques4_pvalue_rownames <- row.names(ques4_pvalue)
ques4_pvalue_scole <- ques4_pvalue
for(i in 1:47){
    for(j in 1:10){
        if(ques4_pvalue_scole[i,j]<=0.001){
            ques4_pvalue_scole[i,j]=1
        }else if(ques4_pvalue_scole[i,j]<=0.01){
            ques4_pvalue_scole[i,j]=2
        }else if(ques4_pvalue_scole[i,j]<=0.05){
            ques4_pvalue_scole[i,j]=3
        }else if(ques4_pvalue_scole[i,j]<=0.1){
            ques4_pvalue_scole[i,j]=4
        }else{
            ques4_pvalue_scole[i,j]=5
        }
    }
}
frame_index <- c()
for(i in 1:47){
    sum_score =sum(ques4_pvalue_scole[i,])
    if((sum_score==50)){
        frame_index <- c(frame_index,i)
    }
}

ques4_result_plot <- ques4_pvalue_scole[-frame_index,]
colnames(ques4_result_plot) <- c("V1","V2","v3","v4","v5",
                                "v6","v7","v8","v9","v10")
ques4_result_plot_melt <- melt(as.matrix(ques4_result_plot))


p <- ggplot(data = ques4_result_plot_melt) +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) +
    theme_classic() + 
    theme(axis.ticks = element_blank(),
          axis.line = element_blank()) + 
    xlab('位点') +
    ylab('性状')
print(p)

write.csv(ques4_pvalue[-frame_index,],file = "pvalue")