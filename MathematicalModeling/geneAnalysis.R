################################################################################
#####问题一、请用适当的方法，把genotype.dat中每个位点的碱基(A,T,C,G) 
#####编码方式转化成数值编码方式，便于进行数据分析
################################################################################

#####加载数据，进行预处理
geneDataUrl <- "F:/gitRespository/PraMachineLearning/MathematicalModeling/genesData"
fileName<-file.path(geneDataUrl,"genotype.dat")
genotype <- read.table(fileName,header = TRUE)
fileName <- file.path(geneDataUrl,"phenotype.txt")
phenotype <- read.table(fileName,header=FALSE,sep="\n")
#####数据错误列修正
for(i in 1:(dim(genotype)[2])){
    if(levels(genotype[,i])==c("DD","ID","II")) {
        levels(genotype[,i]) <- c("CC","TC","TT")
    }
}

genesDataQ1 <- genotype
genesDataQ1 <- sapply(genotype, function(e){
    levels(e) <- c("1","2","3")
    as.integer(e)
})
################################################################################
#####问题二、根据附录中1000个样本在某条有可能致病的染色体片段上
#####的9445个位点的编码信息(见genotype.dat)和样本患有遗传疾病A的
#####信息（见phenotype.txt文件）。设计或采用一个方法，找出某种疾
#####病最有可能的一个或几个致病位点，并给出相关的理论依据。
#####编码方式转化成数值编码方式，便于进行数据分析
################################################################################

######分别对每一个SNP位点作显著性校验
library(limma)
library(caret)
modLimma <- model.matrix(~phenotype$V1)
fitLimma <- lmFit(t(genesDataQ1),modLimma)
ebayes_limma <- ebayes(fitLimma)
p_value <- ebayes_limma$p.value
#p_value_coef_BH <-p.adjust(p_value[,2],method = "BH")
sig_col <- p_value[,2]<0.05

genesDataQ1_sub <- genesDataQ1[,sig_col]

#####随机森林分类
data_rf <- data.frame(genesDataQ1_sub,phenotype=phenotype$V1)
data_rf$phenotype <- as.factor(data_rf$phenotype)
row_index <- sample(1:1000,1000)
data_rf <- data_rf[row_index,]

set.seed(2333)
inTrain <- createDataPartition(y=data_rf$phenotype,p=0.7,list = FALSE)

training <- data_rf[inTrain,]
testing <- data_rf[-inTrain,]

modFit <- train(phenotype~.,method="rf",data=training, 
                trControl=trainControl(method = "cv",number = 10,verboseIter = TRUE))

Pred <- predict(modFit,newdata=testing)
confusionMatrix(testing$phenotype,Pred)

filterCol <- colnames(genesDataQ1_sub)
#####利用袋外错误率筛选属性
result_container <- c()
while(length(filterCol)>2){
    
    modFit = randomForest(phenotype~.,ntree=1500,data=training,importance=TRUE,do.trace=FALSE)
    Pred = predict(modFit,newdata=testing)
    accuracy_cur = sum(testing$phenotype == Pred)/length(Pred)
    print(accuracy_cur)
    val_Imp = importance(modFit,1)
    tem_colNames = row.names(val_Imp)
    new_colNames = tem_colNames[val_Imp<=quantile(val_Imp,0.9)]
    
    result_container <- c(result_container,list(valNames=filterCol,accuracy=accuracy_cur))
    filterCol=new_colNames
    training = cbind(training[,filterCol],phenotype=training$phenotype)
    testing = cbind(testing[,filterCol],phenotype=testing$phenotype)
    
}



