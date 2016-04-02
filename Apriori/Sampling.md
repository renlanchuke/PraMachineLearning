# Sampling
renlanchuke  
2016年4月2日  

来源:[Introduction to arules – A computational environment for mining
association rules and frequent item sets][1]


```r
#加载arules包
library(arules)
```

```
## Loading required package: Matrix
## 
## Attaching package: 'arules'
## 
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
```

```r
data("Adult")
```


```r
supp <- 0.05
epsilon <- 0.1
c <- 0.1
n <- -2 * log(c)/ (supp * epsilon^2)
n
```

```
## [1] 9210.34
```


```r
AdultSample <- sample(Adult, n, replace = TRUE)
```
支持度大于0.05的频集图

```r
itemFrequencyPlot(AdultSample, population = Adult, support = supp,
cex.names = 0.7)
```

![](Sampling_files/figure-html/unnamed-chunk-4-1.png) 


```r
itemFrequencyPlot(AdultSample, population = Adult,support = supp, lift = TRUE,cex.names = 0.9)
```

![](Sampling_files/figure-html/unnamed-chunk-5-1.png) 

USER时间就是执行表达式消耗的时间(CPU时间)

```r
time <- system.time(itemsets <- eclat(Adult,
parameter = list(support = supp), control = list(verbose = FALSE)))
time
```

```
##    user  system elapsed 
##    0.44    0.00    0.43
```


```r
timeSample <- system.time(itemsetsSample <- eclat(AdultSample,
parameter = list(support = supp), control = list(verbose = FALSE)))
timeSample
```

```
##    user  system elapsed 
##    0.11    0.00    0.11
```

```r
itemsets
```

```
## set of 8496 itemsets
```

```r
itemsetsSample
```

```
## set of 8443 itemsets
```


```r
match <- match(itemsets, itemsetsSample, nomatch = 0)
sum(match > 0) / length(itemsets)
```

```
## [1] 0.9676318
```


```r
summary(quality(itemsets[match == 0])$support)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.05002 0.05057 0.05123 0.05148 0.05215 0.05553
```

```r
summary(quality(itemsetsSample[-match])$support)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.05005 0.05049 0.05092 0.05129 0.05179 0.05603
```


```r
supportItemsets <- quality(itemsets[which(match > 0)])$support
supportSample <- quality(itemsetsSample[match])$support
accuracy <- 1 - abs(supportSample - supportItemsets) / supportItemsets
summary(accuracy)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.8519  0.9562  0.9754  0.9702  0.9895  1.0000
```

[1]:https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf
