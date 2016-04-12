# sparkR
renlanchuke  
2016年4月12日  

加载sparkR,spark环境变量已经预先设置

```r
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
```

```
## 
## Attaching package: 'SparkR'
## 
## The following objects are masked from 'package:stats':
## 
##     cov, filter, lag, na.omit, predict, sd, var
## 
## The following objects are masked from 'package:base':
## 
##     colnames, colnames<-, intersect, rank, rbind, sample, subset,
##     summary, table, transform
```

```r
#设定内存大小
sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="8g"))
```

```
## Launching java with spark-submit command F:\spark-1.6.1/bin/spark-submit.cmd   --driver-memory "8g" sparkr-shell C:\Users\Hansel\AppData\Local\Temp\RtmpGeh5oW\backend_port1c0c722a751b
```
context是r与spark集群的交互的入口

```r
sc <- sparkR.init()
```

```
## Re-using existing Spark Context. Please stop SparkR with sparkR.stop() or restart R to create a new Spark Context
```

```r
sqlContext <- sparkRSQL.init(sc)
```
创建dataframe

```r
df <- createDataFrame(sqlContext, faithful)
head(df)
```

```
##   eruptions waiting
## 1     3.600      79
## 2     1.800      54
## 3     3.333      74
## 4     2.283      62
## 5     4.533      85
## 6     2.883      55
```
