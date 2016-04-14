# SparkDataFrame
renlanchuke  
2016年4月14日  

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
## Launching java with spark-submit command F:\spark-1.6.1/bin/spark-submit.cmd   --driver-memory "8g" sparkr-shell C:\Users\Hansel\AppData\Local\Temp\RtmpAJ7306\backend_port1c8852ed2b1a
```


```r
sqlContext <- sparkRSQL.init(sc)
df <- createDataFrame(sqlContext,faithful)
```
