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
## Launching java with spark-submit command F:\spark-1.6.1/bin/spark-submit.cmd   --driver-memory "8g" sparkr-shell C:\Users\Hansel\AppData\Local\Temp\RtmpCofJCs\backend_port283c646741f6
```
context是r与spark集群的交互的入口,所以首先创建一个sqlContext

```r
sqlContext <- sparkRSQL.init(sc)
```
###创建dataframe
在sqlContext中，可以创建dataframes数据结构，数据可以是本地数据，也可以Hive table
从本地创建dataframe

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


从数据源创建dataframe

```r
sc <- sparkR.init(sparkPackages="com.databricks:spark-csv_2.11:1.0.3")
```

```
## Re-using existing Spark Context. Please stop SparkR with sparkR.stop() or restart R to create a new Spark Context
```

```r
sqlContext <- sparkRSQL.init(sc)
```

导入json数据

```r
#read.df可以从数据源创建dataframe
people <- read.df(sqlContext, "F:\\spark-1.6.1\\examples\\src\\main\\resources\\people.json", "json")
head(people)
```

```
##   age    name
## 1  NA Michael
## 2  30    Andy
## 3  19  Justin
```

```r
printSchema(people)
```

```
## root
##  |-- age: long (nullable = true)
##  |-- name: string (nullable = true)
```

将数据写入Parquet文件

```r
write.df(people,path="people.parquet",source="parquet",mode="overwrite")
```

```
## NULL
```
