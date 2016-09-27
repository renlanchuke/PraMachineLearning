#paper plots
library(ggplot2)
sigma <- function(x) log2(x)
x <- seq(0,50,length.out = 1000)
y <- sigma(x)
plotData <- data.frame(x=x,y=y)
qplot(x=x,y=y,data=plotData)


cooperator <- function(N,k,gama){
    cost =gama*(k-N)/N
    payoff=sigma(k)
    payoff-cost
}

cooperator(2,6,0.9)
