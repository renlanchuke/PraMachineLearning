#paper plots
library(ggplot2)

##################################################################################################
#####计算原始状态x均衡值
sstate<- function(x,N,k,c){
   (1-(1-x)^N)/(N*x*(1-x)^(N-1))-log2(k)/(c*(k-N))
}

pstate <- function(x,N,k,c){
    (1-(1-x)^N)/(N*x*(1-x)^(N-1))-log2(k)/(2*c*(k-N))-log2(k)/(2*c*(k-N)*(1-x)^(N-1))
}


plot_x_k <- function(N,c,len){
    k_array=(N+1):(N+len)
    results=c()
    results2=c()
    for(i in k_array){
        temRes =uniroot(sstate,c(0.0001,0.9999),N=N,k=i,c=c,tol = 0.0001)
      
        results=c(results,temRes$root)
    }
    resultFrame <- data.frame(k=k_array,x_eq=results)
  
}

plot_x_k2 <- function(N,c,len){
    k_array=(N+1):(N+len)
   
    results2=c()
    for(i in k_array){
       
        temRes2 =uniroot(pstate,c(0.0001,0.9999),N=N,k=i,c=c,tol = 0.0001)
        
        results2=c(results2,temRes2$root)
        
    }
    resultFrame <- data.frame(k=k_array,x_eq=results2)
  
}

resFrame <- plot_x_k(5,0.1,20)
qplot(x=k,y=x_eq,data = resFrame)

resFrame2 <- plot_x_k2(5,4,20)
qplot(x=k,y=x_eq,data = resFrame2)
######################################################################################
#####dynamic evolution simulation
set.seed(2333)
iniProb <- 0.3
poplation <- sample(c(rep(1,iniProb*100),rep(0,(1-iniProb)*100)))

N=6
agent <- sample(1:100,6)

#############################################################
#######test
f1 <- function(x,a,b) a*x+b
 
a <- 5
b <- -10
uniroot(f1,c(0,10),a=a,b=b,tol=0.0001)

testfunc <- function(x,N){
    (1-(1-x)^N)/(N*x*(1-x)^(N-1))
}

x_test <- seq(0.00001,0.99999,0.01)
y_test <- c()

for(i in x_test){
    y_test=c(y_test,testfunc(i,6))
}
y_test_kog <- log2(y_test)
qplot(x=x_test[1:80],y=y_test_kog[1:80])
