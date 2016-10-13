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

resFrame <- plot_x_k(5,0.2,20)
qplot(x=k,y=x_eq,data = resFrame)

resFrame2 <- plot_x_k2(5,4,20)
qplot(x=k,y=x_eq,data = resFrame2)
######################################################################################
#####dynamic evolution simulation
set.seed(2333)
iniProb <- 0.6
poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
poplation <- sample(poplation)
payoff <- rep(0,100)
k=6
N=5
c=0.2

b_payoff <- log2(k)
cooperater <- function(NC){
    log2(k)-c*(k-N)/NC
}

defector <- function(NC){
    if(NC>0){
        log2(k)
    }else{
        0
    }
}

getAgentPayoff <- function(){
    agent = sample(1:2000,6,replace = TRUE)
    NC = sum(poplation[agent])
    
    index = sample(agent,1)
    if(NC==0){
        payoff=0
    }else{
        if(poplation[index]==0){
            payoff=defector(NC)
        }else{
            payoff=cooperater(NC)
        }
    }
    
    agentPayoff <- list(ind=index,pay=payoff)
}

coop_array <- rep(-1,100000)
for(i in 1:100000){
    agent_i <- getAgentPayoff()
    agent_j <- getAgentPayoff()
    
    trans_prob <- (agent_i$pay-agent_j$pay)/b_payoff
    
    if(trans_prob < 0){
        poplation[agent_i$ind]=sample(c(poplation[agent_i$ind],poplation[agent_j$ind]),
                                      1,prob = c(1+trans_prob,-trans_prob))
    }
    if(trans_prob>0){
        poplation[agent_j$ind]=sample(c(poplation[agent_j$ind],poplation[agent_i$ind]),
                                      1,prob = c(1-trans_prob,trans_prob))
    }

    
    coop_array[i]=sum(poplation)
}

coop_frame <- data.frame(ind=1:100000,sum=coop_array)
qplot(x=ind,y=sum/2000,data = coop_frame)

feimi <- function(x){
    beta=0.9
    1/(1+exp(-beta*x))
}
# for(i in agent){
#     if(NC==0){
#         payoff[i]=0
#     }
#     else{
#         if(poplation[i]==0){
#             payoff[i]=defector(NC)
#         }else{
#             payoff[i]=cooperater(NC)
#         }
#     }
# }

#####################################################################################
#####penalty simunation
set.seed(2333)
iniProb <- 0.8
poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
poplation <- sample(poplation)
record <- rep(0,2000)
k=6
N=5
c=0.1

b_payoff <- log2(k)
cooperater <- function(NC){
    log2(k)-c*(k-N)/NC
}

defector <- function(NC){
    if(NC>0){
        log2(k)
    }else{
        0
    }
}

getAgentPayoff <- function(){
    index_array=1:2000
    agents1 = sample(index_array,6,replace = FALSE)
    index=agents1[1]
    sum=getPayoff(agents1)
    
    agents2=sample(index_array[index_array!=index],5,replace = FALSE)
    agents2=c(index,agents2)
    sum=sum+getPayoff(agents2)
    agentPayoff <- list(ind=index,pay=sum/2)
}

getPayoff <- function(agents){
    if(sum(record[agents])>0){
        record[agents]=0
        0
    }else{
        NC = sum(poplation[agents])
        index=agents[1]
        if(NC==0){
            payoff=0
        }else{
            if(poplation[index]==0){
                payoff=defector(NC)
            }else{
                payoff=cooperater(NC)
            }
        }
        record[agents]=1-poplation[agents]
        payoff
    }
}

coop_array <- rep(-1,100000)
for(i in 1:100000){
    agent_i <- getAgentPayoff()
    agent_j <- getAgentPayoff()
    
    trans_prob <- (agent_i$pay-agent_j$pay)/b_payoff
    
    if(trans_prob < 0){
        poplation[agent_i$ind]=sample(c(poplation[agent_i$ind],poplation[agent_j$ind]),
                                      1,prob = c(1+trans_prob,-trans_prob))
    }
    if(trans_prob>0){
        poplation[agent_j$ind]=sample(c(poplation[agent_j$ind],poplation[agent_i$ind]),
                                      1,prob = c(1-trans_prob,trans_prob))
    }
    
    
    coop_array[i]=sum(poplation)
}

coop_frame <- data.frame(ind=1:100000,sum=coop_array)
qplot(x=ind,y=sum/2000,data = coop_frame)
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
