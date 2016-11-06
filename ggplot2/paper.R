#paper plots
library(ggplot2)

##################################################################################################
#####计算原始状态x均衡值
sstate<- function(x,N,k,c){
   (1-(1-x)^N)/(N*x*(1-x)^(N-1))-log2(k)/(c*(k-N))
}

pstate <- function(x,N,k,c){
    log2(k)*(1-x)^(N-1)/2+c*(k-1)/2-c*(k-N)*(1-(1-x)^N)/(N*x)
}

b_c_rate <- function(k,N,c){
    log(k)/(c*(k-N))
}

x_index <- seq(0.0001,0.9999,by=0.01)

y_index <- pstate(x_index,4,20,0.4)
plot(x=x_index,y_index)
plot_x_k <- function(N,c,len){
    k_array=(N+1):(N+len)
    results = c()
 
    for(i in k_array){
        temRes = tryCatch({
            uniroot(sstate,c(0.0001,0.9999),N=N,k=i,c=c,tol = 0.0001)$root
               
        },  error=function(e){
            print("ok")
            0
        })
        results <- c(results,temRes)
    }
    resultFrame <- data.frame(k=k_array[1:length(results)],x_eq=results)
  
}

plot_x_k2 <- function(N,c,len){
    k_array=(N+1):(N+len)
   
    results2=c()
    for(i in k_array){
       temRes=tryCatch({
           temRes =uniroot(pstate,c(0.0001,0.9999),N=N,k=i,c=c,tol = 0.0001)$root
           
            },error=function(e){
                1
            })
       results2=c(results2,temRes)
       
        
    }
    resultFrame <- data.frame(k=k_array,x_eq=results2)
  
}

resFrame <- plot_x_k(5,0.4,10)
resFrame2 <- plot_x_k2(5,0.4,10)
resFrame3 <-b_c_rate(resFrame$k,5,0.4)
resFrame3 <- data.frame(k=resFrame$k,x_eq=resFrame3)


###计算惩罚机制下均衡解


# frame_bind <- cbind(resFrame,resFrame2,resFrame3)
# names(frame_bind) <- c("k1","x1","k2","x2","k3","x3")

# p <- ggplot(data = frame_bind)+
#     geom_point(aes(x=k1,y=x1),size=4, shape=20,colour="red")+
#     geom_line(aes(x=k1,y=x1),colour="red")+
#     geom_point(aes(x=k2,y=x2),size=4, shape=15,colour="green")+
#     geom_line(aes(x=k2,y=x2),colour="green")+
#     geom_point(aes(x=k3,y=x3),size=4, shape=10,colour="blue")+
#     geom_line(aes(x=k3,y=x3),colour="blue")+xlab("K")+ylab("Frequency of Cooperation")
# 
# p <- p+theme(legend.position="right")

data_rbind <- rbind(resFrame2,resFrame)
label <- c(rep("P_DGG",10),rep("DGG",10))
data_rbind[,"label"] <- as.factor(label)

P <- ggplot(data = data_rbind,aes(x=k,y=x_eq,color=label,shape=label))+
    geom_line()+geom_point(size=5)
previous_theme <- theme_set(theme_bw(base_size = 20))
P <- P+xlab("K")+ylab("Frequency of Cooperation")
p <- P+theme(legend.position=c(0.9,0.8),legend.title=element_blank())
p <- p+scale_x_continuous(breaks=6:15)


###########################
#####compare c vs x
resFrame <- plot_x_k(5,0.1,20)
resFrame2 <- plot_x_k(5,0.2,20)
resFrame3 <- plot_x_k(5,0.3,20)


qplot(data=resFrame3,x=k,y=x_eq)
data_rbind2 <- rbind(resFrame,resFrame2,resFrame3)
label <- c(rep("c=0.1",dim(resFrame)[1]),
               rep("c=0.2",dim(resFrame2)[1]),rep("c=0.3",dim(resFrame3)[1]))
data_rbind2[,"label"] <- as.factor(label)

P <- ggplot(data = data_rbind2,aes(x=k,y=x_eq,color=label,shape=label))+
    geom_line()+geom_point(size=4)

P <- P+xlab("c")+ylab("Frequency of Cooperation")
p <- P+theme(legend.position=c(0.9,0.8),legend.title=element_blank())

######################################################################################
#####dynamic evolution simulation
iniProb <- 0.3
poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
poplation <- sample(poplation)
payoff <- rep(0,100)
k=15
N=5
c=0.4

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
    agent = sample(1:2000,5,replace = TRUE)
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
        if(trans_prob < -1){
            trans_prob=-1
        }
        poplation[agent_i$ind]=sample(c(poplation[agent_i$ind],poplation[agent_j$ind]),
                                      1,prob = c(1+trans_prob,-trans_prob))
    }
    if(trans_prob>0){
        if(trans_prob > 1){
            trans_prob=1
        }
        poplation[agent_j$ind]=sample(c(poplation[agent_j$ind],poplation[agent_i$ind]),
                                      1,prob = c(1-trans_prob,trans_prob))
    }

    
    coop_array[i]=sum(poplation)
}

coop_frame <- data.frame(coop_array)
qplot(x=1:100000,y=coop_frame$coop_array/2000,data = coop_frame)


# feimi <- function(x){
#     beta=0.9
#     1/(1+exp(-beta*x))
# }
simu_result=c()
for(k in 6:15){
    b_payoff <- log2(k)
    cooper=0
    for(j in 1:50){
        iniProb <- 0.3
        poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
        poplation <- sample(poplation)
        print(sum(poplation))
        N <- 5
        
        for(i in 1:100000){
            agent_i <- getAgentPayoff()
            agent_j <- getAgentPayoff()
            
            trans_prob <- (agent_i$pay-agent_j$pay)/b_payoff
            
            if(trans_prob < 0){
                if(trans_prob < -1){
                    trans_prob=-1
                }
                poplation[agent_i$ind]=sample(c(poplation[agent_i$ind],poplation[agent_j$ind]),
                                              1,prob = c(1+trans_prob,-trans_prob))
            }
            if(trans_prob>0){
                if(trans_prob > 1){
                    trans_prob=1
                }
                poplation[agent_j$ind]=sample(c(poplation[agent_j$ind],poplation[agent_i$ind]),
                                              1,prob = c(1-trans_prob,trans_prob))
            }
        }
        cooper=cooper+sum(poplation)
    }
    print(cooper/(2000*50))
    simu_result <- c(simu_result,cooper/(2000*50))
}



   
#####################################################################################
#####penalty simunation
set.seed(2333)
iniProb <- 0.5
poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
poplation <- sample(poplation)
record <- rep(0,2000)
k=6
N=5
c=0.4

b_payoff <- log2(k)
cooperater <- function(NC){
    b_payoff-c*(k-N)/NC
}

defector <- function(NC){
    if(NC>0){
        b_payoff
    }else{
        0
    }
}

getAgentPayoff <- function(){
    index_array=1:2000
    agents1 = sample(index_array,5,replace = FALSE)
    index=agents1[1]
    sum=getPayoff(agents1)
    
#     agents2=sample(index_array[index_array!=index],5,replace = FALSE)
#     agents2=c(index,agents2)
#     sum=sum+getPayoff(agents2)
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
#######penalty2
set.seed(2333)
iniProb <- 0.2
poplation <- c(rep(1,iniProb*2000),rep(0,2000-iniProb*2000))
poplation <- sample(poplation)
record <- rep(0,2000)
k=6
N=5
c=0.1


cooperater <- function(NC){
    b_payoff-c*(k-N)/NC
}

defector <- function(NC){
    if(NC>0){
        b_payoff
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
    
    agents3=sample(index_array[index_array!=index],5,replace = FALSE)
    agents3=c(index,agents3)
    sum=sum+getPayoff(agents3)
    
    agents4=sample(index_array[index_array!=index],5,replace = FALSE)
    agents4=c(index,agents4)
    sum=sum+getPayoff(agents4)
    
    agentPayoff <- list(ind=index,pay=sum/4)
}

getPayoff <- function(agents){
   
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
        
        if((record[agents[1]])==1){
            0
        }
        tem_agents1 = agents[record[agents]==0]
        tem_agents2 = agents[record[agents]==1]

        record[tem_agents1]=1-poplation[tem_agents1]
        record[tem_agents2]=0
        payoff
    
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
