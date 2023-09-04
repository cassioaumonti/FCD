

# Arrival process: describes how the customers arrive to the system, and the 
#distribution of the customers’ arrival

# Service mechanism: is articulated by the number of servers, and whether each server has 
#its own queue or there is one queue feeding all servers, and the distribution of customer’s service times

# Queue discipline: refers to the rule that a server uses to choose the next customer 
#from the queue when the server completes the service of the current customer (e.g. FIFO: first-in, first-out; LIFO: last-in, first-out; priority-based; random selection)

#(lambda): average number or arrivals per time period (i.e. mean arrival rate)

# µ (mu): average number of customers served per time period (i.e. mean service rate)

# The standard notation system to classify a queuing system is A/B/c/k/m
#A represents the probability distribution for the arrival process

#B represents the probability distribution for the service process

#c represents the number of channels (servers)
 
#k represents the maximum number of customer allowed in the queueing system

#m represents the maximum number of customers in total

# Common options for A and B are:
#M for a Poisson arrival distribution (thus, exponential interarrival distribution) or 
#an exponential service time distribution

#D for a deterministic or constant value

#Ek for an Erlang distribution of order k

# G for a general distribution with known mean and variance

# When k and m are not specified, it is assumed they are infinite.

# Summary Output Nomenclatures
# RO (ρ): overall system utilization
# P0: the probability that all servers are idle
# Lq: long-run average number of customers in the queue
# Wq: long-run average time spent in the queue
# X: system throughput
# L: long-run average number of customers in the system
# W: long-run average time spent in the system
# Wqq: long-run average time spent in queue when there is queue in a queueing model
# Lqq: long-run average number of customers in queue when there is queue in a queueing model



# Ex: simplest queuing system: M/M/1, with a Poisson arrival rate of 3 customers 
#per minute, an exponential service time of 4 customers per minute and a single server:
# Import queueing package
library(queueing)

queue = function(x1,y,z){

  # Set queue model input parameters
  input_mm1 <- NewInput.MMC(lambda = x1, mu = y, c = z, n = 0) # n = 0 idle probability
  
  # Create queue class object
  output_mm1 <- QueueingModel(input_mm1)
  
  # Get queue model report
  #Report(output_mm1)
  
  # Get queue model summary
  summary(output_mm1)
  
  # Poisson Distribution Plot for Arrival Process
  # curve(dpois(x, input_mm1$lambda),
  #       from = 0,
  #       to = 20,
  #       type = "b",
  #       lwd = 2,
  #       xlab = "Number of customers",
  #       ylab = "Probability",
  #       main = "Poisson Distribution for Arrival Process",
  #       ylim = c(0, 0.25),
  #       n = 21)
  
  # Exponential Distribution Plot for Interarrival Time
  # curve(dexp(x, rate = 1/input_mm1$lambda),
  #       from = 0,
  #       to = 10,
  #       type = "l",
  #       lwd = 2,
  #       xlab = "Interarrival Time",
  #       ylab = "Probaility",
  #       main = "Exponential Distribution for Interarrival Time",
  #       ylim = c(0, 1))
  # abline(h = 0)
  # 
  # Exponential Distribution Plot for Service Process
  # p = curve(dexp(x, rate = input_mm1$mu),
  #       from = 0, 
  #       to = 5, 
  #       type = "l", 
  #       lwd = 2,
  #       xlab = "Service Waiting Time",
  #       ylab = "Probaility",
  #       main = "Exponential Distribution for Service Process",
  #       ylim = c(0, 1))
  # abline(h = 0)
  # 
}

data = read.table("clipboard",h=F)[,-2]
colnames(data)=c("lambda","mu")

c_b = 6
c_a = 3
c_unload = 6
flagb = flaga = flagu = T
for(i in 1:nrow(data)){
  if(i <= 4){
    b = queue(x1 = data[i,1],y = data[i,2],z = c_b)
    nm = names(b[[1]])
    b = unlist(b)
    names(b) = nm
    b = b[-c(1:5)]
    if(flagb == T){
      out_b = b
      flagb = F
    }else{
      out_b = rbind(out_b,b)
    }
  }else if(i > 4 & i <= 8){
    a = queue(x1 = data[i,1],y = data[i,2],z = c_a)
    nm = names(a[[1]])
    a = unlist(a)
    names(a) = nm
    a = a[-c(1:5)]
    if(flaga == T){
      out_a = a
      flaga = F
    }else{
      out_a = rbind(out_a,a)
    }
  }else{
    un = queue(x1 = data[i,1],y = data[i,2],z = c_unload)
    nm = names(un[[1]])
    un = unlist(un)
    names(un) = nm
    un = un[-c(1:5)]
    if(flagu == T){
      out_un = un
      flagu = F
    }else{
      out_un = rbind(out_un,un)
    }
  }
}

out = rbind(out_b, out_a, out_un)
write.table(out,"out.txt")

# Poisson Distribution Plot for Arrival Process
plot(dpois(1:5, lmbd_b[1]),t="l")
lines(dpois(1:5, lmbd_b[2]),col=2)
lines(dpois(1:5, lmbd_b[3]), col=3)
lines(dpois(1:5, lmbd_b[4]), col=4)

plot(dpois(1:5, lmbd_a[1]),t="l")
lines(dpois(1:5, lmbd_a[2]),col=2)
lines(dpois(1:5, lmbd_a[3]), col=3)
lines(dpois(1:5, lmbd_a[4]), col=4)


# LP model
lmbd_b = c(.75, .62, .54, .47)
mu_b = .56
c_b = 6
queue(x1 = lmbd_b[4],y = mu_b,z = c_b)

lmbd_a = c(.77, .71, .66, .61)
mu_a = .99
c_a = 3
queue(x1 = lmbd_a[4],y = mu_a,z = c_a)




# To solve a transport problem using a fuzzy inference system in R, we can use the
#FuzzyR package. Here's an example code:

library(FuzzyR)
library(tidyverse)

# qt = queuing time
# prane = proportion of trucks at crane
crane_decisionMaker = function(qt,pcrane,...){
  # fuzzy model for crane 1-orientation: industry -> stands (loading cranes)
  fis = newfis("crane",...)
  fis = addvar(fis, "input", "Qtime", c(0, 100)) # queuing time - number of hours
  fis = addvar(fis, "input", "Loadcrane", c(0, 1)) # proportion of trucks at crane2
  fis = addvar(fis, "output", "delay", c(0, 1)) # hours of delay for the truck i
  
  
  # queuing time
  fis = addmf(fis, "input", 1, "low", "trapmf", 
              c(0, 0, 2, 2.5))
  fis = addmf(fis, "input", 1, "medium", "trapmf", 
              c(2, 2.5, 3.5, 4))
  fis = addmf(fis, "input", 1, "high", "trapmf", 
              c(3.5, 4, 4.5, 100))
  # plotmf(fis,"input",1)
  

  # proportion of trucks
  fis = addmf(fis, "input", 2, "low", "trapmf", 
              c(0, 0, 0.3, 0.40))
  fis = addmf(fis, "input", 2, "medium", "trapmf", 
              c(0.30, 0.40, 0.50, 0.60))
  fis = addmf(fis, "input", 2, "high", "trapmf", 
              c(0.50, 0.60, 1, 1))
  # plotmf(fis,"input",2)
  

  # delay crane 1- hours of delay for crane 1
  fis = addmf(fis, "output", 1, "very low", "trimf", 
              c(0, 0.2, 0.4))
  fis = addmf(fis, "output", 1, "low", "trimf", 
              c(0.3, 0.5, 0.7))
  fis = addmf(fis, "output", 1, "medium", "trimf", 
              c(0.6, 0.8, 1))
  fis = addmf(fis, "output", 1, "high", "trimf", 
              c(0.7, 0.9, 1))
  fis = addmf(fis, "output", 1, "very high", "trimf", 
              c(0.8, 1, 1))
# plotmf(fis, "output", 1)

  qtime2=c(3,2,1)
  propc1=c(3,2,1)
  
  qlt = expand.grid(qtime2,propc1)
  
  # delay
  # rule = c(3,3,2,
  #          3,2,1,
  #          3,2,1)

  rule = c(5,4,3,
           4,3,2,
           3,2,1)
  
    
  # qlt = cbind(qlt,hsi1)
  qlt = cbind(qlt,rule)
  qlt$w = 1
  qlt$op = 1
  names(qlt)=c("[,1]","[,2]","[,3]","[,4]","[,5]")
  qlt = as.matrix(qlt)
  rl = qlt
  
  fis = addrule(fis, rl)
  
  # showfis(fis)
  # showGUI(fis)
  # 
  # qt = 12.6
  # pcrane = 0.29
  
  number_inputs = 2
  
  Input_data <- matrix(c(qt,pcrane),1,number_inputs)
  eval = evalfis(Input_data, fis,...)
  
  return(eval)
  
}

# crane_decisionMaker(qt = q[1], pcrane = crane[1])

# graphs of membership functions
graphs = function(){
  
  CCmfp <- genmf('trapmf', c(0, 0, 2, 2.5))
  CCmfg <- genmf('trapmf', c(2, 2.5, 3.5, 4))
  CCmfb <- genmf('trapmf', c(3.5, 4, 4.5, 5))
  
  lne = seq(0,5,length=50)
  
  CC = data.frame(Low = evalmf(lne, CCmfp), 
                  Medium = evalmf(lne, CCmfg), 
                  High = evalmf(lne, CCmfb))
  
  CC1 = pivot_longer(CC,cols = 1:3, names_to = "Class", values_to = "MF")
  
  gcc = ggplot(CC1, aes(y = MF, x = 1:nrow(CC1), color = Class)) + 
    geom_line(size = 2) + xlab("Queuing Time (hours)") + 
    ylab(expression(Membership~Function~mu~(x))) + 
    theme_bw() + 
    # scale_x_continuous(breaks = round(lne,1)) + 
    scale_y_continuous(breaks = round(seq(min(CC1$MF), max(CC1$MF), by = 0.05),1))
  gcc
  
  CCmfp <- genmf('trapmf', c(0, 0, 0.3, 0.40))
  CCmfg <- genmf('trapmf', c(0.30, 0.40, 0.50, 0.60))
  CCmfb <- genmf('trapmf', c(0.50, 0.60, 1,1))
  
  lne = seq(0,1,length=50)
  
  CC = data.frame(Low = evalmf(lne, CCmfp), 
                  Medium = evalmf(lne, CCmfg), 
                  High = evalmf(lne, CCmfb))
  
  CC1 = pivot_longer(CC,cols = 1:3, names_to = "Class", values_to = "MF")
  
  gcc2 = ggplot(CC1, aes(y = MF, x = 1:nrow(CC1)/150, color = Class)) + 
    geom_line(size = 2) + xlab("Proportion of Vehicles at Crane") + 
    ylab(expression(Membership~Function~mu~(x))) + 
    theme_bw() + 
    # scale_x_continuous(breaks = round(lne,1)) + 
    scale_y_continuous(breaks = round(seq(min(CC1$MF), max(CC1$MF), by = 0.05),1))
  gcc2
  
  
  CCmfvl <- genmf('trimf', c(0, 0, 0.15))
  CCmfl <- genmf('trimf', c(0, 0.25, 0.4))
  CCmfm <- genmf('trimf', c(0.25, 0.50, 0.7))
  CCmfh <- genmf('trimf', c(0.5, 0.75, 0.9))
  CCmfvh <- genmf('trimf', c(0.75, 1, 1))
                 
  
  lne = seq(0,1,length=50)
  
  CC = data.frame(Very_Low = evalmf(lne, CCmfvl), 
                  Low = evalmf(lne, CCmfl),
                  Medium = evalmf(lne, CCmfm), 
                  High = evalmf(lne, CCmfh),
                  Very_high = evalmf(lne, CCmfvh))
  
  CC1 = pivot_longer(CC,cols = 1:5, names_to = "Class", values_to = "MF")
  
  jpeg(filename = "Fig_mf_output.jpg", 
       width = 12, height = 8, units = "in", res = 300)
  
  gcc3 = ggplot(CC1, aes(y = MF, x = 1:nrow(CC1)/250, color = Class)) + 
    geom_line(size = 2) + xlab("Allocation Proportion") + 
    ylab(expression(Membership~Function~mu~(x))) + 
    theme_bw() + 
    # scale_x_continuous(breaks = round(lne,1)) + 
    scale_y_continuous(breaks = round(seq(min(CC1$MF), max(CC1$MF), by = 0.05),1))
  gcc3
  
  dev.off()
  
  library(ggpubr)
  setwd(choose.dir())
  getwd()
  jpeg(filename = "Fig_mf_inputs.jpg", 
       width = 12, height = 8, units = "in", res = 300)
  
  ggarrange(gcc, gcc2, 
            # labels = c("A", "B", "C"),
            ncol = 1, nrow = 2,
            common.legend = TRUE, legend="right")
  
  dev.off()
  
}



# method

# step 1:
# run LP model and simulator - calculate the probability of queue
# choose a "cadencia" and use throughout.
# calculate the probability of queue or idle time.

# Step 2:
# with LP output, run simulator and use fuzzy to decide the arriving cranes
# Build a new simulator to adapt fuzzy decisions
# calculate the probability of queue or idle time.



# compare both probabilities (with and without fuzzy decision system)


