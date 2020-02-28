##Code for HW3 ECS 132
# Viviana Rosas Romero 913305326
# Bryan Chiu 996230418 

library(lattice)

#PROBLEM A
sim1 <- function(nreps, p = 0.2 , k = 3){
  
  res <- rgeom(nreps,p)
  res = res[which(res <= k)]
  res = res[which(res != 0)]
  return(mean(res))
  
}

#PROBLEM C
sim2 <- function(){
  
  # read the table
  data_table <- read.table("~/Documents/School/cs132/hw3/dnc-corecipient/out.dnc-corecipient.txt",col.names = c("recip1ID", "recip2ID", "x"), header=F, skip=1)
  
  # filter the table
  new_table <- subset(data_table, recip1ID < recip2ID)
  #View(new_table)
  
  freq <- data.frame(table(new_table$recip1ID))
  #View(freq)
  
  mi <- data.frame(table(freq$Freq))
  #View(mi)
  xyplot((log2(mi$Freq) ~ log2(as.numeric(mi$Var1))), mi, grid=TRUE, type = c("p", "r"))
  #z <- lm(as.numeric(mi$Var1) ~ mi$Freq)
  #z
 
}
sim2()

#PROBLEM D
daccum <- function(i, k){
  
  if(i == 1){
    total <- 0
    
    #Base Cases
    if(k > 12){
      return(0);
    }
    if(k < 1){
      return(0);
    }
    else{
      count = 0
      
      for(roll1 in 1:6){
        for(roll2 in 1:6){
          
          if(roll1 + roll2 >= k)
            count = count + 1
        }
      }
      #probability of get roll1 val and roll2 val from two dice rolls
      prob = 1/36
      
      total <- total + (prob * count)
      return(total)
    }
    
    
  }
  else{
    
    total = 0
    for(roll1 in 1:6){
      for(roll2 in 1:6){
      p = 1/36
      res_daccum = daccum(i - 1, k - (roll1 + roll2))
      total = total + (p * res_daccum)
      
      }
      
    }
    return(total)
  }
}

paccum <- function(i, k){
  
  #find values less than or equal to i rolls
  accum <- 0
  for (f in ceiling(k/12):i )
    accum <- accum + daccum(f,k)
    
  return (accum)
  
}

qaccum <- function(m,k){
  
  for(c in ceiling(k/12):ceiling(k/2)){
    
    #find some c where its prob >= to m
    if(paccum(c,k) >= m){
      return(c)
    }
    
  }
  # return null if not found (not sure if this ever happens)
  return(NULL)
  
}

raccum <- function(nreps, k){
  
  # apply daccum over our vector
  prob = sapply(ceiling(k/12):ceiling(k/2), daccum, k)
  
  return(sample(ceiling(k/12):ceiling(k/2), nreps, TRUE, prob))
  
}


