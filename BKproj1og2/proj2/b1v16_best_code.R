isingModel <- function(rows=10,cols=6,runsG=500,beta=0.5,
                       u=matrix(0, nrow = rows, ncol = cols),colsum=FALSE){
  #Generates a matrix where each column is a state of the Markov Chain
  #If colsum=true only retrun number of ones
  #Decalare Markov Chain vector
  if(!colsum){
    mc=matrix(NA,nrow=rows*cols,ncol = runsG+1) #each column is an observation
    mc[,1]=as.vector(u) #plug in initial state
  }else{
    k1_temp=matrix(NA,nrow=1,ncol = runsG+1)
    k1_temp[1]=sum(u)
    #k2= antall elementer av u[1:(runsG-1),] == u[2:(runsG),], delt paa (rows-1)*cols
    k2_temp=matrix(NA,nrow=1,ncol = runsG+1)
    k2_temp[1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)
    #k2[1]= ifelse((sum((u[1:(runsG-1),]-u[2:(runsG),])==0)/((rows-1)*cols))>(3/4),1,0)
  }
  case= matrix(9,nrow = rows ,ncol = cols)
  #columns
  case[,1]=5; case[,cols]=6
  #rows
  case[1,]=7; case[rows,]=8
  #corners
  case[1,1]=1; case[rows,1]=2; 
  case[1,cols]=3; case[rows,cols]=4
  
  for(r in 1:runsG){
    for(i in 1:(cols*rows)){ #fill grid 1 time
      # (1) PROPOSE CANDIDATE STATE
      #next node to consider
      val=u[i]
      
      # (2) COMPUTE ACCEPTANCE PROBABILITY
      #which type of node is this?
      #compute 2d -n
      temp=switch(case[i],
                  -2+2*abs(sum(u[c(i+1,i+rows)]-val)), #(1,1)
                  -2+2*abs(sum(u[c(i-1,i+rows)]-val)), #(rows,1)
                  -2+2*abs(sum(u[c(i+1,i-rows)]-val)), #(1,cols)
                  -2+2*abs(sum(u[c(i-1,i-rows)]-val)), #(rows,cols)
                  -3+2*abs(sum(u[c(i-1, i+1, i+rows)]-val)), #(:,1)
                  -3+2*abs(sum(u[c(i-1, i+1, i-rows)]-val)), #(:,cols)
                  -3+2*abs(sum(u[c(i+1, i-rows, i+rows)]-val)), #(1,:)
                  -3+2*abs(sum(u[c(i-1, i-rows, i+rows)]-val)), #(rows,:)
                  -4+2*abs(sum(u[c(i-1, i+1, i-rows, i+rows)]-val))) #interior
      #acceptance probability
      #accept = exp(beta*temp)
      accept = beta*temp
      
      # (3) ACCEPT OR REJECT CANDIDATE STATE
      random = log(runif(1))
      u[i]=ifelse(random<=accept,1-val,val)  
      i=i+1
    }
    
    if(!colsum){
      mc[,r+1]=as.vector(u)
    }else{
      k1_temp[r+1]=sum(u)
      k2_temp[r+1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)
    }
  }
  if(!colsum){
    return(mc)
  }else{
    return(list("k1" = k1_temp/(rows*cols), "k2" = k2_temp/((rows-1)*cols)))
  }
}