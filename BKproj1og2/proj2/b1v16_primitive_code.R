isingModel <- function(rows=10,cols=6,runs=500,beta=0.5,
                       u=matrix(0, nrow = rows, ncol = cols),colsum=FALSE){
  #Generates a matrix where each column is a state of the Markov Chain
  #If colsum=true only retrun number of ones
  #Decalare Markov Chain vector
  if(!colsum){
    mc=matrix(NA,nrow=rows*cols,ncol = runs+1) #each column is an observation
    mc[,1]=as.vector(u) #plug in initial state
  }else{
    numberOfones=matrix(NA,nrow=1,ncol = runs+1)
    numberOfones[1]=sum(u)
    k2_temp=matrix(NA,nrow=1,ncol = runs+1)
    k2_temp[1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)
  }
  r=0 #run number
  i=0 #index of considered node
  case= matrix(9,nrow = rows ,ncol = cols)
  #columns
  case[,1]=5; case[,cols]=6
  #rows
  case[1,]=7; case[rows,]=8
  #corners
  case[1,1]=1; case[rows,1]=2; 
  case[1,cols]=3; case[rows,cols]=4
  
  while(r < runs){
    # (1) PROPOSE CANDIDATE STATE
    r=r+1
    #next node to consider
    i= ifelse(i==cols*rows, 1, i+1)
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
    
    if(!colsum){
      mc[,r+1]=as.vector(u)
    }else{
      numberOfones[r+1]=sum(u)
      k2_temp[r+1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)/((rows-1)*cols)
    }
  }
  if(!colsum){
    return(mc)
  }else{
    return(list("k1" = numberOfones/(rows*cols), "k2" = k2_temp)/((rows-1)*cols))
  }
}