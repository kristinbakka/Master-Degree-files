library(tictoc)
tic()
isingModel <- function(rows=50,cols=50,runsG=400000,beta=0.87,
                       u=matrix(0, nrow = rows, ncol = cols),colsum=TRUE){
  #Generates a matrix where each column is a state of the Markov Chain
  #If colsum=true only return function values
  #Decalare Markov Chain vector
  if(!colsum){
    mc=matrix(NA,nrow=rows*cols,ncol = runsG+1) #each column is an observation
    mc[,1]=as.vector(u) #plug in initial state
  }else{
    k1_temp=matrix(NA,nrow=1,ncol = runsG+1)
    k1_temp[1]=sum(u)
    k2_temp=matrix(NA,nrow=1,ncol = runsG+1)
    k2_temp[1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)
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
    for (k in 1:10){
      ##CORNERS
      #corner 1
      index=1
      val=u[index]
      accept=beta*(-2+2*abs(sum(u[c(index+1,index+rows)]-val)))
      
      random = log(runif(1))
      u[1]=ifelse(random<=accept,1-val,val)
      
      #corner 2
      index=rows
      val=u[index]
      accept=beta*(-2+2*abs(sum(u[c(index-1,index+rows)]-val)))
      
      random = log(runif(1))
      u[index]=ifelse(random<=accept,1-val,val)
      
      #corner 3
      index=rows*(cols-1)+1
      val=u[rows*(cols-1)+1]
      accept=beta*(-2+2*abs(sum(u[c(index+1,index-rows)]-val)))
      
      random = log(runif(1))
      u[index]=ifelse(random<=accept,1-val,val)
      
      #corner 4
      index=rows*cols
      val=u[index]
      accept=beta*(-2+2*abs(sum(u[c(index-1,index-rows)]-val)))
      
      random = log(runif(1))
      u[rows*cols]=ifelse(random<=accept,1-val,val)
      
      for (i in 1:rows){
        ##Left side
        val=u[i]
        accept=beta*(-3+2*abs(sum(u[c(i-1, i+1, i+rows)]-val)))
        
        random = log(runif(1))
        u[i]=ifelse(random<=accept,1-val,val)
        
        ##Right side
        index=i+rows*(cols-1)
        val=u[index]
        accept=beta*(-3+2*abs(sum(u[c(index-1, index+1, index-rows)]-val)))
        
        random = log(runif(1))
        u[index]=ifelse(random<=accept,1-val,val)
      }
      
      for(i in (rows+1):(cols*rows-rows)){ #fill grid 1 time
        # (1) PROPOSE CANDIDATE STATE
        #next node to consider
        val=u[i]
        
        # (2) COMPUTE ACCEPTANCE PROBABILITY
        #which type of node is this?
        #compute 2d -n
       temp=switch(case[i],
                    0, #(1,1)
                    0, #(rows,1)
                    0, #(1,cols)
                    0, #(rows,cols)
                    0, #(:,1)
                    0, #(:,cols)
                    -3+2*abs(sum(u[c(i+1, i-rows, i+rows)]-val)), #(1,:)
                    -3+2*abs(sum(u[c(i-1, i-rows, i+rows)]-val)), #(rows,:)
                    -4+2*abs(sum(u[c(i-1, i+1, i-rows, i+rows)]-val))) #interior
        #acceptance probability
        #accept = exp(beta*temp)
        accept = beta*temp
        
        # (3) ACCEPT OR REJECT CANDIDATE STATE
        random = log(runif(1))
        u[i]=ifelse(random<=accept,1-val,val)  
      }
      
      if(!colsum){
        mc[,r+1]=as.vector(u)
      }else{
        k1_temp[r+1]=sum(u)
        k2_temp[r+1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)
      }
    }
  }
  if(!colsum){
    return(mc)
  }else{
    return(list("k1" = k1_temp/(rows*cols), "k2" = k2_temp/((rows-1)*cols)))
  }
}
#last state of the Markov Chain
#b2_state_01
#b2_state_02
#b2_more_params_02=isingModel(u=b2_state_02)

#b2_state_03=matrix(b2ar1mc[,4001],nrow = 50,ncol = 50)
#b2_more_params_03=isingModel(runsG=40000,u=b2_state_03)
#and more parameters from state1
#b1_state_01=matrix(b1a0mc[,4001],nrow = 50,ncol = 50)
#tic()
#b1_more_params_01=isingModel(runsG=40000,u=b1_state_01,beta=0.5)
#toc()

#
#tick()
#b2_state_04=matrix(b2ac1mc[,4001],nrow = 50,ncol = 50)
#b2_more_params_04=isingModel(runsG=400000,u=b2_state_04)
#and more parameters from state1
#b1_state_02=matrix(b1a1mc[,4001],nrow = 50,ncol = 50)
#b1_more_params_02=isingModel(runsG=400000,u=b1_state_02,beta=0.5)
#toc()
tic()
tester=isingModel(runsG=400,u=b1_state_02,beta=0.5)
toc()
