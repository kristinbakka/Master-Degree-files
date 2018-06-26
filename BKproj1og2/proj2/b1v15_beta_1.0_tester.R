library(tictoc)
tic()
isingModel <- function(rows=50,cols=50,runsG=500,beta=0.5,
                       u=matrix(0, nrow = rows, ncol = cols)){
  ##Generates a matrix where each column is a state of the Markov Chain
  mc=matrix(NA,nrow=rows*cols,ncol = runsG+1) #each column is an observation
  mc[,1]=as.vector(u) #plug in initial state
  ##Generate case vector (1 time cost)
  case= matrix(9,nrow = rows ,ncol = cols)
  #columns
  case[,1]=5; case[,cols]=6
  #rows
  case[1,]=7; case[rows,]=8
  #corners
  case[1,1]=1; case[rows,1]=2 
  case[1,cols]=3; case[rows,cols]=4
  
  for(r in 1:runsG){
    for(i in 1:(cols*rows)){ #fill grid 1 time
      # (1) PROPOSE CANDIDATE STATE
      #next node to consider
      val=u[i]
      
      # (2) COMPUTE ACCEPTANCE PROBABILITY
      #which type of node is this?
      #compute temp = 2d -n
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
      
      # (3) ACCEPT OR REJECT CANDIDATE STATE
      random = log(runif(1))
      u[i]=ifelse(random<=beta*temp,1-val,val)  
      i=i+1
    }
    #save after 1 run through grid
    mc[,r+1]=as.vector(u)
  }
  return(mc)
}


#input variables
rows2=5 #rows
cols2=5 #columns
runsG2=round(1000000000/(rows1*cols2))


#Plot functions of state with time
#function values
funValues=matrix(NA,runsG2+1,4) #One column for each set of function values i.e. funValues[,1] are for a0
#for test case sum(mc[,1])

#Different initial values
#all zeroes (1)
a0=matrix(0, nrow = rows1, ncol = cols2)
#all ones (2)
a1=matrix(1, nrow = rows1, ncol = cols2)
#random (3)
set.seed(42)
aR1=matrix(rbinom(n=rows1*cols2, size=1,prob = 0.5), nrow = rows1, ncol = cols2)
set.seed(198)
aR2=matrix(rbinom(n=rows1*cols2, size=1,prob = 0.5), nrow = rows1, ncol = cols2)

#checkerboard (4)
if(rows1%%2==1){
  #odd number of rows
  aC=matrix(rep_len(c(1,0),length.out=rows1*cols2),nrow=rows1,ncol=cols2)
}else{
  ##even number of rows
  aC=matrix(rep_len(c(1,0),length.out=(rows1+1)*cols2),nrow=rows1+1,ncol=cols2)[1:rows1,1:cols2]
}

beta3=1
testera0mc=isingModel(rows=rows2,cols=cols2,runsG=runsG2,beta=beta3,u=a0)
testera1mc=isingModel(rows=rows2,cols=cols2,runsG=runsG2,beta=beta3,u=a1)
testerar1mc=isingModel(rows=rows2,cols=cols2,runsG=runsG2,beta=beta3,u=aR1)
testerar2mc=isingModel(rows=rows2,cols=cols2,runsG=runsG2,beta=beta3,u=aR2)
testerac1mc=isingModel(rows=rows2,cols=cols2,runsG=runsG2,beta=beta3,u=aC)
#---------------------------
toc()