
isingModel <- function(rows=10,cols=6,runsG=500,beta=0.5,
                       u=matrix(0, nrow = rows, ncol = cols),colsum=FALSE){
  #Generates a matrix where each column is a state of the Markov Chain
  #If colsum=true only retrun number of ones
  #Decalare Markov Chain vector
  if(!colsum){
    mc=matrix(NA,nrow=rows*cols,ncol = runsG+1) #each column is an observation
    mc[,1]=as.vector(u) #plug in initial state
  }else{
    numberOfones=matrix(NA,nrow=1,ncol = runsG+1)
    numberOfones[1]=sum(u)
    #k2= antall elementer av u[1:(runsG-1),] == u[2:(runsG),], delt på (rows-1)*cols
    k2=matrix(NA,nrow=1,ncol = runsG+1)
    k2[1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)/((rows-1)*cols)
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
      numberOfones[r+1]=sum(u)
      k2[r+1]= sum((u[1:(rows-1),]-u[2:(rows),])==0)/((rows-1)*cols)
    }
  }
  if(!colsum){
    return(mc)
  }else{
    return(list("k1" = numberOfones, "k2" = k2))
  }
}


#input variables
rows1=50 #rows
cols1=50 #columns
runsG1=round(1000000000/(rows1*cols1))
beta1=0.87


#Plot functions of state with time
#function values
funValues=matrix(NA,runsG1+1,4) #One column for each set of function values i.e. funValues[,1] are for a0
#for test case sum(mc[,1])

#Different initial values
#all zeroes (1)
a0=matrix(0, nrow = rows1, ncol = cols1)
funValues[,1]=isingModel(rows=rows1,cols=cols1,runsG=runsG1,beta=beta1,
                  u=a0, colsum = TRUE)$k1

#all ones (2)
a1=matrix(1, nrow = rows1, ncol = cols1)
funValues[,2]=isingModel(rows=rows1,cols=cols1,runsG=runsG1,beta=beta1,
                  u=a1, colsum = TRUE)$k1

#random (3)
set.seed(42)
aR=matrix(rbinom(n=rows1*cols1, size=1,prob = 0.5), nrow = rows1, ncol = cols1)
funValues[,3]=isingModel(rows=rows1,cols=cols1,runsG=runsG1,beta=beta1,
                  u=aR, colsum = TRUE)$k1
#checkerboard (4)
if(rows1%%2==1){
  #odd number of rows
  aC=matrix(rep_len(c(1,0),length.out=rows1*cols1),nrow=rows1,ncol=cols1)
}else{
  ##even number of rows
  aC=matrix(rep_len(c(1,0),length.out=(rows1+1)*cols1),nrow=rows1+1,ncol=cols1)[1:rows1,1:cols1]
}
funValues[,4]=isingModel(rows=rows1,cols=cols1,runsG=runsG1,beta=beta1,
                  u=aC, colsum = TRUE)$k1
#Make it into a proportion
funValues=funValues/(rows1*cols1)

#Plot
plot(x=1:(runsG1+1),y=funValues[,1],type='l',col='red',xlim=c(0,runsG1+1),ylim=c(0,1),xlab='State number',ylab='Proportion of ones')
lines(x=1:(runsG1+1),y=funValues[,2],type='l',col='darkorange')
lines(x=1:(runsG1+1),y=funValues[,3],type='l',col='green')
lines(x=1:(runsG1+1),y=funValues[,4],type='l',col='blue')

#Plot before
truncate1=round(2500000/(rows1*cols1))
tx1=1:(truncate1)
truncfun3=funValues[tx1,]

plot(x =tx1,y=truncfun3[,1],type='l',col='red',xlim=c(0,truncate1),ylim=c(0,1),xlab='State number',ylab='Proportion of ones')
lines(x=tx1,y=truncfun3[,2],type='l',col='darkorange')
lines(x=tx1,y=truncfun3[,3],type='l',col='green')
lines(x=tx1,y=truncfun3[,4],type='l',col='blue')



#Plot after
truncate2=truncate1
tx2=(truncate2):(runsG1+1)
truncfun=funValues[tx2,]

plot(x =tx2,y=truncfun[,1],type='l',col='red',xlim=c(truncate2,runsG1+1),ylim=c(0,1),xlab='State number',ylab='Proportion of ones')
lines(x=tx2,y=truncfun[,2],type='l',col='darkorange')
lines(x=tx2,y=truncfun[,3],type='l',col='green')
lines(x=tx2,y=truncfun[,4],type='l',col='blue')


#Do I need to use a fun server or something for the function to run?
#How to make the axis a bit more readable. Is it needed though?
#Assuming it has converged (it hasn't though) this is F(x) (in the different cases):
xvals=sort(as.vector(funValues[tx2,]))
Fvals=(1:length(xvals))/length(xvals)
plot(x=xvals,y=Fvals,type='l',xlim=c(0,1),ylim=c(0,1),xlab='x',ylab='F(x)')

#How to find f(x) from f(x)? I think I saw it somewhere in a function.

#test number of elements equal to zero
