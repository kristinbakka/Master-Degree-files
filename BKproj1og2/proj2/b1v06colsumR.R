
case <-function(integer,rows,cols){ 
  if(integer==1){#(1,1) first element first column
    return (1)
  }
  if(integer==rows){#(rows,1) last element first column
    return (2)
  }
  if(integer==(rows-1)*cols){#(1,cols) first element last column
    return (3)
  }
  if(integer==(rows*cols)){#(rows,cols) last element last column
    return (4)
  }
  if(integer<rows+1){#(:,1) first column
    return (5)
  }
  if(integer>(cols-1)*rows){#(:,cols) last column
    return (6)
  }
  if((integer-1)%%rows==0){#(1,:) first row
    return (7)
  }
  if(integer%%rows==0){#(rows,:) last row
    return (8)
  }
  return (9) #interior node
}

#todo
#1  Test the function case
#keep = matrix(0,rows,cols)
#for (i in 1:(rows*cols)){
#  keep[i]=case(integer = i, rows=rows,cols=cols)  
#}


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
  }
  r=0 #run number
  i=0 #index of considered node
  while(r < runs){
    # (1) PROPOSE CANDIDATE STATE
    r=r+1
    #next node to consider
    i= ifelse(i==cols*rows, 1, i+1)
    val=u[i]
    
    # (2) COMPUTE ACCEPTANCE PROBABILITY
    #which type of node is this?
    mycase=case(integer=i,rows=rows,cols=cols)
    #compute 2d -n
    temp=switch(mycase,
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
    accept = exp(beta*temp)
    
    # (3) ACCEPT OR REJECT CANDIDATE STATE
    random = runif(1)
    u[i]=ifelse(random<=accept,1-val,val)
    
    if(!colsum){
      mc[,r+1]=as.vector(u)
    }else{
      numberOfones[r+1]=sum(u)
    }
    
  }
  if(!colsum){
    return(mc)
  }else{
    return(numberOfones)
  }
}


#input variables
rows1=100 #rows
cols1=100 #columns
runs1=100000
beta1=0.87

#Different initial values
#all zeroes (1)
a0=matrix(0, nrow = rows1, ncol = cols1)
model0=isingModel(rows=rows1,cols=cols1,runs=runs1,beta=beta1,
                  u=a0)

#all ones (2)
a1=matrix(1, nrow = rows1, ncol = cols1)
model1=isingModel(rows=rows1,cols=cols1,runs=runs1,beta=beta1,
                  u=a1)

#random (3)
set.seed(42)
aR=matrix(rbinom(n=rows1*cols1, size=1,prob = 0.5), nrow = rows1, ncol = cols1)
modelR=isingModel(rows=rows1,cols=cols1,runs=runs1,beta=beta1,
                  u=aR)
#checkerboard (4)
if(rows1%%2==1){
  #odd number of rows
  aC=matrix(rep_len(c(1,0),length.out=rows1*cols1),nrow=rows1,ncol=cols1)
}else{
  ##even number of rows
  aC=matrix(rep_len(c(1,0),length.out=(rows1+1)*cols1),nrow=rows1+1,ncol=cols1)[1:rows1,1:cols1]
}
modelC=isingModel(rows=rows1,cols=cols1,runs=runs1,beta=beta1,
                  u=aC)


#Plot functions of state with time
#function values
funValues=matrix(NA,runs1+1,4) #One column for each set of function values i.e. funValues[,1] are for a0
#for test case sum(mc[,1])
funValues[,1]=colSums (model0, na.rm = FALSE, dims = 1)
funValues[,2]=colSums (model1, na.rm = FALSE, dims = 1)
funValues[,3]=colSums (modelR, na.rm = FALSE, dims = 1)
funValues[,4]=colSums (modelC, na.rm = FALSE, dims = 1)

#Plot
plot(x=1:(runs1+1),y=funValues[,1],type='l',col='red',xlim=c(0,runs1+1),ylim=c(0,rows1*cols1),xlab='State number',ylab='Number of ones')
lines(x=1:(runs1+1),y=funValues[,2],type='l',col='darkorange')
lines(x=1:(runs1+1),y=funValues[,3],type='l',col='green')
lines(x=1:(runs1+1),y=funValues[,4],type='l',col='blue')


