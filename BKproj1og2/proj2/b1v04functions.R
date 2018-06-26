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
                       u=matrix(0, nrow = rows, ncol = cols)){
  #Generates a matrix where each column is a state of the Markov Chain
  #Decalare Markov Chain vector
  mc=matrix(NA,nrow=rows*cols,ncol = runs+1) #each column is an observation
  mc[,1]=as.vector(u) #plug in initial state
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
    mc[,r+1]=as.vector(u)
  }
  return(mc)
}




#input variables
rows=10 #rows
cols=10 #columns
runs=100
beta=0.87
#Different initial values
#all zeroes
a0=matrix(0, nrow = rows, ncol = cols)
#all ones
a1=matrix(1, nrow = rows, ncol = cols)
#random
aR=matrix(rbinom(n=rows*cols, size=1,prob = 0.5), nrow = rows, ncol = cols)
#checkerboard
#odd number of rows
#aC=matrix(rep_len(c(1,0),length.out=rows*cols),nrow=rows,ncol=cols)
#even number of rows
aC=matrix(rep_len(c(1,0),length.out=(rows+1)*cols),nrow=rows+1,ncol=cols)[1:rows,1:cols]



