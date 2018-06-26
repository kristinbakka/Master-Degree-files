case <-function(integer,rows,cols){ 
  if(i==1){#(1,1) first element first column
    return (1)
  }
  if(i==rows){#(rows,1) last element first column
    return (2)
  }
  if(i==(rows-1)*cols){#(1,cols) first element last column
    return (3)
  }
  if(i==(rows*n)){#(rows,cols) last element last column
    return (4)
  }
  if(i<rows+1){#(:,1) first column
    return (5)
  }
  if(i>(cols-1)*rows){#(:,cols) last column
    return (6)
  }
  if((i-1)%%rows==0){#(1,:) first row
    return (7)
  }
  if(i%%rows==0){#(rows,:) last row
    return (8)
  }
  return (9) #interior node
}

#todo
#1  Test the function case
#keep = matrix(0,m,n)
#for (i in 1:(m*n)){
#  keep[i]=case(integer = i, rows=m,cols=n)  
#}

#input variables
m=6 #rows
n=5 #columns
runs=100
beta=0.87
#isingModelf <- function(m=10,n=6,runs=500,beta=0.5)
#Decalare Markov Chain vector
mc=matrix(NA,nrow=m*n,ncol = runs+1) #each column is an observation
#Make a lattice of initial values
u=matrix(0, nrow = m, ncol = n)
#Insert initial values
mc[,1]=as.vector(u)

#TRY TO SOLVE IN SIMPLEST POSSIBLE WAY
r=0
i=0
#random = runif(runs)
a100=0
a090=0
while(r < runs){
  r=r+1
  i= ifelse(i==n*m, 1, i+1)
  val=u[i]
  two=c(1,1)*val
  three=c(1,1,1)*val
  four=c(1,1,1,1)*val
  
  #compute alpha
  mycase=case(integer=i,rows=m,cols=n)
  temp=switch(mycase,
    -2+2*abs(sum(u[c(i+1,i+m)]-two)), #(1,1)
    -2+2*abs(sum(u[c(i-1,i+m)]-two)), #(m,1)
    -2+2*abs(sum(u[c(i+1,i-m)]-two)), #(1,n)
    -2+2*abs(sum(u[c(i-1,i-m)]-two)), #(m,n)
    -3+2*abs(sum(u[c(i-1, i+1, i+m)]-three)), #(:,1)
    -3+2*abs(sum(u[c(i-1, i+1, i-m)]-three)), #(:,n)
    -3+2*abs(sum(u[c(i+1, i-m, i+m)]-three)), #(1,:)
    -3+2*abs(sum(u[c(i-1, i-m, i+m)]-three)), #(m,:)
    -4+2*abs(sum(u[c(i-1, i+1, i-m, i+m)]-three))) #interior
  accept = exp(beta*temp)
  
  random = runif(1)
  u[i]=ifelse(random<=accept,1-val,val)
  mc[,r+1]=as.vector(u)
}


