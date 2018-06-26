#input variables
m=6 #rows
n=5 #columns
runs=100
#Decalare Markov Chain vector
mc=matrix(NA,nrow=m*n,ncol = runs+1) #each column is an observation
#Make a lattice of initial values
u=matrix(0, nrow = m, ncol = n)
#Insert initial values
mc[,1]=as.vector(u)

tester = u
tester[10]=11
#View(tester)

#Propose candidate v
node = rep(1:(m*n),ceiling(runs/(m*n)))[1:runs]
random = runif(m*n)

#Compute acceptance probability
#is node a corner node?
#assume no:
#index of neighbors

neighbors=switch(i, 
       foo={
         # case 'foo' here...
         print('foo')
       },
       bar={
         # case 'bar' here...
         print('bar')    
       },
       {
         print('default')
       }
)

i=

  
  
  n = c(i-1, i+1, i-m,i+m) #over, under, left right neighbor


#is element first column?
b = 0

#first element: 
f1 = 1
nf1=n = c(i+1,i+m) #over, under, left right neighbor

#last element first column:
b1= m
nb1= c(i-1, i+m)
  
#first element last column
fn=(m-1)*n +1
nfn=
#last element last column
bn=m*n
nbn=
#first column


#Accept

#Idea: first suggest all fringe nodes, then suggest all interior nodes?


#How to represent state vector?
#Solution:
#Compute stuff with state as matrix
#Then mc[run]=as.vector(u)

#example
i=8
nn = c(i-1, i+1, i-m,i+m) #over, under, left right neighbor
mm=matrix(1:(m*n),m,n)
tester=as.vector(m)
#View(tester)
View(mm)
tester=mm[nn]
View(tester)
