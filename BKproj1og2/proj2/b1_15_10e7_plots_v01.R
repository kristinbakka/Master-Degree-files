#beta is 0.5
funValues_k1=matrix(NA,runsG1+1,5)

funValues_k1[,1]=colSums(b1a0mc)/(rows1*cols1)
funValues_k1[,2]=colSums(b1a1mc)/(rows1*cols1)
funValues_k1[,3]=colSums(b1ar1mc)/(rows1*cols1)
#funValues_k1[,4]=colSums(b1ar2mc)/(rows1*cols1)
funValues_k1[,5]=colSums(b1ac1mc)/(rows1*cols1)

#Plot before
truncate1=100
tx1=1:(truncate1)
truncfun3_k1=funValues_k1[tx1,]

plot(x =tx1,y=truncfun3_k1[,1],type='l',col='red',xlim=c(0,truncate1),ylim=c(0,1),xlab='State number',ylab='Proportion of ones')
lines(x=tx1,y=truncfun3_k1[,2],type='l',col='darkorange')
lines(x=tx1,y=truncfun3_k1[,3],type='l',col='green')
#lines(x=tx1,y=truncfun3_k1[,4],type='l',col='aquamarine4')
lines(x=tx1,y=truncfun3_k1[,5],type='l',col='blue')

#compute k2:
funValues_k2=matrix(NA,runsG1+1,5)
k2_ind <- (1:(cols1*rows1-1))[!(1:(cols1*rows1-1)) %in% (1:(cols1-1))*rows1]

k2_temp=b1a0mc[2:(rows1*cols1),]-b1a0mc[1:(rows1*cols1-1),]
funValues_k2[,1]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b1a1mc[2:(rows1*cols1),]-b1a1mc[1:(rows1*cols1-1),]
funValues_k2[,2]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b1ar1mc[2:(rows1*cols1),]-b1ar1mc[1:(rows1*cols1-1),]
funValues_k2[,3]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b1ac1mc[2:(rows1*cols1),]-b1ac1mc[1:(rows1*cols1-1),]
funValues_k2[,5]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)

#Plot before
truncate1=200
tx1=1:(truncate1)
truncfun3_k1=funValues_k2[tx1,]

plot(x =tx1,y=truncfun3_k1[,1],type='l',col='red',xlim=c(0,truncate1),ylim=c(.5,.7),xlab='State number',ylab='Proportion of ones')
lines(x=tx1,y=truncfun3_k1[,2],type='l',col='darkorange')
lines(x=tx1,y=truncfun3_k1[,3],type='l',col='green')
#lines(x=tx1,y=truncfun3_k1[,4],type='l',col='aquamarine4')
lines(x=tx1,y=truncfun3_k1[,5],type='l',col='blue')

