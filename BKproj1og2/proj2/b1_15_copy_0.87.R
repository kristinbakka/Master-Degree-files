#beta is 0.87
funValues_k1=matrix(NA,runsG1+1,4)

funValues_k1[,1]=colSums(b2a0mc)/(rows1*cols1)
funValues_k1[,2]=colSums(b2a1mc)/(rows1*cols1)
funValues_k1[,3]=colSums(b2ar1mc)/(rows1*cols1)
#funValues_k1[,4]=colSums(b2ar2mc)/(rows1*cols1)
funValues_k1[,4]=colSums(b2ac1mc)/(rows1*cols1)

#Plot before
truncate1_k1=100
tx1_k1=1:(truncate1_k1)
truncfun3_k1=funValues_k1[tx1_k1,]

plot(x =tx1_k1,y=truncfun3_k1[,1],type='l',col='red',xlim=c(0,truncate1_k1),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
lines(x=tx1_k1,y=truncfun3_k1[,2],type='l',col='darkorange',lwd=1.5)
lines(x=tx1_k1,y=truncfun3_k1[,3],type='l',col='green',lwd=1.5)
#lines(x=tx1_k1,y=truncfun3_k1[,4],type='l',col='aquamarine4',lwd=1.5)
lines(x=tx1_k1,y=truncfun3_k1[,4],type='l',col='blue',lwd=1.5)

axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext(expression(k[a]), side=2, line=2, cex=2)
mtext("State number", side=1, line=3, cex=2)
lines(x=tx1_k1,y=rep(0.5,length(tx1_k1)),type='l',lwd=1.5)



#Plot after
truncate2_k1=2000
tx2_k1=(truncate2_k1):(runsG1+1)
truncfun2_k1=funValues_k1[tx2_k1,]

plot(x =tx2_k1,y=truncfun2_k1[,1],type='l',col='red',xlim=c(truncate2_k1,runsG1+1),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
lines(x=tx2_k1,y=truncfun2_k1[,2],type='l',col='darkorange',lwd=1.5)
lines(x=tx2_k1,y=truncfun2_k1[,3],type='l',col='green',lwd=1.5)
lines(x=tx2_k1,y=truncfun2_k1[,4],type='l',col='blue',lwd=1.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext(expression(k[a]), side=2, line=2, cex=2)
mtext("State number", side=1, line=3, cex=2)

#this was k1
#-----------------------------------------
#here comes k2

#compute k2:
funValues_k2=matrix(NA,runsG1+1,4)
k2_ind <- (1:(cols1*rows1-1))[!(1:(cols1*rows1-1)) %in% (1:(cols1-1))*rows1]

k2_temp=b2a0mc[2:(rows1*cols1),]-b2a0mc[1:(rows1*cols1-1),]
funValues_k2[,1]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b2a1mc[2:(rows1*cols1),]-b2a1mc[1:(rows1*cols1-1),]
funValues_k2[,2]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b2ar1mc[2:(rows1*cols1),]-b2ar1mc[1:(rows1*cols1-1),]
funValues_k2[,3]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)
k2_temp=b2ac1mc[2:(rows1*cols1),]-b2ac1mc[1:(rows1*cols1-1),]
funValues_k2[,4]=colSums(k2_temp[k2_ind,]==0)/((rows1-1)*cols1)

#Plot before
truncate1_k2=4000
tx1_k2=1:(truncate1_k2)
truncfun3_k2=funValues_k2[tx1_k2,]

plot(x =tx1_k2,y=truncfun3_k2[,1],type='l',col='red',xlim=c(0,truncate1_k2),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
lines(x=tx1_k2,y=truncfun3_k2[,2],type='l',col='darkorange',lwd=1.5)
lines(x=tx1_k2,y=truncfun3_k2[,3],type='l',col='green',lwd=1.5)
#lines(x=tx1_k2,y=truncfun3_k1[,4],type='l',col='aquamarine4',lwd=1.5)
lines(x=tx1_k2,y=truncfun3_k2[,4],type='l',col='blue',lwd=1.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext(expression(k[b]), side=2, line=2, cex=2)
mtext("State number", side=1, line=3, cex=2)

#Plot after
truncate2_k2=2000
tx2_k2=(truncate2_k2):(runsG1+1)
truncfun2_k2=funValues_k2[tx2_k2,]

plot(x =tx2_k2,y=truncfun2_k2[,1],type='l',col='red',xlim=c(truncate2_k2,runsG1+1),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
lines(x=tx2_k2,y=truncfun2_k2[,2],type='l',col='darkorange',lwd=1.5)
lines(x=tx2_k2,y=truncfun2_k2[,3],type='l',col='green',lwd=1.5)
lines(x=tx2_k2,y=truncfun2_k2[,4],type='l',col='blue',lwd=1.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext(expression(k[b]), side=2, line=2, cex=2)
mtext("State number", side=1, line=3, cex=2)

#CDF k2
xvals_k2=sort(c(as.vector(truncfun2_k2),b2_more_params_03$k2))
Fvals=(1:length(xvals_k2))/length(xvals_k2)
plot(x=xvals_k2,y=Fvals,type='l',xlim=c(min(xvals_k2),max(xvals_k2)),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext("F(x)", side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k2_mean=mean(xvals_k2)
abline(v = k2_mean, col = "red",lwd=2)


#PDF-plot try01 k2:
hist(xvals_k2, freq = FALSE)# Plot density og f_1 in sa
#PDF-plot k2:
hist(xvals_k2, freq = FALSE,breaks = 50,main=' ',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5,xlim = c(0.76,0.88))# Plot density og f_1 in sa
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext("f(x)", side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k2_mean=mean(xvals_k2)
k2_var=var(xvals_k2)
k2_sd=sqrt(k2_var)
k2_upper=1.96*sqrt(k2_var)+k2_mean
k2_lower=-1.96*sqrt(k2_var)+k2_mean
abline(v = k2_mean, col = "red",lwd=2)


#CDF-plot k1
xvals_k1=sort(c(as.vector(truncfun2_k1),b2_more_params_03$k1))
#xvals_k1=sort(c(as.vector(truncfun2_k1),b2_more_params$k1,b2_more_params_02$k1))

Fvals=(1:length(xvals_k1))/length(xvals_k1)
plot(x=xvals_k1,y=Fvals,type='l',xlim=c(min(xvals_k1),max(xvals_k1)),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
axis(2,cex.axis=1.5,lwd=1.5)
axis(1,cex.axis=1.5,lwd=1.5)
mtext('F(x)', side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)

# Add vertical line for estimated mean
k1_mean=mean(xvals_k1)
abline(v = k1_mean, col = "red",lwd=2)


#PDF-plot k1:
hist(xvals_k1, freq = FALSE,breaks = 50,main=' ',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)# Plot density og f_1 in sa
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext("f(x)", side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k1_mean=mean(xvals_k1)
k1_var=var(xvals_k1)
k1_sd=sqrt(k1_var)
k1_upper=1.96*sqrt(k1_var)+k1_mean
k1_lower=-1.96*sqrt(k1_var)+k1_mean
abline(v = k1_mean, col = "red",lwd=2)


k1_iterations=length(xvals_k1)
k2_iterations=length(xvals_k2)



#the plots of the cdf's:
#plot(x=xvals_k2,y=Fvals,type='l',xlim=c(min(xvals_k2),max(xvals_k2)),ylim=c(0,1),xlab='x',ylab='F(x) for k_2')

