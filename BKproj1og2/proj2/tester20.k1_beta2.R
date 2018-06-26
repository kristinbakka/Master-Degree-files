truncfun2_k1=b2_more_params_04$k1

#CDF
xvals_k1=sort(as.vector(truncfun2_k1))
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

#l=length(b1_k1_01)
#b1_k1_01=b1_k1_01[7+(1:((l-7)/10))*10]

#b1_k2_01=truncfun2_k2[1:(-1+length(truncfun2_k2))]
#l=length(b1_k2_01)
#b1_k2_01=b1_k2_01[8+(1:((l-7)/10))*10]

#and these are the same:
#b2_k1_01=truncfun2_k1[1:(-1+length(truncfun2_k1))]
#l=length(b2_k1_01)
#b2_k1_01=b2_k1_01[8+(1:((l-8)/10))*10]

#b2_k2_01=truncfun2_k2[1:(-1+length(truncfun2_k2))]
#l=length(b2_k2_01)
#b2_k2_01=b2_k2_01[8+(1:((l-8)/10))*10]
