#truncfun2_k2=b1_more_params_03$k2
truncfun2_k2=b1_more_params_03$k2

#CDF
xvals_k2=sort(as.vector(truncfun2_k2))
Fvals=(1:length(xvals_k2))/length(xvals_k2)
plot(x=xvals_k2,y=Fvals,type='l',xlim=c(min(xvals_k2),max(xvals_k2)),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
axis(2,cex.axis=1.5,lwd=1.5)
axis(1,cex.axis=1.5,lwd=1.5)
mtext('F(x)', side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k2_mean=mean(xvals_k2)
abline(v = k2_mean, col = "red",lwd=2)

#PDF-plot k2:
hist(xvals_k2, freq = FALSE,breaks = 49,main=' ',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)# Plot density og f_1 in sa
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
k2_iterations=length(xvals_k2)

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
