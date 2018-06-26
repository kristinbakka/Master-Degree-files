#CDF-plot k2
myfunc=b1_k1_02
myfunc=b2_k1_02
myfunc=b1_k2_02
myfunc=b2_k2_02

xvals_k2=sort(as.vector(myfunc))
Fvals=(1:length(xvals_k2))/length(xvals_k2)
plot(x=xvals_k2,y=Fvals,type='l',xlim=c(min(xvals_k2),max(xvals_k2)),ylim=c(0,1),xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext("F(x)", side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k2_mean=mean(xvals_k2)
abline(v = k2_mean, col = "red",lwd=2)

#PDF-plot k2:
hist(xvals_k2, freq = FALSE,breaks = 47,main=' ',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=1.5)# Plot density og f_1 in sa
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext("f(x)", side=2, line=2, cex=2)
mtext("x", side=1, line=3, cex=2)
# Add vertical line for estimated mean
k2_mean=mean(xvals_k2)
abline(v = k2_mean, col = "red",lwd=2)

