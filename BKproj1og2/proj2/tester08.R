
multiplier=100:110

for (i in 1:10){
  multiplier[i]=i
}

View(multiplier)

multiplier=1000

library(stats)
#l=length(b1_more_params_03$k1)

#b1_k1_01=truncfun2_k1[1:(-1+length(truncfun2_k1))]
#l=length(b1_k1_01)
#b1_k1_01=b1_k1_01[6+(1:((l-13)/10))*10]
#not the markov chain, but the values k_a(u_i) for beta1
#b1_k1_02=c(b1_k1_01[1:1509],b1_more_params_03$k1[1:120001])
s=length(b1_k1_02)
b1_k1_mean= mean(b1_k1_02)
b1_k1_var=var(b1_k1_02)*s/(s-1)
b1_k1_lower = b1_k1_mean-1.96*sqrt(b1_k1_var)
b1_k1_upper = b1_k1_mean+1.96*sqrt(b1_k1_var)
  
b1_k2_01=truncfun2_k2[1:(-1+length(truncfun2_k2))]
b1_k2_01=b1_k2_01[(1:((length(b1_k2_01))/10))*10]

#not the markov chain, but the values k_a(u_i) for beta1
b1_k2_02=c(b1_k2_01[1:1509],b1_more_params_03$k2[1:120001])
s=length(b1_k2_02)
b1_k2_mean= mean(b1_k2_02)
b1_k2_var=var(b1_k2_02)*s/(s-1)
b1_k2_lower = b1_k2_mean-1.96*sqrt(b1_k2_var)
b1_k2_upper = b1_k2_mean+1.96*sqrt(b1_k2_var)


#and these are the same:
#b2_k1_01=truncfun2_k1[1:(-1+length(truncfun2_k1))]
#l=length(b2_k1_01)
#b2_k1_01=b2_k1_01[7+(1:((l-7)/10))*10]

#b2_k2_01=truncfun2_k2[1:(-1+length(truncfun2_k2))]
#l=length(b2_k2_01)
#b2_k2_01=b2_k2_01[7+(1:((l-7)/10))*10]


#1---------------
#b1_k1
temp=c(b1_k1_01,b1_more_params_03$k1[1:120000])
# ca 1
acftester1=acf(x=,lag.max=3,main='',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2.5)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)

#ca 3 (eller 2)
acftester1_02=acf(b1_k1_01,lag.max=5,main='',xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2.5,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)


#2---------------
#b2_k1

#ca 80 eller 100
acftester2=acf(b2_more_params_04$k1[1:250001],lag.max=100,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2.5,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)

#ca 320
acftester2_02=acf(b2_k1_01,lag.max=320,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)


#3---------------
#b1_k2

#ca 1
acftester3=acf(b1_more_params_03$k2[1:120000],lag.max=4,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)

#ca 10
acftester3_02=acf(b1_k2_01,lag.max=10,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)


#4---------------
#b2_k2

#ca 20 er mer enn nok
acftester4=acf(b2_more_params_04$k2[1:250000],lag.max=20,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)

#ca 50
acftester4_02=acf(b2_k2_01,lag.max=50,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)

############################################################################

