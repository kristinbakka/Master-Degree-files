
multiplier=100:110

for (i in 1:10){
  multiplier[i]=i
}

View(multiplier)

multiplier=1000

library(stats)
#l=length(b1_more_params_03$k1)

#----- these are the same:
#b1_k1_01=truncfun2_k1[1:(-1+length(truncfun2_k1))]
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


#1---------------
#b1_k1

#2---------------
#b2_k1
#b2_k1_02=c(b2_k1_01,b2_more_params_04$k1[1:250001])

#ca 320
#acftester2_02=acf(b2_k1_02,lag.max=100,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2,main='')
#axis(2,cex.axis=1.5)
#axis(1,cex.axis=1.5)
#mtext('ACF', side=2, line=2, cex=2)
#mtext("Lag", side=1, line=3, cex=2)

#b2_k1_mu_01=colSums(matrix(b2_k1_02,nrow = 100))
b2_k1_mu_01=colSums(matrix(b2_k1_02,nrow = 264))
#b2_k1_mu_01=matrix(b2_k1_02,nrow = 76)[1,]
b2_k1_length=length(b2_k1_mu_01)

b2_k1_mean= mean(b2_k1_02)
b2_k1_var=var(b2_k1_02)*s/(s-1)
b2_k1_lower = b2_k1_mean-1.96*sqrt(b2_k1_var)
b2_k1_upper = b2_k1_mean+1.96*sqrt(b2_k1_var)

acftester2_02=acf(b2_k1_mu_01,lag.max=5,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=2,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)


#3---------------
#b1_k2

#4---------------
#b2_k2
#b2_k2_02=c(b2_k2_01,b2_more_params_04$k2[1:250001])

#ca 50
#acftester4_02=acf(b2_k2_02,lag.max=20,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
#axis(2,cex.axis=1.5)
#axis(1,cex.axis=1.5)
#mtext('ACF', side=2, line=2, cex=2)
#mtext("Lag", side=1, line=3, cex=2)

b2_k2_mu_01=colSums(matrix(b2_k2_02,nrow = 60))
#b2_k2_mu_01=matrix(b2_k2_02,nrow = 20)[1,]
b2_k2_length=length(b2_k2_mu_01)

b2_k2_mean= mean(b2_k2_02)
b2_k2_var=var(b2_k2_02)*s/(s-1)
b2_k2_lower = b2_k2_mean-1.96*sqrt(b2_k2_var)
b2_k2_upper = b2_k2_mean+1.96*sqrt(b2_k2_var)




acftester4_02=acf(b2_k2_mu_01,lag.max=20,xlab=' ',ylab=' ',yaxt="n",xaxt="n",lwd=3,main='')
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
mtext('ACF', side=2, line=2, cex=2)
mtext("Lag", side=1, line=3, cex=2)




############################################################################

