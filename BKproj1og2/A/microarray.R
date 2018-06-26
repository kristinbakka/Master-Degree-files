microarrayanalysis = function(nu_mean, nu_variance, delta_mean, delta_variance, 
                              tau_shape, tau_scale, genedatax, genedatay, N){
  # Gibbs sample function to make ONE markov chain for each parameter of the microarray-data.
  # nu_mean, nu_variance, delta_mean, delta_variance, tau_shape, tau_scale start values for
  # the parameters of the distribution of mu (normal), delta(normal) and tau(gamma)
  # genedatax and genedatay are data for the gene for the two different groups x and y
  # N number of samples for each parameter wanted
  
  s_1 = length(genedatax) # s_1 number of data from gene for group x
  s_2 = length(genedatay) #s_2 number of data from gene for grup y
  
  nu_samples = vector(mode = "numeric", length = N) # samples of nu
  delta_samples = vector(mode = "numeric", length = N) # samples of delta
  tau_samples = vector(mode = "numeric", length = N) # samples of tau
  
  # Generate random start values for nu, delta and tau 
  nu_samples[1] = rnorm(1,nu_mean,sqrt(nu_variance))
  delta_samples[1] = rnorm(1,delta_mean,sqrt(delta_mean))
  tau_samples[1] = rgamma(1, shape = tau_shape, scale = tau_scale)
  # Generate Gibbs samples:
  for(i in 2:N){
    
    temp1=(tau_samples[i-1]*delta_samples[i-1]*(s_2-s_1) + 
      tau_samples[i-1]*(sum(genedatax)+sum(genedatay)) + 
      nu_mean/nu_variance)/ (tau_samples[i-1]*s_1 + tau_samples[i-1]*s_2 + 1/nu_variance )
    temp2=1/(tau_samples[i-1]*s_1 + tau_samples[i-1]*s_2 + 1/nu_variance )

    # sample for nu:
    nu_samples[i] = rnorm(1, temp1, temp2)
    
    # sample for delta
    delta_samples[i] = rnorm(1,(tau_samples[i-1]*nu_samples[i]*(s_2-s_1) + tau_samples[i-1]*(sum(genedatax)-sum(genedatay)) + delta_mean/delta_variance)/  
                               (tau_samples[i-1]*s_1 + tau_samples[i-1]*s_2 + 1/delta_variance ),  1/(tau_samples[i-1]*s_1 + tau_samples[i-1]*s_2 + 1/delta_variance ))
    #sample for tau 
    tau_samples[i] = rgamma(1, shape = (tau_shape + (s_1+s_2)/2), scale = 1/(0.5*(sum((genedatax - nu_samples[i] - delta_samples[i])^2) + 
                                                                                    sum((genedatay - nu_samples[i] + delta_samples[i])^2)) + 1/tau_scale))
    
    
  }
  # Create list containing sample from each parameter
  l = list(nu_samples,delta_samples,tau_samples)
  return(l)
}