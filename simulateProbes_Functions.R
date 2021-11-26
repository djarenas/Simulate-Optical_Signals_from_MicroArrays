#=============================================
#gen_from_lognormal
#=============================================
gen_from_lognormal <- function(n, des_mean, des_sd){
  #'Generate n points from a log-normal distribution with specified means and sds
  #' @param n The desired number of points generated
  #' @param m The desired mean of the log-normal distribution
  #' @param s The desired standard deviation of the log-normal distribution
  #--------------------------------------------------------------------------------
  #To avoid singularities, change zero values of m and s to a small non-zero value.
  #--------------------------------------------------------------------------------
  if (des_mean < 0.00001){
    des_mean = 0.00001
  }
  if (des_sd < 0.00001){
    des_sd = 0.00001
  }
  #--------------------------------------------------------------------------------
  #Re-parametrize so that you obtain the desired mean (m) and standard deviation (s)
  #--------------------------------------------------------------------------------
  m <- log(des_mean^2 / sqrt(des_sd^2 + des_mean^2))
  s <- sqrt(log(1 + (des_sd^2 / des_mean^2)))
  #--------------------------------------------------------------------------------
  #Generate a vector with n points from the desired log-normal distribution
  #--------------------------------------------------------------------------------
  result <- rlnorm(n = n, m, s)
  return(result)
}

#=============================================
#simulate_analytical_probes
#=============================================
simulate_analytical <- function(pr, n, mu, sigma){  
  #' Returns simulated data as a data frame. n subjects as rows; pr columns as probes.
  #' @param pr The number of probes you want to generate per subject.
  #' @param n The number of subjects, with pr probes, for whom you want to simulate signal.
  #' @param mu The 
  #----------------------------------------------
  #Simulate data for each probe for each subject
  #----------------------------------------------
  dat <- NULL   #Dataframe to store values
  for (j in 1:n){
    #--------------------------------------------------------------------------------
    #Simulate yi. Where yi^2 is the amount of signal, above background, for each individual
    #Simulated from a lognormal distribution with desired average mu and sd of sigma.
    #--------------------------------------------------------------------------------
    yi <- gen_from_lognormal(1, mu, sigma)
    #--------------------------------------------------------------------------------
    #Non-central chi-squared distribution. 
    #--------------------------------------------------------------------------------
    #Generate points from a normal distribution centered at mu, sd of one, and square those points.
    signal <- (rnorm(pr, mean=yi, sd=1))^2
    #---------------------------------------------------------------
    #Append the pr points for this subject to the results data-frame
    #---------------------------------------------------------------
    dat <- rbind(dat,signal)
  }
  return(dat)    
}

#=============================================
#bootstrap_sub_avr
#=============================================
bootstrap_sub_avr <- function(pr, n, exp_signal){
  #Samples experimental signal and returns pr data points averaged over n subjects
  s_subj = rep(0, pr)
  for (i in c(1:n)){
    s_subj = s_subj + sample(exp_signal, pr, replace = TRUE)/n
  }
  return(s_subj)
}  
#=============================================
#bootstrap_n_pr
#=============================================
bootstrap_n_pr <- function(exp_signal, n, pr){
  #' Simulates signal for n subjects and pr probes by bootstrapping from experimental signal
  #' @param exp_signal A numeric vector. Experimentally produced signal points.
  #' @param n The number of subjects for which you want to simulate data
  #' @param pr The number of probes per subject you want to simulate.
  #' Returns a data-frame where rows are subjects and columns are probes
  #----------------------------------------------
  #Simulate data for each probe for each subject
  #----------------------------------------------
  dat <- NULL
  for (j in c(1:n)){
    signal = sample(exp_signal, pr, replace = TRUE)
    dat <- rbind(dat,signal)
  }
  return(dat)
}
