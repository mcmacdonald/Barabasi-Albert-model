#-------------------------------------------------------------------------------------------------------------------------------------------------

# File 03: goodness-of-fit for power-law models

# file contains functions to test the goodness-of-fit (GOF) of power law distribution in comparison to Poisson distribution:
#    1. function to calculate the degree distribution for the criminal networks
#    2. function to calculate Erdos-Renyi random networks that have the same size and dimensions of the criminal networks
#    3. function to calculate Vuoing's likelihood-ratio test that compares GOF power law distribution of the criminal networks and random networks
#    4. function to test whether the criminal networks meet requirements of power law distribution
# ... see criticisms of this model: https://www.barabasilab.com/post/love-is-all-you-need

#-------------------------------------------------------------------------------------------------------------------------------------------------



# 1) generate the degree distributions of the criminal networks
d = function(graph){

  # required packages
  required("igraph")
  
  # A degree distribution of the random graph
  d = igraph::degree(
    graph = graph, 
    mode = "total", 
    loops = FALSE, 
    normalized = FALSE
    )
  d <- d[ d != 0 ] # drop if degree k = 0
  return(d) # return the degree distribution
}
d_siren    = d(graph = siren)
d_togo     = d(graph = togo)
d_caviar   = d(graph = caviar)
d_cielnet  = d(graph = cielnet)
d_cocaine  = d(graph = cocaine)
d_heroin   = d(graph = heroin)
d_italy    = d(graph = italian)
d_london   = d(graph = london)
d_montreal = d(graph = montreal)
d_infinito = d(graph = infinito)



# 2) generate Endos-Renyi (ER) random graphs
r = function(graph){

  # required packages
  required("igraph")
  
  # set seed for replication purposes
  set.seed(20190812)
  
  # dimensions of the graph
  n <- length(igraph::V(graph)) # 'n' vertices
  m <- length(igraph::E(graph)) # 'm' edges
  
  # calculate 10,000 random networks
  samples <- 10000
  r = c() # create a results vector
  for(i in 1 : samples) {
    random = igraph::erdos.renyi.game(
      n = n, 
      p.or.m = m, # m edges taken from the uniform random distribution from the set of all possible edges 
      type = "gnm", 
      directed = FALSE, 
      loops = FALSE
      )
    
    # degree distribution of the random graphs
    d = igraph::degree(
      graph = random, 
      mode = "total", 
      loops = FALSE, 
      normalized = FALSE
      )
    d <- d[d != 0] # drop if degree k = 0
    
    # put degree distribution into results vector
    r = c(r, d)  
  }
  
  # average frequencies from the sampling distribution
  r = data.frame(r)
  r$n <-1
  r = aggregate(
    r$n, 
    by = list(r$r), 
    FUN = sum
    )
  r$x <- r$x/samples # averaging across the 10,000 samples
  r$x <- round(x = r$x, digits = 0)
  r <- rep(r$Group.1, r$x) # transform frequency table into a vector
  
  # return the results vector
  return(r)
}
r_siren    = d(graph = siren)
r_siren    = r(graph = siren)
r_togo     = r(graph = togo)
r_caviar   = r(graph = caviar)
r_cielnet  = r(graph = cielnet)
r_cocaine  = r(graph = cocaine)
r_heroin   = r(graph = heroin)
r_italian  = r(graph = ity)
r_london   = r(graph = ldn)
r_montreal = r(graph = mtl)
r_infinito = r(graph = infinito)



# 3) compare Vuong's likelihood ratio test for GOF of power law distribution
vuong = function(d){ # where d = the degree distribution for the criminal networks from the prior function d()

  # required packages
  required("poweRlaw")
  
  # fit power law distribution
  alpha = poweRlaw::displ(d)
  alpha$setXmin(1) # for all degree k >= 1
  alpha$setPars(poweRlaw::estimate_pars(pl_fit))
  
  # fit Poisson distribution
  lambda = poweRlaw::dispois(d)
  lambda$setXmin(1) # for all degree k => 1
  lambda$setPars(poweRlaw::estimate_pars(lambda))
  
  # Vuong's likelihood-ratio test to compare models
  lrtest = poweRlaw::compare_distributions(alpha, lambda)
  
  # Vuong's LR test statistic 
  lrstat = lrtest$test_statistic
  cat("Vuong's LR test statistic = "); cat(lrstat ); cat("\n"); cat("\n")
  
  # notes on the interpretation of Vuong's likelihood-ratio test statistic: 
  # the sign of the test statistic (i.e., +/-) has meaning for interpretation (Vuong's formula is a sign test)
  # because 'alpha' is the first input into the poweRlaw::compare_distributions() function and 'lambda' is the second:
  # ... a positive (+) test statistic suggests the degree distribution more so resembles the power law distribution
  # ... a negative (-) test statistic suggests the degree distribution more so resembles the Poisson distribution
  # ... reversing the input order in poweRlaw::compare_distributions() (i.e., 'lambda' before 'alpha') computes the same test statistic, but in the opposite direction
  
  # p-value (two-tailed)
  p = lrtest$p_two_sided
  # if p < 0.05, reject H0: the degree distribution neither resembles the power law distribution or Poisson distribution
  # if p > 0.05, fail to reject H1: degree distribution resemebles power law distribution or Poisson distribution (see notes on Vuong's likelihood-ratio test)
  cat( "p-value = "); cat(p); cat("\n"); cat("\n")
  
  # interpretation:
  message("Interpretation:"
  if(lrstat > 0){
    message("The postive likelihood-ratio test statistic suggests the degree distribution more closely resembles the power law distribution.")
    message("A larger test statistic suggests better goodness-of-fit.")
    message("If p < 0.05, then reject the null hypothesis that the degree distribution does not resemble the power law distribution.")
  } else {
    message("The negative likelihood-ratio test statistic suggests the degree distribution more closely resembles the Poisson distribution.")
    message("A larger test statistic suggests better goodness-of-fit.")
    message("If p < 0.05, then reject the null hypothesis that the degree distribution does not resemble the Poisson distribution.")
  }
}
# test results for the criminal networks
vuong(d = d_siren)
vuong(d = d_togo)
vuong(d = d_caviar)
vuong(d = d_cielnet)
vuong(d = d_cocaine)
vuong(d = d_heroin)
vuong(d = d_italian)
vuong(d = d_london)
vuong(d = d_montreal)
vuong(d = d_infinito)

# test results for the random networks
vuong(d = r_siren)
vuong(d = r_togo)
vuong(d = r_caviar)
vuong(d = r_cielnet)
vuong(d = r_cocaine)
vuong(d = r_heroin)
vuong(d = r_italian)
vuong(d = r_london)
vuong(d = r_montreal)
vuong(d = r_infinito)



# 4) function testing if data meets requirements for power law distribution ------------------------------
# Broido & Clauset (2019) argue power law scaling in networks is the exception, not the norm and only occunders under certain conditions
# Clauset's test: https://scholar.google.ca/scholar?hl=en&as_sdt=0%2C5&q=Goldstein+2004+power+law&btnG=     
# see Broido & Clauset's paper: https://www.nature.com/articles/s41467-019-08746-5
# see Barabasi's response to Broido & Clauset's paper: https://www.barabasilab.com/post/love-is-all-you-need
# see Holme's response to this debate: https://www.nature.com/articles/s41467-019-09038-8

pk_clauset = function(d, k){

  # required packages
  require("igraph")
  
  # power law model for a given k cut point i.e., degree >= k 
  alpha = igraph::fit_power_law(
    x = d,     # the degree distribution
    xmin = k,  # for k or xmin, the algorithm tests only values >= k for power law scaling
    implementation = 'plfit'
    )
  # print test results
  cat("scaling exponent for all degree >= "); cat("k"); cat(" = "); cat(alpha$alpha); cat("\n")
  cat("log likelihood statistic = "); cat(alpha$logLik ); cat("\n")
  cat("Kolmogorov-Smirnov test statistic = "); cat(alpa$KS.stat); cat("\n")
  cat("p-value for Kolmogorov-Smirnov test statistic = "); cat(alpha$KS.p); cat("\n")
  
  # interpretation
  message("goodness-of-fit (GOF) interpretation:")
  message("Kolmogorov-Smirnov (KS) test compares the degree distribution k >= 1 against degree distribution >= the cupt point k, where smaller KS test statistics suggest better GOF.")
  message("If Kolmogorov-Smirnov test has p-value < 0.05, reject the null hypothesis that the degree distribution for all degree >= k resembles the power law distribution.")
}
pk_clauset( # i.e., for all k > 0, where k = degree
  d = d_siren, 
  k = 1
  )
pk_clauset( # i.e., for all k >= 2
  d = d_siren,
  k = 2
  )
pk_clauset( # i.e., for all k >= 5
  d = d_siren, 
  k = 5
  )
pk_clauset( # ... when 'k' for xmin isn't specified, igraph::fit_power_law() chooses the optimum value for the cut point k
  d = d_siren, 
  k = NULL
  )



# ... close .R script
