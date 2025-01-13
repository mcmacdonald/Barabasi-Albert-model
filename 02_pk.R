#------------------------------------------------------------------

# file 02: fit power law distributions by the Barabasi-Albert model

#------------------------------------------------------------------

# function to estimate degree scaling exponents in networks ---------------------------------------------
pk = function(graph, cdf) {
  
  # notes
  # graph = igraph object
  # cdf = TRUE calculates the cumulative distribution function CDF)
  # cdf = FALSE calculates the probability distribution function (PDF) 
  
  # required packages
  require("igraph")
  
  # calculate degree
  d = igraph::degree(
    graph = graph, 
    v = igraph::V(graph), 
    mode = "all", 
    loops = FALSE, 
    normalized = FALSE
    )
  # calculate degree distribution
  dd = igraph::degree.distribution(
    graph = graph, 
    mode = "all", 
    cumulative = cdf
    )
  
  # probability distribution for degree centrality
  p = dd[-1]
  p0 = which(p != 0) # remove degree = 0 ... i.e., p > 0
  pk  = p[p0] # subset
 
  # degree distribution
  degree = 1:max(d) # centralities min - max range
  degree = degree[p0]  # overwrite degree distribution when degree centrality > 0
  
  # scaling coefficients
  m = lm(log(pk) ~ log(degree))
  b = coef(m)
  alpha = b[[2]]
  message("Scaling exponent and 95% confidence intervals"); cat("\n")
  cat("alpha = "); cat(round(alpha, digits = 2)); cat("\n"); cat("\n")
  
  # calculate the standard error
  # don't run:
  # print(confint.lm(m)) # 95% CIs
  # print(paste("N =", nobs(m) 
  #          ) 
  #    )
  
  # I use Gabaix & Ibragimov's method to calculate standard errors:
  # https://scholar.google.ca/citations?view_op=view_citation&hl=en&user=aCSds20AAAAJ&citation_for_view=aCSds20AAAAJ:UebtZRa9Y70C
  n <- igraph::ecount(graph)    # dyads
  q <- abs(alpha) * sqrt(2/n)   # error
  
  # 95% confidence intervals
  hi <- alpha + (q * 1.96); hi <- round(hi, digits = 2)
  lo <- alpha - (q * 1.96); lo <- round(lo, digits = 2)
  cat("95% CIs = "); cat("["); cat(hi); cat(", "); cat(lo); cat("]")
  cat("\n"); cat("\n")
  
  # R-squared
  R2 = summary(m)$r.squared
  cat("R-squared = "); cat(round(R2, digits = 2)); cat("\n"); cat("\n")

  # message on computation
  if(cdf == TRUE){
    message("Note: the cumulative distribution function is used to calculate the scaling exponent.")
  } else {
    message("Note: the probability distribution function is used to calculate the scaling exponent.")
  }
  
  # plot the slope
  line  = function(x) exp(b[[1]] + b[[2]] * log(x))
  k1 <- 1    # min degree
  kn <- max(d) # max degree
  options(scipen = 999) # turn off scientific notation in Y-axis
  plot(p ~ degree, 
       log = "xy",
       xlim = c(k1, kn), # x-axis scale 
       ylim = c(0.0001, 1), # y-axis scale
       xlab = "degree k (log)", 
       ylab = "probability k (log)", 
       main = "Scaling in criminal networks",
       pch = 1, 
       cex = 2)
  curve(line, 
        col = "firebrick1", 
        lwd = 2, 
        add = TRUE, 
        n = length(d)
        )
}

# plot results for the PDF -------------------------
pk(graph = r_siren, cdf = FALSE)
pk(graph = r_togo, cdf = FALSE)
pk(graph = d_caviar, cdf = FALSE)
pk(graph = d_cocaine, cdf = FALSE)
pk(graph = d_heroin, cdf = FALSE)
pk(graph = d_cielnet, cdf = FALSE)
pk(graph = g_ity, cdf = FALSE)
pk(graph = g_ldn, cdf = FALSE)
pk(graph = g_mtl, cdf = FALSE)
pk(graph = m_infinito, cdf = FALSE)


# plot results for the CDF -------------------------
pk(graph = r_siren, cdf = TRUE)
pk(graph = r_togo, cdf = TRUE)
pk(graph = d_caviar, cdf = TRUE)
pk(graph = d_cocaine, cdf = TRUE)
pk(graph = d_heroin, cdf = TRUE)
pk(graph = d_cielnet, cdf = TRUE)
pk(graph = g_ity, cdf = TRUE)
pk(graph = g_ldn, cdf = TRUE)
pk(graph = g_mtl, cdf = TRUE)
pk(graph = m_infinito, cdf = TRUE)

# ... close .R script
