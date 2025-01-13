#------------------------------------------------------------------

# file 02: fit power law distributions by the Barabasi-Albert model

#------------------------------------------------------------------

# function to estimate degree scaling exponents in networks ---------------------------------------------
pk = function(graph) {
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
    cumulative = FALSE
    )
  
  # centralities min - max range
  degree = 1:max(d)
  
  # calculate probability distribution for degree centrality
  p = dd[-1]
  pk1 = which(p != 0) # remove degree = 0 ... i.e., p > 0
  p = p[pk1] # subset
  
  # overwrite degree distribution when degree centrality > 0
  degree = degree[pk1]
  
  # scaling coefficients
  m = lm(log(p) ~ log(degree))
  b = coef(m)
  alpha = b[[2]]
  message("scaling exponent and 95% confidence intervals"); cat("\n")
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
  cat("95% CI = "); cat("["); cat(hi); cat(", "); cat(lo); cat("]")
  cat("\n"); cat("\n")
  
  # R-squared
  R2 = summary(m)$r.squared
  cat("R-squared = "); cat(round(R2, digits = 2))
  
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

# plot results -------------------------
pk(graph = r_siren)
pk(graph = r_togo)
pk(graph = d_caviar)
pk(graph = d_cocaine)
pk(graph = d_heroin)
pk(graph = d_cielnet)
pk(graph = g_ity)
pk(graph = g_ldn)
pk(graph = g_mtl)
pk(graph = m_infinito)

# ... close .R script
