# Barabasi-Albert-model

An .r script to calculate the slope in scale-free social networks. 

For adjacency graphs, Ravasz & Barabási (2003) propose to measure the level of hierarchy in social networks by the equation:

p(k) ~ k<sup>Y</sup>

where p(*k*) is the probability distribution function (PDF) of degree *k*, *k* represents the elements throughout the degree distribution, and the scaling exponent Y is the slope that indicates if the degree distribution is scale-free. 

The pk() command calculates the gradient of the slope and standard errors that provide the probable range of the slope. The script uses ordinary least squares to regress the logged PDF of the degree distribution on the logged degree *k*. The log-log regression implies that the gradient of the slope scales by k<sup>Y</sup> orders of magnitude. A negative slope >= -2 indicates that the network is centralized and scale-free i.e., there is greater degree disassortativity (heterophily), such that majority of small degree nodes attach themselves to the minority of large degree nodes in the upper-tail of the degree distribution. The steeper the slope, the more centralized the network is. A slope that is < 2, by comparison, indicates that the network is more decentralized and not scale-free i.e., there is greater degree assortivitiy (homophily). As the slope -> 0, the more decentralized the network is.

I use the procedures in Gabaix & Ibragimov (2011) to calculate the standard error of the slope [Clauset et al. (2009) explain why ordinary least squares does not accurately calculate the standard error of the power-law slope].

I illustrate the procedures on different drug trafficking networks:

  - Morselli's  (2009) "caviar" drug trafficking organization 
  - Morselli's  (2009) "cielnet" drug trafficking organization
  - Natarajan's (2000) cocaine trafficking
  - Nararajan's (2006) heroing trafficking

The original data is published by The Mitchell Centre for Social Network Analysis, University of Manchester [https://sites.google.com/site/ucinetsoftware/datasets/covert-networks].

This script imports the numerous data sources and symmetrizes them prior to the calculation of slopes and standard error.



CITATIONS:

Barabási, A. L., & Albert, R. (1999). Emergence of scaling in random networks. Science, 286(5439), 509-512.

Barabási, A. L. (2009). Scale-free networks: a decade and beyond. Science, 325(5939), 412-413.

Clauset, A., Shalizi, C. R., & Newman, M. E. 2009. Power-law distributions in empirical data. SIAM review, 51(4), 661-703.

Gabaix, X., & Ibragimov, R. 2011. Rank− 1/2: a simple way to improve the OLS estimation of tail exponents. Journal of Business & Economic Statistics, 29(1), 24-39.

Morselli, C., 2009. Inside criminal networks. New York, NY: Springer.

Natarajan, M. 2000. Understanding the structure of a drug trafficking organization: A conversational analysis. Crime Prevention Studies, 11, 273-298.

Natarajan, M. 2006. Understanding the Structure of a Large Heroin Distribution Network: A Quantitative Analysis of Qualitative Data. Quantitative Journal of Criminology, 22(2), 171-192.
