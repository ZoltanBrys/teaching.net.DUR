# load R-pack
if (!require(ergm)) {install.packages("ergm")}
library(ergm)
## ------------------------------------------------------------------------

#let's take a look at our dataset
data(florentine) # loads flomarriage and flobusiness data

flomarriage # Look at the flomarriage network properties (uses `network`), esp. the vertex attributes

plot(flomarriage, 
     main="Florentine Marriage", 
     cex.main=0.8, 
     label = network.vertex.names(flomarriage)) # Plot the network

wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
plot(flomarriage, 
     label = network.vertex.names(flomarriage) ,
     vertex.cex=wealth/25, 
     main="Florentine marriage by wealth", cex.main=0.8) 

## ------------------------------------------------------------------------
summary(flomarriage ~ edges) # Look at the $g(y)$ statistic for this model
flomodel.01 <- ergm(flomarriage ~ edges) # Estimate the model 
summary(flomodel.01) # Look at the fitted model object


## ------------------------------------------------------------------------
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

## ------------------------------------------------------------------------
summary(wealth) # summarize the distribution of wealth
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model

flomodel.03 <- ergm(flomarriage ~ edges + nodecov('wealth'),
                    control = control.ergm(force.main = TRUE, MCMLE.maxit = 50))

summary(flomodel.03)

#do MCMC
mcmc.diagnostics(flomodel.03)

#GOF
gof.03 <- gof(flomodel.03)
plot(gof.03)

#NOTE:
# for effects size   
# advanced code : https://github.com/DISSINET/bologna.incr.social

#plot GOF

